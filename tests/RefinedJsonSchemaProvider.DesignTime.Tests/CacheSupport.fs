namespace RefinedJsonSchemaProvider.Tests

// Shared by CacheEquivalenceTests and the cache benchmark (benchmarks/CacheBenchmark links this file):
// running the provider with or without the conversion cache, forcing the lazily built invoke code of
// every provided member, and random schemas to run it on.
module CacheSupport =
    open System
    open System.Reflection
    open System.Text
    open FSharp.Quotations
    open ProviderImplementation.ProvidedTypes
    open RefinedJsonSchemaProvider
    open RefinedJsonSchemaProvider.DesignTime
    open RefinedJsonSchemaProvider.DesignTime.ProviderConfiguration

    let flagsWithCache (useCache: bool) : CompileFlags =
        { SkipRuntimeValidation = false
          RefinedTypes = true
          RequireIntegerBounds = false
          UseConversionCache = useCache }

    let provide (flags: CompileFlags) (schemaText: string) : ProvidedTypeDefinition =
        let schema = SchemaCache.parseSchema schemaText
        TypeProvider.run schema (schemaText.GetHashCode()) (Assembly.GetExecutingAssembly()) "Test" "Root" typeof<NullableJsonValue> flags

    let private declared =
        BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Static ||| BindingFlags.Instance ||| BindingFlags.DeclaredOnly

    // Placeholder arguments for a member's invoke code; the code is only built, never evaluated.
    // The compiler hands invoke code its arguments at their erased types, so a provided type is
    // replaced by the first base type that is not provided (NullableJsonValue for objects).
    let rec private erased (t: Type) : Type =
        match t with
        | :? ProvidedTypeDefinition -> erased t.BaseType
        | _ when t.IsGenericType && not t.IsGenericTypeDefinition ->
            let arguments = t.GetGenericArguments()
            let erasedArguments = Array.map erased arguments
            if Array.forall2 (fun a b -> obj.ReferenceEquals(a, b)) arguments erasedArguments then t
            else t.GetGenericTypeDefinition().MakeGenericType erasedArguments
        | _ -> t

    let private argsFor (m: MethodBase) : Expr list =
        let parameters =
            m.GetParameters() |> Array.toList |> List.map (fun p -> Expr.Var(Var(p.Name, erased p.ParameterType)))
        if m.IsStatic then parameters else Expr.Var(Var("this", erased m.DeclaringType)) :: parameters

    // GetInvokeCode is internal in the TypeProviders SDK (the compiler reaches it through
    // ITypeProvider.GetInvokerExpression), so it is called through reflection here.
    // On a constructor it is a method taking the argument list. On a method it is a property holding
    // an option of that function (None for a method without code).
    let private anyInstance = BindingFlags.Instance ||| BindingFlags.NonPublic ||| BindingFlags.Public

    let private constructorInvoke =
        lazy typeof<ProvidedConstructor>.GetMethod("GetInvokeCode", anyInstance)

    let private methodInvoke =
        lazy typeof<ProvidedMethod>.GetProperty("GetInvokeCode", anyInstance)

    let private unwrap (f: unit -> obj) : obj =
        try f () with :? TargetInvocationException as ex -> raise ex.InnerException

    let private invokeCodeOf (m: MemberInfo) : Expr option =
        match m with
        | :? ProvidedConstructor as pc ->
            Some(unwrap (fun () -> constructorInvoke.Value.Invoke(pc, [| box (argsFor pc) |])) :?> Expr)
        | :? ProvidedMethod as pm ->
            match methodInvoke.Value.GetValue pm with
            | null -> None
            | some ->
                let code = some.GetType().GetProperty("Value").GetValue some :?> (Expr list -> Expr)
                Some(code (argsFor pm))
        | _ -> None

    let private membersOf (t: Type) =
        t.GetMembers declared |> Array.sortBy (fun m -> string m.MemberType, m.Name, string m)

    // The provider adds the provided type of a nested object as a nested type of every object above
    // it, so the same type is reachable along many paths (2^d times in a chain of d objects). Each
    // type is visited once, compared by reference.
    let private newVisited () = Collections.Generic.HashSet<Type>(HashIdentity.Reference)

    // Builds the invoke code of every method, constructor and property getter, nested types included.
    // The compiler does this lazily for the members a program uses; forcing all of them measures the
    // worst case, where every generated member is used.
    let force (root: Type) : unit =
        let visited = newVisited ()
        let rec go (t: Type) =
            if visited.Add t then
                for m in membersOf t do
                    match m with
                    | :? Type as nested -> go nested
                    | :? PropertyInfo as p ->
                        match p.GetGetMethod true with
                        | null -> ()
                        | getter -> invokeCodeOf getter |> ignore
                    | :? MethodInfo as mi when mi.IsSpecialName -> () // property getters, handled above
                    | _ -> invokeCodeOf m |> ignore
        go root

    // Renames every bound variable to v0, v1, ... in traversal order. Generated names carry a fresh
    // Guid (runtimeObj2391ac22-...), and with the cache one lambda can appear several times under
    // the same name, while without it each occurrence gets its own. After renaming, two expressions
    // print the same exactly when they are equal up to renaming of bound variables.
    let canonical (expr: Expr) : Expr =
        let mutable counter = 0
        let rec go (env: Map<Var, Var>) (e: Expr) =
            match e with
            | ExprShape.ShapeVar v ->
                match env.TryFind v with
                | Some renamed -> Expr.Var renamed
                | None -> e
            | ExprShape.ShapeLambda(v, body) ->
                let renamed = Var($"v{counter}", v.Type, v.IsMutable)
                counter <- counter + 1
                Expr.Lambda(renamed, go (env.Add(v, renamed)) body)
            | ExprShape.ShapeCombination(shape, args) ->
                ExprShape.RebuildShapeCombination(shape, List.map (go env) args)
        go Map.empty expr

    // A text rendering of everything the provider generates: nested types, member signatures and the
    // invoke code of each member. Two runs that render the same give the user the same types and code.
    let describe (root: Type) : string =
        let sb = StringBuilder()
        let line (indent: string) (text: string) = sb.Append(indent).AppendLine(text) |> ignore
        let signature (m: MethodBase) =
            m.GetParameters() |> Array.map (fun p -> $"{p.Name}: {p.ParameterType}") |> String.concat ", "
        let code indent (m: MemberInfo) =
            match invokeCodeOf m with
            | Some expr -> line (indent + "    ") (string (canonical expr))
            | None -> ()

        let visited = newVisited ()
        let rec go (indent: string) (t: Type) =
          if not (visited.Add t) then
            line indent $"type {t.Name} (shown above)"
          else
            line indent $"type {t.Name} : {t.BaseType}"
            for m in membersOf t do
                match m with
                | :? Type as nested -> go (indent + "  ") nested
                | :? PropertyInfo as p ->
                    line (indent + "  ") $"property {p.Name}: {p.PropertyType}"
                    match p.GetGetMethod true with
                    | null -> ()
                    | getter -> code indent getter
                | :? MethodInfo as mi when mi.IsSpecialName -> ()
                | :? MethodInfo as mi ->
                    line (indent + "  ") $"method {mi.Name}({signature mi}): {mi.ReturnType}"
                    code indent mi
                | :? ConstructorInfo as c ->
                    line (indent + "  ") $"new({signature c})"
                    code indent c
                | other -> line (indent + "  ") $"{other.MemberType} {other.Name}"

        go "" root
        sb.ToString()

    // A random schema of at most maxDepth levels, mixing every node kind and the keywords that give
    // refined types (integer bounds, array lengths, uniqueItems, oneOf).
    let randomSchema (rnd: Random) (maxDepth: int) : string =
        let pick (options: 'a[]) = options[rnd.Next options.Length]

        let integer () =
            match rnd.Next 4 with
            | 0 -> """{ "type": "integer" }"""
            | 1 -> sprintf """{ "type": "integer", "minimum": %d }""" (rnd.Next(-5, 5))
            | 2 -> sprintf """{ "type": "integer", "minimum": 0, "maximum": %s }""" (pick [| "10"; "300"; "70000"; "5000000000" |])
            | _ -> sprintf """{ "type": "integer", "minimum": %d, "maximum": %d }""" (rnd.Next(-200, 0)) (rnd.Next(1, 200))

        let leaf () =
            match rnd.Next 4 with
            | 0 -> """{ "type": "boolean" }"""
            | 1 -> integer ()
            | 2 -> pick [| """{ "type": "number" }"""; """{ "type": "number", "minimum": 0 }""" |]
            | _ -> pick [| """{ "type": "string" }"""; """{ "type": "string", "minLength": 1 }""" |]

        let rec node depth =
            if depth >= maxDepth then
                leaf ()
            else
                match rnd.Next 7 with
                | 0 | 1 | 2 -> leaf ()
                | 3 | 4 -> object depth
                | 5 -> array depth
                | _ -> oneOf depth

        and object depth =
            let count = rnd.Next 5
            let names = List.init count (fun i -> $"p{i}")
            let properties = names |> List.map (fun n -> $"\"{n}\": {node (depth + 1)}") |> String.concat ", "
            let required = names |> List.filter (fun _ -> rnd.Next 2 = 0) |> List.map (fun n -> $"\"{n}\"") |> String.concat ", "
            $"{{ \"type\": \"object\", \"properties\": {{ {properties} }}, \"required\": [ {required} ] }}"

        and array depth =
            let lengths =
                match rnd.Next 5 with
                | 0 -> ""
                | 1 -> $", \"minItems\": {rnd.Next 4}"
                | 2 -> $", \"maxItems\": {rnd.Next(1, 4)}"
                | 3 -> let n = rnd.Next(1, 4) in $", \"minItems\": {n}, \"maxItems\": {n}"
                | _ -> let m = rnd.Next 3 in $", \"minItems\": {m}, \"maxItems\": {m + rnd.Next(1, 3)}"
            let unique = if rnd.Next 4 = 0 then ", \"uniqueItems\": true" else ""
            $"{{ \"type\": \"array\", \"items\": {node (depth + 1)}{lengths}{unique} }}"

        and oneOf depth =
            let branches = List.init (rnd.Next(1, 4)) (fun _ -> node (depth + 1)) |> String.concat ", "
            $"{{ \"oneOf\": [ {branches} ] }}"

        // The root is an object most of the time, as in real schemas.
        if rnd.Next 4 = 0 then node 0 else object 0
