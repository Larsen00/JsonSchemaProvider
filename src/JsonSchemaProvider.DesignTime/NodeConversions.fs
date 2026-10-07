namespace JsonSchemaProvider.DesignTime

module NodeConversions =
    open System
    open System.Reflection
    open FSharp.Quotations
    open FSharp.Data
    open SchemaConversion
    open ProviderConfiguration
    open ProviderImplementation.ProvidedTypes
    open JsonSchemaProvider
    open System.Collections.Concurrent
    open ArrayShape

    type TypeMap = Map<string, ProvidedTypeDefinition>

    // Every type carries its own Path
    // A root-level node's Path is "#".
    let pathOf (schemaType: JsonSchemaType) : string =
        match schemaType with
        | JsonNumber keywords
        | JsonInteger keywords            -> keywords.common.Path
        | JsonBoolean keywords           -> keywords.common.Path
        | JsonString keywords         -> keywords.common.Path
        | JsonObject (keywords, _)     -> keywords.common.Path
        | JsonArray(_, keywords)       -> keywords.common.Path
        | JsonOneOf (keywords, _, _)  -> keywords.Path

    // Builds `fun jsonVal -> let jsonArr = jsonVal.AsArray() in buildBody jsonArr`
    let private withJsonArrayLambda (buildBody: Var -> Expr) : Expr =

        let body jsonValVar =
            let jsonArrVar = Var($"jsonArr{Guid.NewGuid()}", typeof<JsonValue[]>)
            Expr.Let(
                jsonArrVar,
                CommonExprs.callJsonValueAsArray (Expr.Var jsonValVar),
                buildBody jsonArrVar)

        CommonExprs.freshLambda "jsonVal" typeof<JsonValue> body

    // `true` when the JsonValue array behind `jsonArrVar` has at least one element
    let private hasAtLeastOneElement (jsonArrVar: Var) : Expr =
        <@@ (%%(Expr.Var jsonArrVar): JsonValue[]).Length > 0 @@>

    // Rebuilds the flat n-tuple (n >= 2) from an ExactLength nested-pair value (h1, (h2, ... hn)).
    let private buildExactLengthToTuple (n: int) (inner: ProviderConfiguration.NodeConversion) (runtimeType: Type) : ProviderConfiguration.RuntimeHelperConversion =
        let rec elems (remaining: int) (current: Expr) : Expr list =
            if remaining = 1 then
                [ current ]
            else
                Expr.TupleGet(current, 0) :: elems (remaining - 1) (Expr.TupleGet(current, 1))

        { Convert = CommonExprs.freshLambda "value" runtimeType (fun valueVar -> Expr.NewTuple(elems n (Expr.Var valueVar)))
          CompileTimeReturnType = Microsoft.FSharp.Reflection.FSharpType.MakeTupleType(Array.create n inner.CompileTimeType) }

    // Creates a ToList helper; `body` builds the list from the array value.
    let private buildToList (inner: ProviderConfiguration.NodeConversion) (runtimeType: Type) (body: Var -> Expr) : ProviderConfiguration.RuntimeHelperConversion =
        { Convert = CommonExprs.freshLambda "value" runtimeType body
          CompileTimeReturnType = typedefof<_ list>.MakeGenericType inner.CompileTimeType }


    // Wrapper function to "conversion" that uses a cache to avoid recomputation
    // #region convert-conversion
    let rec convert (context: GenerationContext) (typeMap: TypeMap) (schemaType: JsonSchemaType) : ProviderConfiguration.NodeConversion = 
        
        context.ConversionCache.GetOrAdd( 
            pathOf schemaType,
            fun _ -> conversion context typeMap schemaType
        )
        

    // #elide-start
    // Builds everything there is to know about turning one JsonSchemaType node into F#: its
    // compile-time type, its runtime/erased type, and the two conversion functions between
    // JsonValue and that runtime type
    //
    // Its done like this because otherwise we needed four separate functions that all needed
    // to stay in sync with each other. -- a bonus is that we also get better performance with less overhead
    // #elide-end
    and conversion (context: GenerationContext) (typeMap: TypeMap) (schemaType: JsonSchemaType) : ProviderConfiguration.NodeConversion =
        
        match schemaType with
        | JsonBoolean keywords -> { 
                CompileTimeType = typeof<bool>
                RuntimeType = typeof<bool>
                ToRuntime = <@@ fun (jsonVal: JsonValue) -> jsonVal.AsBoolean() @@>
                ToJson = <@@ fun (runtimeObj: bool) -> JsonValue.Boolean(runtimeObj) @@>
                // #elide-start
                FullyCompilable = keywords.common.CanBeCompiled
                // #elide-end
            }

        // #elide-start
        | JsonInteger keywords -> {
                CompileTimeType = typeof<int>
                RuntimeType = typeof<int>
                ToRuntime = <@@ fun (jsonVal: JsonValue) -> jsonVal.AsInteger() @@>
                ToJson = <@@ fun (runtimeObj: int) -> JsonValue.Number(decimal runtimeObj) @@>
                FullyCompilable = keywords.common.CanBeCompiled
            }

        | JsonNumber keywords -> {
                CompileTimeType = typeof<double>
                RuntimeType = typeof<double>
                ToRuntime = <@@ fun (jsonVal: JsonValue) -> jsonVal.AsFloat() @@>
                ToJson = <@@ fun (runtimeObj: double) -> JsonValue.Float runtimeObj @@>
                FullyCompilable = keywords.common.CanBeCompiled
            }

        | JsonString keywords -> {
                CompileTimeType = typeof<string>
                RuntimeType = typeof<string>
                ToRuntime = <@@ fun (jsonVal: JsonValue) -> jsonVal.AsString() @@>
                ToJson = <@@ fun (runtimeObj: string) -> JsonValue.String runtimeObj @@>
                FullyCompilable = keywords.common.CanBeCompiled
            }

        | JsonObject(keywords, properties) ->
            {
                CompileTimeType = typeMap[keywords.common.Path]
                RuntimeType = typeof<NullableJsonValue>
                ToRuntime = <@@ fun (jsonVal: JsonValue) -> NullableJsonValue jsonVal @@>
                ToJson = <@@ fun (runtimeObj: NullableJsonValue) -> runtimeObj.JsonVal @@>
                FullyCompilable = isClassFullyCompilable context typeMap keywords properties
            }

        // Sicne array have compile time type support we delegate the conversion to `buildArrayConversion` function.
        | JsonArray(innerType, arrayKeywords) -> (buildArrayConversion context typeMap innerType arrayKeywords).common

        // OneOf types are handled by the `buildOneOfConversion` function. keywords.CanBeCompiled
        // is about the oneOf node itself (e.g. a stray keyword sitting alongside "oneOf"), which
        // buildOneOfConversion has no access to - folded in here instead.
        | JsonOneOf (keywords, head, tail) ->
            let conv = buildOneOfConversion context typeMap (head :: tail)
            { conv with FullyCompilable = keywords.CanBeCompiled && conv.FullyCompilable }
        // #elide-end
        // #endregion
        

    

    // Performs the recursive conversion for array types based on their inner type and array keywords.
    //
    // Note: this assumes minItems and maxItems can never be 0 - a 0 value is converted to None in
    // SchemaConversion.fs.
    and buildArrayConversion
        (context: GenerationContext)
        (typeMap: TypeMap)
        (innerType: JsonSchemaType)
        (arrayKeywords: JsonArray.Keywords)
        : ProviderConfiguration.ArrayConversion =

        // Default conversion for F# list arrays based on their inner type.
        let defaultArrayConversion: ProviderConfiguration.ArrayConversion =
            // Extract the inner type conversion
            let inner = convert context typeMap innerType

            // The compile-time and runtime types for the list based on the inner type conversion
            let compileTimeType = typedefof<_ list>.MakeGenericType inner.CompileTimeType
            let runtimeType     = typedefof<_ list>.MakeGenericType inner.RuntimeType

            let toRuntime =
                CommonExprs.freshLambda "jsonVal" typeof<JsonValue> (
                    fun jsonValVar ->

                        // Calls asArray on the JSON value to get it as an array
                        let asArray = CommonExprs.callJsonValueAsArray (Expr.Var jsonValVar)

                        // Maps each element of the JSON array to its runtime representation using the inner type conversion
                        let mapped = CommonExprs.callArrayMap inner.ToRuntime asArray typeof<JsonValue> inner.RuntimeType

                        // Turns the array in to a list
                        CommonExprs.callListOfArray mapped inner.RuntimeType
                    )

            let toJson =
                CommonExprs.freshLambda "runtimeObj" runtimeType (
                    fun runtimeObjVar ->

                        // map each element of the list back to a JSON value
                        let mappedBack = CommonExprs.callListMap inner.ToJson (Expr.Var runtimeObjVar) inner.RuntimeType typeof<JsonValue>

                        // convert the list of JSON values back into a JSON array
                        let arrayOfList = CommonExprs.callArrayOfList mappedBack typeof<JsonValue>

                        // create a new JSON array from the array of JSON values (Reverse of asArray)
                        CommonExprs.newJsonValueArray arrayOfList
                    )

            // Already a list, so ToList returns it unchanged.
            let toList = buildToList inner runtimeType (fun valueVar -> Expr.Var valueVar)

            // MinItems.IsNone matters here specifically: reaching this default case with MinItems
            // still Some means compileFlags.CompileMinItems was false, so minItems is a real,
            // uncompiled constraint - MaxItems needs no equivalent check, since any Some MaxItems
            // is always caught by one of the tuple/option cases above, unconditionally.
            { common =
                { CompileTimeType = compileTimeType
                  RuntimeType = runtimeType
                  ToRuntime = toRuntime
                  ToJson = toJson
                  FullyCompilable = arrayKeywords.common.CanBeCompiled && arrayKeywords.specific.MinItems.IsNone && inner.FullyCompilable }
              specific = { ToList = toList; ToTuple = None } }



        match classifyArrayShape context.CompileFlags innerType arrayKeywords with

        // Invalid case: minItems greater than maxItems
        | InvalidBounds _ ->
            failwith "MinItems cannot be greater than MaxItems - Please check your schema."

        // Keyword combinations that does not have a compiled type -> so we gonna fallback to runtime validation
        // If IgnoreSpecificKeywords is set, we need to fallback to runtime validation regardless of other keyword combinations.
        | UnsupportedKeywords _ ->
            { defaultArrayConversion with common.FullyCompilable = false }

        // Exact-size, exactly one element left: same base case as MaxItemsSingle - just the element itself
        | ExactLength(_, _, 1) ->
            let inner = convert context typeMap innerType

            let toRuntime =
                withJsonArrayLambda (fun jsonArrVar ->
                    Expr.Application(inner.ToRuntime, CommonExprs.callArrayGet 0 (Expr.Var jsonArrVar) typeof<JsonValue>))

            let toJson =
                CommonExprs.freshLambda "runtimeObj" inner.RuntimeType (fun runtimeObjVar ->
                    CommonExprs.newJsonValueArray (
                        Expr.NewArray(typeof<JsonValue>, [ Expr.Application(inner.ToJson, Expr.Var runtimeObjVar) ])
                    ))

            let toList =
                buildToList inner inner.RuntimeType (fun valueVar ->
                    CommonExprs.newListSingleton inner.RuntimeType (Expr.Var valueVar))

            { common =
                { CompileTimeType = inner.CompileTimeType
                  RuntimeType = inner.RuntimeType
                  ToRuntime = toRuntime
                  ToJson = toJson
                  FullyCompilable = arrayKeywords.common.CanBeCompiled && inner.FullyCompilable }
              specific = { ToList = toList; ToTuple = None } }

        // Exact-size tuple, more than one element left: (Head, Tail) where Tail recurses on the
        // same exact-length shape with n - 1
        | ExactLength(_, _, n) ->
            let inner = convert context typeMap innerType
            let tailKeywords =
                { arrayKeywords with
                    specific.MinItems = Some(n - 1)
                    specific.MaxItems = Some(n - 1) }
            let headTail = buildHeadTailConversion context typeMap innerType arrayKeywords inner tailKeywords
            { headTail with specific.ToTuple = Some(buildExactLengthToTuple n inner headTail.common.RuntimeType) }

        // minItems mandatory prefix: (Head, Tail) where Tail recurses with minItems decremented
        // by one (dropping to None once exhausted) and maxItems decremented by one
        | MinItemsPrefix(_, _, minItems, maxItems) ->
            let inner = convert context typeMap innerType
            let tailKeywords =
                { arrayKeywords with
                    specific.MinItems = if minItems > 1 then Some(minItems - 1) else None
                    specific.MaxItems = maxItems |> Option.map (fun m -> m - 1) }
            buildHeadTailConversion context typeMap innerType arrayKeywords inner tailKeywords

        // maxItems = 1: option<inner>.
        | MaxItemsSingle _ ->
            // Extract the inner type
            let inner = convert context typeMap innerType

            // Define the compile-time and runtime types for the option<inner> conversion.
            let compileTimeType = typedefof<option<_>>.MakeGenericType [| inner.CompileTimeType |]
            let runtimeType = typedefof<option<_>>.MakeGenericType [| inner.RuntimeType |]

            let toRuntime =
                withJsonArrayLambda (
                    fun jsonArrVar ->
                        
                        // Get the first element of the JSON array.
                        let element = CommonExprs.callArrayGet 0 (Expr.Var jsonArrVar) typeof<JsonValue>

                        let some =
                            CommonExprs.newOptionSome
                                inner.RuntimeType
                                (Expr.Application(inner.ToRuntime, element))
                        
                        // Return Some(inner) if the array has at least one element, otherwise return None.
                        Expr.IfThenElse(hasAtLeastOneElement jsonArrVar, some, CommonExprs.newOptionNone inner.RuntimeType)
                    )

            let toJson =
                CommonExprs.freshLambda "runtimeObj" runtimeType (
                    fun runtimeObjVar ->
                        let thenBranch =
                            CommonExprs.newJsonValueArray (
                                Expr.NewArray(
                                    typeof<JsonValue>,
                                    [ Expr.Application(inner.ToJson, CommonExprs.getOptionValue inner.RuntimeType (Expr.Var runtimeObjVar)) ]
                                )
                            )
                        Expr.IfThenElse(CommonExprs.getOptionIsSome inner.RuntimeType (Expr.Var runtimeObjVar), thenBranch, CommonExprs.emptyJsonValueArray)
                    )

            // Some x -> [x] | None -> []
            let toList =
                buildToList inner runtimeType (fun valueVar ->
                    let thenBranch = CommonExprs.newListSingleton inner.RuntimeType (CommonExprs.getOptionValue inner.RuntimeType (Expr.Var valueVar))
                    Expr.IfThenElse(CommonExprs.getOptionIsSome inner.RuntimeType (Expr.Var valueVar), thenBranch, CommonExprs.newListEmpty inner.RuntimeType))

            { common =
                { CompileTimeType = compileTimeType
                  RuntimeType = runtimeType
                  ToRuntime = toRuntime
                  ToJson = toJson
                  FullyCompilable = arrayKeywords.common.CanBeCompiled && inner.FullyCompilable }
              specific = { ToList = toList; ToTuple = None } }

        // maxItems > 1: option<(inner * tail)>, tail built by recursing on this same function.
        | MaxItemsChain(_, _, maxItems) ->

            let inner = convert context typeMap innerType

            // Build the tail conversion by recursively calling this function with MaxItems decreased by 1.
            let tailKeywords = { arrayKeywords with specific.MaxItems = Some(maxItems - 1) }
            let tailArray = buildArrayConversion context typeMap innerType tailKeywords
            let tail = tailArray.common

            // Build the compile-time and runtime types for the option containing the pair of head and tail.
            let compileTimePairType = Microsoft.FSharp.Reflection.FSharpType.MakeTupleType [| inner.CompileTimeType; tail.CompileTimeType |]
            let compileTimeType = typedefof<option<_>>.MakeGenericType [| compileTimePairType |]
            let pairType = Microsoft.FSharp.Reflection.FSharpType.MakeTupleType [| inner.RuntimeType; tail.RuntimeType |]
            let runtimeType = typedefof<option<_>>.MakeGenericType [| pairType |]

            let toRuntime =
                withJsonArrayLambda (fun jsonArrVar ->
                    // Convert the head element of the array to its runtime representation.
                    let headRuntime = Expr.Application(inner.ToRuntime, CommonExprs.callArrayGet 0 (Expr.Var jsonArrVar) typeof<JsonValue>)
                    
                    // Convert the tail of the array to its runtime representation.
                    let tailJsonValue =
                        CommonExprs.newJsonValueArray (CommonExprs.callArraySkip 1 (Expr.Var jsonArrVar) typeof<JsonValue>)
                    
                    let tailRuntime = Expr.Application(tail.ToRuntime, tailJsonValue)
                    
                    let some = CommonExprs.newOptionSome pairType (Expr.NewTuple [ headRuntime; tailRuntime ])
                    
                    // If the array has at least one element, wrap the converted pair in Some; otherwise, return None.
                    Expr.IfThenElse(hasAtLeastOneElement jsonArrVar, some, CommonExprs.newOptionNone pairType)
                )

            let toJson =
                CommonExprs.freshLambda "runtimeObj" runtimeType (fun runtimeObjVar ->
                    let pairVar = Var($"pair{Guid.NewGuid()}", pairType)
                    let headBack = Expr.Application(inner.ToJson, Expr.TupleGet(Expr.Var pairVar, 0))
                    let tailBack = Expr.Application(tail.ToJson, Expr.TupleGet(Expr.Var pairVar, 1))
                    let thenBranch =
                        Expr.Let(
                            pairVar,
                            CommonExprs.getOptionValue pairType (Expr.Var runtimeObjVar),
                            CommonExprs.newJsonValueArray (
                                CommonExprs.callArrayAppend
                                    (Expr.NewArray(typeof<JsonValue>, [ headBack ]))
                                    (CommonExprs.callJsonValueAsArray tailBack)
                                    typeof<JsonValue>
                            )
                        )
                    Expr.IfThenElse(CommonExprs.getOptionIsSome pairType (Expr.Var runtimeObjVar), thenBranch, CommonExprs.emptyJsonValueArray))

            // Some(h, t) -> h :: tail.ToList(t) | None -> []
            let toList =
                buildToList inner runtimeType (fun valueVar ->
                    let pairVar = Var($"pair{Guid.NewGuid()}", pairType)
                    let headExpr = Expr.TupleGet(Expr.Var pairVar, 0)
                    let tailExpr = Expr.TupleGet(Expr.Var pairVar, 1)
                    let tailListExpr = Expr.Application(tailArray.specific.ToList.Convert, tailExpr)
                    let thenBranch =
                        Expr.Let(
                            pairVar,
                            CommonExprs.getOptionValue pairType (Expr.Var valueVar),
                            CommonExprs.newListCons inner.RuntimeType headExpr tailListExpr
                        )
                    Expr.IfThenElse(CommonExprs.getOptionIsSome pairType (Expr.Var valueVar), thenBranch, CommonExprs.newListEmpty inner.RuntimeType))

            { common =
                { CompileTimeType = compileTimeType
                  RuntimeType = runtimeType
                  ToRuntime = toRuntime
                  ToJson = toJson
                  FullyCompilable = arrayKeywords.common.CanBeCompiled && inner.FullyCompilable && tail.FullyCompilable }
              specific = { ToList = toList; ToTuple = None } }

        // Default case: unbounded list.
        | Unbounded _ -> defaultArrayConversion

    // Shared by ExactLength (n > 1) and MinItemsPrefix: peels one head off the array and
    // recurses via `buildArrayConversion` on `tailKeywords` for the rest, producing (Head, Tail).
    // Splitting a value of this shape into head/tail is then just F#'s own tuple destructuring -
    // no runtime logic needed beyond building the pair itself.
    and private buildHeadTailConversion
        (context: GenerationContext)
        (typeMap: TypeMap)
        (innerType: JsonSchemaType)
        (arrayKeywords: JsonArray.Keywords)
        (inner: ProviderConfiguration.NodeConversion)
        (tailKeywords: JsonArray.Keywords)
        : ProviderConfiguration.ArrayConversion =

        let tailArray = buildArrayConversion context typeMap innerType tailKeywords
        let tail = tailArray.common

        let compileTimeType = Microsoft.FSharp.Reflection.FSharpType.MakeTupleType [| inner.CompileTimeType; tail.CompileTimeType |]
        let runtimeType = Microsoft.FSharp.Reflection.FSharpType.MakeTupleType [| inner.RuntimeType; tail.RuntimeType |]

        let toRuntime =
            withJsonArrayLambda (fun jsonArrVar ->
                let headRuntime = Expr.Application(inner.ToRuntime, CommonExprs.callArrayGet 0 (Expr.Var jsonArrVar) typeof<JsonValue>)
                let tailJsonValue = CommonExprs.newJsonValueArray (CommonExprs.callArraySkip 1 (Expr.Var jsonArrVar) typeof<JsonValue>)
                let tailRuntime = Expr.Application(tail.ToRuntime, tailJsonValue)
                Expr.NewTuple [ headRuntime; tailRuntime ])

        let toJson =
            CommonExprs.freshLambda "runtimeObj" runtimeType (fun runtimeObjVar ->
                let headBack = Expr.Application(inner.ToJson, Expr.TupleGet(Expr.Var runtimeObjVar, 0))
                let tailBack = Expr.Application(tail.ToJson, Expr.TupleGet(Expr.Var runtimeObjVar, 1))
                CommonExprs.newJsonValueArray (
                    CommonExprs.callArrayAppend
                        (Expr.NewArray(typeof<JsonValue>, [ headBack ]))
                        (CommonExprs.callJsonValueAsArray tailBack)
                        typeof<JsonValue>
                ))

        // head :: tail.ToList(tail-value)
        let toList =
            buildToList inner runtimeType (fun valueVar ->
                let headExpr = Expr.TupleGet(Expr.Var valueVar, 0)
                let tailExpr = Expr.TupleGet(Expr.Var valueVar, 1)
                let tailListExpr = Expr.Application(tailArray.specific.ToList.Convert, tailExpr)
                CommonExprs.newListCons inner.RuntimeType headExpr tailListExpr)

        { common =
            { CompileTimeType = compileTimeType
              RuntimeType = runtimeType
              ToRuntime = toRuntime
              ToJson = toJson
              FullyCompilable = arrayKeywords.common.CanBeCompiled && inner.FullyCompilable && tail.FullyCompilable }
          specific = { ToList = toList; ToTuple = None } }


    // Choice has a limit of 7 generic parameters, so more than 2 branches nest as Choice<T1, Choice<T2, Choice<T3, ...>>>
    and buildOneOfConversion
        (context: GenerationContext)
        (typeMap: TypeMap)
        (branchSchemaTypes: JsonSchemaType list)
        : ProviderConfiguration.NodeConversion =
        match branchSchemaTypes with
        | [] -> failwith "OneOf must have at least one type"
        | [ single ] -> convert context typeMap single
        | headType :: restTypes ->
            let head = convert context typeMap headType
            let tail = buildOneOfConversion context typeMap restTypes
            let compileTimeType = ProvidedTypeBuilder.MakeGenericType(typedefof<Choice<_, _>>, [ head.CompileTimeType; tail.CompileTimeType ])
            let runtimeType = ProvidedTypeBuilder.MakeGenericType(typedefof<Choice<_, _>>, [ head.RuntimeType; tail.RuntimeType ])
            let cases = Microsoft.FSharp.Reflection.FSharpType.GetUnionCases runtimeType
            let choice1, choice2 = cases.[0], cases.[1]

            let toRuntime =
                CommonExprs.freshLambda "jsonVal" typeof<JsonValue> (fun jsonValVar ->
                    let headMatches =
                        SchemaValidationExprs.validateJsonSchemaExpr (Expr.Var jsonValVar) context.SchemaHashCode context.SchemaString (pathOf headType)
                    let thenBranch = Expr.NewUnionCase(choice1, [ Expr.Application(head.ToRuntime, Expr.Var jsonValVar) ])
                    let elseBranch = Expr.NewUnionCase(choice2, [ Expr.Application(tail.ToRuntime, Expr.Var jsonValVar) ])
                    Expr.IfThenElse(headMatches, thenBranch, elseBranch))

            let toJson =
                CommonExprs.freshLambda "runtimeObj" runtimeType (fun runtimeObjVar ->
                    let isChoice1 = Expr.UnionCaseTest(Expr.Var runtimeObjVar, choice1)
                    let headValue = CommonExprs.callGetChoice1Of2 head.RuntimeType tail.RuntimeType (Expr.Var runtimeObjVar)
                    let tailValue = CommonExprs.callGetChoice2Of2 head.RuntimeType tail.RuntimeType (Expr.Var runtimeObjVar)
                    Expr.IfThenElse(isChoice1, Expr.Application(head.ToJson, headValue), Expr.Application(tail.ToJson, tailValue)))

            { CompileTimeType = compileTimeType; RuntimeType = runtimeType; ToRuntime = toRuntime; ToJson = toJson; FullyCompilable = head.FullyCompilable && tail.FullyCompilable }

    // Whether a class's own Create can skip Result-wrapping: its own JSON carries nothing
    // unmodeled, and every property's own conversion is itself FullyCompilable.
    and isClassFullyCompilable
        (context: GenerationContext)
        (typeMap: TypeMap)
        (keywords: JsonObject.Keywords)
        (properties: (PropertyName * JsonSchemaType) list)
        : bool =
        keywords.common.CanBeCompiled
        && properties |> List.forall (fun (_, propertyType) -> (convert context typeMap propertyType).FullyCompilable)

    let optionalOrPlainType (optional: bool) (dotnetType: Type) : Type =
        if optional then
            typedefof<_ option>.MakeGenericType(dotnetType)
        else
            dotnetType

    let nullableOrPlainType (optional: bool) (dotnetType: Type) : Type =
        if optional then
            if dotnetType.IsValueType then
                typedefof<Nullable<_>>.MakeGenericType(dotnetType)
            else
                dotnetType
        else
            dotnetType

    let defaultValueForNullableType (compileTimeType: Type) : obj =
        if compileTimeType.IsValueType then Nullable() else null

    let schemaTypeToMethodParameterType
        (context: GenerationContext)
        (typeMap: TypeMap)
        (optional: bool)
        (schemaType: JsonSchemaType)
        : Type =
        let compileTimeType = (convert context typeMap schemaType).CompileTimeType
        nullableOrPlainType optional compileTimeType
