namespace JsonSchemaProvider.DesignTime

module ExprGenerator =
    open FSharp.Quotations
    open FSharp.Data
    open SchemaConversion
    open NodeConversions
    open SchemaValidationExprs
    open JsonSchemaProvider
    open System
    open FSharp.Quotations.Patterns
    open FSharp.Quotations.DerivedPatterns



    // "optional" (an F# property/parameter being absent, represented as Nullable<T> for value
    // types) is a call-site concern, not a property of the node's own JsonValue<->runtime
    // conversion - NodeConversion.ToJson always expects a plain (non-Nullable) RuntimeType. This
    // wraps it to also accept Nullable<RuntimeType> when the value type needs that.
    let private wrapOptionalToJson (conv: ProviderConfiguration.NodeConversion) (optional: bool) : Expr =
        if optional && conv.RuntimeType.IsValueType then
            let nullableType = typedefof<Nullable<_>>.MakeGenericType conv.RuntimeType
            CommonExprs.freshLambda "nullableObj" nullableType (fun nullableVar ->
                Expr.Application(conv.ToJson, CommonExprs.getNullableValue conv.RuntimeType (Expr.Var nullableVar)))
        else
            conv.ToJson

    // only for class
    let generatePropertyGetter
        (context: GenerationContext)
        (classMap: ClassMap)
        (keywords:  JsonObject.Keywords)
        ((name, innertype): PropertyName * FSharpType)
        : Expr list -> Expr =
        let conv = convert context classMap innertype
        let plainPropertyRuntimeType = conv.RuntimeType
        let convertToRuntimeType = conv.ToRuntime

        if not <| Map.find name keywords.specific.Required then
            fun (args: Expr list) ->
                // Implements:
                // <@@
                //     match %%(args[0]).JsonVal.TryGetProperty(name) with
                //     | None -> None
                //     | Some(jsonVal) -> Some((%%conversion) jsonVal)
                //
                //     let maybeProperty = %%(args[0]).JsonVal.TryGetProperty(name)
                //     if maybeProperty.IsSome then
                //         Some(%%conversion maybeProperty.Value)
                //     else
                //         None
                // @@>
                let scrutineeVar = Var($"maybeProperty{Guid.NewGuid()}", typeof<JsonValue option>)
                let jsonVal = CommonExprs.getNullableJsonValueJsonVal args[0]
                let maybePropertySelect = CommonExprs.callJsonValueTryGetPropertyName jsonVal name

                let isSome = CommonExprs.getOptionIsSome typeof<JsonValue> (Expr.Var(scrutineeVar))

                let thenBranch =
                    CommonExprs.newOptionSome
                        plainPropertyRuntimeType
                        (Expr.Application(
                            convertToRuntimeType,
                            CommonExprs.getOptionValue typeof<JsonValue> (Expr.Var(scrutineeVar))
                        ))

                let elseBranch = CommonExprs.newOptionNone plainPropertyRuntimeType

                Expr.Let(scrutineeVar, maybePropertySelect, Expr.IfThenElse(isSome, thenBranch, elseBranch))
        else
            // Implements: <@@ %%conversion %%(args[0]).JsonVal[name] @@>
            fun (args: Expr list) ->
                let jsonVal = CommonExprs.getNullableJsonValueJsonVal args[0]

                let propertySelect = CommonExprs.callJsonValueItem jsonVal name

                Expr.Application(convertToRuntimeType, propertySelect)

    let private generateIsNullCheck (fSharpType: FSharpType) (arg: Expr) : Expr =
        match fSharpType with
        | FSharpBool(_) -> CommonExprs.callOpNot (CommonExprs.getNullableHasValue typeof<bool> arg)
        | FSharpInt(_) -> CommonExprs.callOpNot (CommonExprs.getNullableHasValue typeof<int> arg)
        | FSharpDouble(_) -> CommonExprs.callOpNot (CommonExprs.getNullableHasValue typeof<double> arg)
        | _ -> CommonExprs.callOpEquality arg (Expr.Value(null))

    let private generatePropertyCreation
        (context: GenerationContext)
        (classMap: ClassMap)
        (name: string)
        (optional: bool)
        (fSharpType: FSharpType)
        (arg: Expr)
        =
        let conv = convert context classMap fSharpType
        let toJson = wrapOptionalToJson conv optional

        if optional then
            let isNull = generateIsNullCheck fSharpType arg

            let thenBranch = Expr.NewArray(typeof<string * JsonValue>, [])

            let elseBranch =
                Expr.NewArray(
                    typeof<string * JsonValue>,
                    [ Expr.NewTuple
                          [ Expr.Value(name)
                            Expr.Application(toJson, arg) ]
                    ]
                )

            Expr.IfThenElse(isNull, thenBranch, elseBranch)
        else
            Expr.NewArray(
                typeof<string * JsonValue>,
                [ Expr.NewTuple
                      [ Expr.Value(name)
                        Expr.Application(toJson, arg) ]
                ]
            )


    // Extracts the MethodInfo behind a two-argument top-level function, e.g.
    //   methodInfoOf2 <@ fun (a: float) (b: string) -> Number.create a b @>
    let methodInfoOf2 (expr: Expr<'a -> 'b -> 'c>) =
        match expr with
        | Lambdas(_, Call(_, mi, _)) -> mi
        | _ -> failwith "Expected a quotation of the form <@ fun a b -> SomeModule.someFunction a b @>"

    // Extracts the MethodInfo behind a one-argument top-level function, e.g.
    //  methodInfoOf <@ fun (a: float) -> Number.create a @>
    let methodInfoOf1 (expr: Expr<'a -> 'b -> 'c>) =
        match expr with
        | Lambdas(_, Call(_, mi, _)) -> mi
        | _ -> failwith "Expected a quotation of the form <@ fun a b -> SomeModule.someFunction a b @>"


    let generateCreateInvokeCode
        (context: GenerationContext)
        (classMap: ClassMap)
        (fsharptype: FSharpType)
        : Expr list -> Expr =

        // Plain locals extracted up front so the quotations below only ever close over ordinary
        // values (int/string), never `context` itself - a quoted `context.SchemaHashCode` would
        // try to splice the whole GenerationContext record into the generated code.
        let schemaHashCode = context.SchemaHashCode
        let schemaSource = context.SchemaString

        match fsharptype with
        | FSharpClass(keywords, properties) ->
            fun (args: Expr list) ->
                let elementType = typedefof<(string * JsonValue)[]>

                let elements =[
                    for (name, innerType), arg in List.zip properties args ->
                        generatePropertyCreation context classMap name (not <| Map.find name keywords.specific.Required) innerType arg
                    ]

                let fields = Expr.NewArray(elementType, elements)

                let jsonValExpr = <@@ JsonValue.Record(Array.concat (%%fields: (string * JsonValue)[][])) @@>

                if  context.CompileFlags.SkipRuntimeValidation || isClassFullyCompilable context classMap keywords properties then
                    <@@ NullableJsonValue(%%jsonValExpr: JsonValue) @@>
                else
                    let path = keywords.common.Path
                    <@@
                        let record = NullableJsonValue(%%jsonValExpr: JsonValue)
                        validateJsonSchema path record schemaHashCode schemaSource
                    @@>


        // Only hitting this branch when the type is at the root of the json Schema
        // never gets its own Create when nested as a property, so this always validates against
        // the whole schema directly, no path lookup needed.
        | FSharpBool _ | FSharpInt _ | FSharpDouble _ | FSharpString _ | FSharpList _ | FSharpOneOf _ ->
            fun (args: Expr list) ->
                
                if  context.CompileFlags.SkipRuntimeValidation || (convert context classMap fsharptype).FullyCompilable then
                    args[0]
                else 
                    let conv = convert context classMap fsharptype
                    let jsonValExpr = Expr.Application(conv.ToJson, args[0])
                    let jsonTextExpr = <@@ (%%jsonValExpr: JsonValue).ToString() @@>

                    let path = pathOf fsharptype
                    let errorsExpr = <@@ collectValidationErrors path (%%jsonTextExpr: string) schemaHashCode schemaSource @@>

                    // The success payload is args[0] itself (the caller's bool/int/double/string/list),
                    // not the JsonValue used for validation - those are different types, and which one
                    // args[0] actually is isn't resolved until here, so Result<successType, _> is built
                    // directly with Expr.NewUnionCase, same as the Choice1/Choice2 construction above.
                    let successType = conv.RuntimeType
                    let resultType = typedefof<Result<_, _>>.MakeGenericType(successType, typeof<string list>)
                    let cases = Reflection.FSharpType.GetUnionCases resultType
                    let okCase, errorCase = cases.[0], cases.[1]

                    let errorsVar = Var($"validationErrors{Guid.NewGuid()}", typeof<string list>)
                    let isEmpty = <@@ List.isEmpty (%%(Expr.Var errorsVar): string list) @@>

                    Expr.Let(
                        errorsVar,
                        errorsExpr,
                        Expr.IfThenElse(
                            isEmpty,
                            Expr.NewUnionCase(okCase, [ args[0] ]),
                            Expr.NewUnionCase(errorCase, [ Expr.Var errorsVar ])
                        )
                    )
