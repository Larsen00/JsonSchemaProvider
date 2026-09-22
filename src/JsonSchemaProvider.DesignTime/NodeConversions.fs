namespace JsonSchemaProvider.DesignTime

module NodeConversions =
    open System
    open System.Reflection
    open FSharp.Quotations
    open FSharp.Data
    open SchemaConversion
    open ProviderImplementation.ProvidedTypes
    open JsonSchemaProvider

    type ClassMap = Map<string, ProvidedTypeDefinition>

    // Many of the functions in TypeProvider.fs/ExprGenerator.fs reuse the same static data, hence
    // a record type to bundle it instead of threading each field separately. Lives here (rather
    // than in TypeProvider.fs, where it used to be) so ExprGenerator.fs - which compiles before
    // TypeProvider.fs - can see it too.
    type GenerationContext =
        { Assembly: Assembly
          NamespaceName: string
          RootBaseType: Type
          SchemaHashCode: int32
          SchemaString: string
          CompileFlags: ProviderConfiguration.CompileFlags }

    // Every type carries its own Path
    // A root-level node's Path is "#".
    let pathOf (fsharpType: FSharpType) : string =
        match fsharpType with
        | FSharpDouble keywords
        | FSharpInt keywords            -> keywords.common.Path
        | FSharpBool keywords           -> keywords.common.Path
        | FSharpString keywords         -> keywords.common.Path
        | FSharpClass (keywords, _)     -> keywords.common.Path
        | FSharpList(_, keywords)       -> keywords.common.Path
        | FSharpOneOf (keywords, _, _)  -> keywords.Path

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

    // Builds everything there is to know about turning one FSharpType node into F#: its
    // compile-time type, its runtime/erased type, and the two conversion functions between
    // JsonValue and that runtime type
    //
    // Its done like this because otherwise we needed four separate functions that all needed
    // to stay in sync with each other. -- a bonus is that we also get better performance with less overhead
    let rec convert (context: GenerationContext) (classMap: ClassMap) (fSharpType: FSharpType) : ProviderConfiguration.NodeConversion =
        match fSharpType with
        | FSharpBool keywords -> { 
                CompileTimeType = typeof<bool>
                RuntimeType = typeof<bool>
                ToRuntime = <@@ fun (jsonVal: JsonValue) -> jsonVal.AsBoolean() @@>
                ToJson = <@@ fun (runtimeObj: bool) -> JsonValue.Boolean(runtimeObj) @@>
                FullyCompilable = keywords.common.CanBeCompiled
            }

        | FSharpInt keywords -> {
                CompileTimeType = typeof<int>
                RuntimeType = typeof<int>
                ToRuntime = <@@ fun (jsonVal: JsonValue) -> jsonVal.AsInteger() @@>
                ToJson = <@@ fun (runtimeObj: int) -> JsonValue.Number(decimal runtimeObj) @@>
                FullyCompilable = keywords.common.CanBeCompiled
            }

        | FSharpDouble keywords -> {
                CompileTimeType = typeof<double>
                RuntimeType = typeof<double>
                ToRuntime = <@@ fun (jsonVal: JsonValue) -> jsonVal.AsFloat() @@>
                ToJson = <@@ fun (runtimeObj: double) -> JsonValue.Float runtimeObj @@>
                FullyCompilable = keywords.common.CanBeCompiled
            }

        | FSharpString keywords -> {
                CompileTimeType = typeof<string>
                RuntimeType = typeof<string>
                ToRuntime = <@@ fun (jsonVal: JsonValue) -> jsonVal.AsString() @@>
                ToJson = <@@ fun (runtimeObj: string) -> JsonValue.String runtimeObj @@>
                FullyCompilable = keywords.common.CanBeCompiled
            }

        | FSharpClass(keywords, properties) ->
            {
                CompileTimeType = classMap[keywords.common.Path]
                RuntimeType = typeof<NullableJsonValue>
                ToRuntime = <@@ fun (jsonVal: JsonValue) -> NullableJsonValue jsonVal @@>
                ToJson = <@@ fun (runtimeObj: NullableJsonValue) -> runtimeObj.JsonVal @@>
                FullyCompilable = isClassFullyCompilable context classMap keywords properties
            }

        // Sicne array have compile time type support we delegate the conversion to `buildArrayConversion` function.
        | FSharpList(innerType, arrayKeywords) -> buildArrayConversion context classMap innerType arrayKeywords

        // OneOf types are handled by the `buildOneOfConversion` function. keywords.CanBeCompiled
        // is about the oneOf node itself (e.g. a stray keyword sitting alongside "oneOf"), which
        // buildOneOfConversion has no access to - folded in here instead.
        | FSharpOneOf (keywords, head, tail) ->
            let conv = buildOneOfConversion context classMap (head :: tail)
            { conv with FullyCompilable = keywords.CanBeCompiled && conv.FullyCompilable }


    // Performs the recursive conversion for array types based on their inner type and array keywords.
    //
    // Note: this assumes minItems and maxItems can never be 0 - a 0 value is converted to None in
    // SchemaConversion.fs.
    and buildArrayConversion
        (context: GenerationContext)
        (classMap: ClassMap)
        (innerType: FSharpType)
        (arrayKeywords: JsonArray.Keywords)
        : ProviderConfiguration.NodeConversion =

        let compileFlags = context.CompileFlags

        match arrayKeywords.specific with

        // Invalid case: minItems greater than maxItems
        | { MinItems = Some minItems; MaxItems = Some maxItems } when minItems > maxItems ->
            failwith "MinItems cannot be greater than MaxItems - Please check your schema."
        
        // Keyword combinations that does not have a compiled type -> so we gonna fallback to runtime validation
        | keys when keys.UniqueItems || not keys.AllowAdditionalItems || keys.HasAdditionalItemsSchema ->
            let resetKeywords =
                { arrayKeywords with
                    specific.MinItems = None
                    specific.MaxItems = None
                    specific.UniqueItems = false
                    specific.AllowAdditionalItems = true
                    specific.HasAdditionalItemsSchema = false
                }
            { buildArrayConversion context classMap innerType resetKeywords with FullyCompilable = false }
        // Exact-size tuple: minItems = maxItems, same shape both directions, nothing optional.
        | { MinItems = Some n; MaxItems = Some n2 } when n = n2 ->

            // Convert the inner type for the exact-size tuple case.
            let inner = convert context classMap innerType

            // Create the compile-time and runtime tuple types for the exact-size array.
            let compileTimeType = Array.create n inner.CompileTimeType |> Microsoft.FSharp.Reflection.FSharpType.MakeTupleType
            let runtimeType     = Array.create n inner.RuntimeType     |> Microsoft.FSharp.Reflection.FSharpType.MakeTupleType

            let toRuntime =
                withJsonArrayLambda (
                    fun jsonArrVar ->
                        // iterate over the elements of the JSON array and convert each to the runtime type
                        [ for i in 0 .. n - 1 -> 
                            Expr.Application(inner.ToRuntime, CommonExprs.callArrayGet i (Expr.Var jsonArrVar) typeof<JsonValue>) ]
                        // create a tuple from the converted elements
                        |> Expr.NewTuple
                    )

            let toJson =
                CommonExprs.freshLambda "runtimeObj" runtimeType (
                    fun runtimeObjVar ->
                        // iterate over the elements of the runtime tuple and convert each to JSON
                        let x = [ for i in 0 .. n - 1 -> Expr.Application(inner.ToJson, Expr.TupleGet(Expr.Var runtimeObjVar, i)) ]
                        // create a JSON array from the converted elements
                        let newArray = Expr.NewArray(typeof<JsonValue>, x)
                        CommonExprs.newJsonValueArray newArray
                    )

            { CompileTimeType = compileTimeType; RuntimeType = runtimeType; ToRuntime = toRuntime; ToJson = toJson; FullyCompilable = arrayKeywords.common.CanBeCompiled && inner.FullyCompilable }

        // minItems mandatory prefix, tail built by recursing on this same function - the tail
        // may itself land on the exact-tuple, maxItems-bounded, or open-list case below.
        | { MinItems = Some minItems; MaxItems = maxItems } when compileFlags.CompileMinItems ->
            let inner = convert context classMap innerType

            // Create keywords for the tailing list. 
            let restKeywords =
                { arrayKeywords with
                    specific.MinItems = None
                    specific.MaxItems = maxItems |> Option.map (fun m -> m - minItems) }

            // Build the conversion for the tailing list.
            let rest = buildArrayConversion context classMap innerType restKeywords
            
            // The compile-time and runtime types is a tuple with the fist minItems elements followed by the rest of the array as a tuple element.
            let compileTimeType =
                Array.append (Array.create minItems inner.CompileTimeType) [| rest.CompileTimeType |]
                |> Microsoft.FSharp.Reflection.FSharpType.MakeTupleType
            let runtimeType =
                Array.append (Array.create minItems inner.RuntimeType) [| rest.RuntimeType |]
                |> Microsoft.FSharp.Reflection.FSharpType.MakeTupleType


            let toRuntime =
                withJsonArrayLambda (
                    fun jsonArrVar ->
                        let elems =
                            // Convert the first minItems elements of the JSON array to the corresponding runtime types.
                            [ for i in 0 .. minItems - 1 ->
                                Expr.Application(inner.ToRuntime, CommonExprs.callArrayGet i (Expr.Var jsonArrVar) typeof<JsonValue>) ]
                        
                        // Takes the remaining elements and turns them into a JSON array 
                        let restJsonValue =
                            CommonExprs.newJsonValueArray (CommonExprs.callArraySkip minItems (Expr.Var jsonArrVar) typeof<JsonValue>)

                        // Convert the remaining elements of the JSON array to the corresponding runtime type using the rest conversion.
                        let tail = Expr.Application(rest.ToRuntime, restJsonValue) 
                        
                        // Construct a tuple with the first minItems elements followed by the converted rest of the array. Ie. at this pont we dont know/care what the rest type actually is.
                        Expr.NewTuple(elems @ [ tail ])
                    )

            let toJson =
                CommonExprs.freshLambda "runtimeObj" runtimeType (
                    fun runtimeObjVar ->
                        let elemsBack =
                            // Convert the first minItems elements of the runtime tuple to JSON values.
                            [ for i in 0 .. minItems - 1 -> Expr.Application(inner.ToJson, Expr.TupleGet(Expr.Var runtimeObjVar, i)) ]
                        
                        // Convert the remaining elements of the runtime tuple to JSON values.
                        let restBack = Expr.Application(rest.ToJson, Expr.TupleGet(Expr.Var runtimeObjVar, minItems))
                        CommonExprs.newJsonValueArray (
                            CommonExprs.callArrayAppend
                                (Expr.NewArray(typeof<JsonValue>, elemsBack))
                                (CommonExprs.callJsonValueAsArray restBack)
                                typeof<JsonValue>
                        )
                    )

            { CompileTimeType = compileTimeType; RuntimeType = runtimeType; ToRuntime = toRuntime; ToJson = toJson; FullyCompilable = arrayKeywords.common.CanBeCompiled && inner.FullyCompilable && rest.FullyCompilable }

        // maxItems = 1: option<inner>.
        | { MaxItems = Some 1 } ->
            // Extract the inner type
            let inner = convert context classMap innerType

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

            { CompileTimeType = compileTimeType; RuntimeType = runtimeType; ToRuntime = toRuntime; ToJson = toJson; FullyCompilable = arrayKeywords.common.CanBeCompiled && inner.FullyCompilable }

        // maxItems > 1: option<(inner * tail)>, tail built by recursing on this same function.
        | { MaxItems = Some maxItems } ->

            // im not super sure but i think i might accidental call this function maxitems times -- TODO
            let inner = convert context classMap innerType

            // Build the tail conversion by recursively calling this function with MaxItems decreased by 1.
            let tailKeywords = { arrayKeywords with specific.MaxItems = Some(maxItems - 1) }
            let tail = buildArrayConversion context classMap innerType tailKeywords

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

            { CompileTimeType = compileTimeType; RuntimeType = runtimeType; ToRuntime = toRuntime; ToJson = toJson; FullyCompilable = arrayKeywords.common.CanBeCompiled && inner.FullyCompilable && tail.FullyCompilable }

        // Default case: unbounded list.
        | _ ->
            // Extract the inner type conversion
            let inner = convert context classMap innerType

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

            // MinItems.IsNone matters here specifically: reaching this default case with MinItems
            // still Some means compileFlags.CompileMinItems was false, so minItems is a real,
            // uncompiled constraint - MaxItems needs no equivalent check, since any Some MaxItems
            // is always caught by one of the tuple/option cases above, unconditionally.
            { CompileTimeType = compileTimeType; RuntimeType = runtimeType; ToRuntime = toRuntime; ToJson = toJson; FullyCompilable = arrayKeywords.common.CanBeCompiled && arrayKeywords.specific.MinItems.IsNone && inner.FullyCompilable }

    // Choice has a limit of 7 generic parameters, so more than 2 branches nest as Choice<T1, Choice<T2, Choice<T3, ...>>>
    and buildOneOfConversion
        (context: GenerationContext)
        (classMap: ClassMap)
        (branchFSharpTypes: FSharpType list)
        : ProviderConfiguration.NodeConversion =
        match branchFSharpTypes with
        | [] -> failwith "OneOf must have at least one type"
        | [ single ] -> convert context classMap single
        | headType :: restTypes ->
            let head = convert context classMap headType
            let tail = buildOneOfConversion context classMap restTypes
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
    // unmodeled, and every property's own conversion is itself FullyCompilable. Split out from
    // convert's FSharpClass case (rather than inlined there) because TypeProvider.fs's
    // buildClassMapHelper and ExprGenerator.fs's generateCreateInvokeCode both need this same
    // answer while still building the class's own members - before its own path is registered in
    // classMap, so calling convert on the class itself (which needs classMap[keywords.common.Path])
    // isn't an option there. This only touches the properties, never the class's own entry, so it
    // works from all three call sites.
    and isClassFullyCompilable
        (context: GenerationContext)
        (classMap: ClassMap)
        (keywords: JsonObject.Keywords)
        (properties: (PropertyName * FSharpType) list)
        : bool =
        keywords.common.CanBeCompiled
        && properties |> List.forall (fun (_, propertyType) -> (convert context classMap propertyType).FullyCompilable)

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

    let fSharpTypeToMethodParameterType
        (context: GenerationContext)
        (classMap: ClassMap)
        (optional: bool)
        (fSharpType: FSharpType)
        : Type =
        let compileTimeType = (convert context classMap fSharpType).CompileTimeType
        nullableOrPlainType optional compileTimeType
