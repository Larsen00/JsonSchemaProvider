namespace JsonSchemaProvider.DesignTime

module ExprGenerator =
    open FSharp.Quotations
    open FSharp.Data
    open SchemaConversion
    open TypeLevelConversion
    open JsonSchemaProvider
    open System
    open ProviderImplementation.ProvidedTypes
    open FSharp.Data.Runtime
    open FSharp.Quotations.Patterns
    open FSharp.Quotations.DerivedPatterns



    let validateJsonSchema path record (schemaHashCode: int32) (schemaSource: string) =
        let recordSource = record.ToString()
        let rootschema = SchemaCache.retrieveSchema schemaHashCode schemaSource

        // This allow us to validate a nested class on .create if the path is '#' then we are at the root.
        let subschema =
            if path = "#" then
                rootschema
            else
                SchemaCache.resolveByPath rootschema path

        let validationErrors = subschema.Validate recordSource


        if Seq.isEmpty validationErrors then
                Ok ()
            else
                let message =
                    validationErrors
                    |> Seq.map (fun validationError -> validationError.ToString())
                    |> fun msgs -> System.String.Join(", ", msgs) |> sprintf "JSON Schema validation failed: %s"

                Error message


    let validateJsonSchemaExpr (jsonValExpr: Expr) schemaHashCode schemaSource path =
        <@@ validateJsonSchema path (%%jsonValExpr: JsonValue) schemaHashCode schemaSource |> Result.isOk @@>
        

    let rec private generateStructualMatchExpr (context: GenerationContext) (fsharpType: FSharpType) (jsonValExpr: Expr) =


        // An applay function that takes in the path to the sub schema and then validated the jsonValExpr agains it.
        let validate = validateJsonSchemaExpr jsonValExpr context.SchemaHashCode context.SchemaString

        match fsharpType with
        | FSharpDouble keywords 
        | FSharpInt keywords        -> keywords.common.Path |> validate
        | FSharpBool keywords       -> keywords.common.Path |> validate
        | FSharpString keywords     -> keywords.common.Path |> validate
        | FSharpClass (keywords, _) -> keywords.common.Path |> validate
        | FSharpList(_, keywords)   -> keywords.common.Path |> validate


        | FSharpOneOf [single] -> 
            generateStructualMatchExpr context single jsonValExpr
        | FSharpOneOf (head :: tail) ->
            let headMatchExpr = generateStructualMatchExpr context head jsonValExpr
            let tailMatchExpr = generateStructualMatchExpr context (FSharpOneOf tail) jsonValExpr
            <@@ %%headMatchExpr || %%tailMatchExpr @@>

    let rec private generateJsonValToRuntimeTypeConversion
        (context: GenerationContext)
        (classMap: ClassMap)
        (fSharpType: FSharpType)
        : Expr =
        match fSharpType with
        | FSharpBool(_) -> <@@ fun (jsonVal: JsonValue) -> jsonVal.AsBoolean() @@>
        | FSharpClass(_) -> <@@ fun (jsonVal: JsonValue) -> NullableJsonValue(jsonVal) @@>
        | FSharpList(innerType, arrayKeywords) -> //TODO: An idea would be to greate a file for the List type that holds this conversion as when we add more keywords it will get more complex
            // Implements: <@@ fun (jsonVal: JsonValue) -> List.ofArray (Array.map %%generateForInner (jsonVal.AsArray())) @@>
            // Recursive call to generateJsonValToRuntimeTypeConversion for the inner type
            let generateForInner: Expr =
                generateJsonValToRuntimeTypeConversion context classMap innerType

            // Get the runtime type of the inner type
            let innerRuntimeType: Type =
                fSharpTypeToRuntimeType classMap innerType context.CompileFlags

            // Declare a variable to hold the JsonValue parameter
            let jsonValVar: Var =
                Var($"jsonVal{Guid.NewGuid()}", typeof<JsonValue>)


            let jsonValAsArray: Expr =
                CommonExprs.callJsonValueAsArray (Expr.Var jsonValVar)

            match arrayKeywords.specific.MinItems, context.CompileFlags.CompileMinItems with
            | Some minItems, true when minItems > 0 ->

                let mappedArrVar = Var($"mappedArr{Guid.NewGuid()}",innerRuntimeType.MakeArrayType())
                let mappedArrExpr = CommonExprs.callArrayMap generateForInner jsonValAsArray typeof<JsonValue> innerRuntimeType

                let elemExprs: list<Expr> = [ 
                    for i in 0 .. minItems - 1 -> CommonExprs.callArrayGet i (Expr.Var mappedArrVar) innerRuntimeType
                    ]
                let restExpr =
                    CommonExprs.callListOfArray
                        (CommonExprs.callArraySkip minItems (Expr.Var(mappedArrVar)) innerRuntimeType)
                        innerRuntimeType

                let tupleExpr = Expr.NewTuple(elemExprs @ [ restExpr ])
                Expr.Lambda(jsonValVar, Expr.Let(mappedArrVar, mappedArrExpr, tupleExpr))
                
            | _ -> 
                let mappedArray: Expr =
                    CommonExprs.callArrayMap generateForInner jsonValAsArray typeof<JsonValue> innerRuntimeType
                let arrayAsList: Expr = 
                    CommonExprs.callListOfArray mappedArray innerRuntimeType
                Expr.Lambda(jsonValVar, arrayAsList)
        | FSharpDouble(_) -> <@@ fun (jsonVal: JsonValue) -> jsonVal.AsFloat() @@>
        | FSharpInt(_) -> <@@ fun (jsonVal: JsonValue) -> jsonVal.AsInteger() @@>
        | FSharpString(_) -> <@@ fun (jsonVal: JsonValue) -> jsonVal.AsString() @@>
        // We can assume that the json value is a valid one, hence we can justify that the first branch of oneOf that matches the json value is the correct one. 
        | FSharpOneOf [single] ->
            generateJsonValToRuntimeTypeConversion context classMap single
        | FSharpOneOf (head :: tail) ->

            // Frist we generate the conversion for the head and tail of the oneOf type. (The tail being how to unfold the choise type)
            let headConversion = generateJsonValToRuntimeTypeConversion context classMap head
            let tailConversion = generateJsonValToRuntimeTypeConversion context classMap (FSharpOneOf tail)

            // Get the type of the choice ie. something like Choice<_, _>
            let choiceType = fSharpTypeToRuntimeType classMap fSharpType context.CompileFlags

            // Retrive the types within the choice
            let cases = Reflection.FSharpType.GetUnionCases choiceType
            let choice1 = cases.[0] // Will always be a FSharpType
            let choice2 = cases.[1] // Can be a FSharpType or another Choice type

            let jsonValVar = Var($"jsonVal{Guid.NewGuid()}", typeof<JsonValue>)
            let jsonValExpr = Expr.Var jsonValVar

            // Generate the expression that checks if the json value matches the head type
            let headMatches = generateStructualMatchExpr context head jsonValExpr
            
            // The expression that will be executed if the head matches - ie. we will convert the json value to the head type
            let thenBranch = Expr.NewUnionCase(choice1, [Expr.Application(headConversion, jsonValExpr) ])

            // The expression that will be executed if the head does not match - ie. we will continue the unfolding of the choice type and try to match the nect type in the choice.
            let elseBranch = Expr.NewUnionCase(choice2, [Expr.Application(tailConversion, jsonValExpr) ])


            Expr.Lambda(jsonValVar, Expr.IfThenElse(headMatches, thenBranch,
            elseBranch))


    let rec private generateRuntimeTypeToJsonValConversion
        (context: GenerationContext)
        (classMap: ClassMap)
        (optional: bool)
        (fSharpType: FSharpType)
        : Expr =
        match fSharpType with
        | FSharpBool(_) ->
            if optional then
                <@@ fun (runtimeObj: Nullable<bool>) -> JsonValue.Boolean(runtimeObj.Value) @@>
            else
                <@@ fun (runtimeObj: bool) -> JsonValue.Boolean(runtimeObj) @@>
        | FSharpClass(_) -> <@@ fun (runtimeObj: NullableJsonValue) -> runtimeObj.JsonVal @@>
        | FSharpList(innerType, arrayKeywords) ->
            // Implements: <@@ fun runtimeObj -> Array.ofList (List.map %%generatoreForInner runtimeObj)@@>
            let generateForInner: Expr =
                generateRuntimeTypeToJsonValConversion context classMap false innerType

            let innerRuntimeType = fSharpTypeToRuntimeType classMap innerType context.CompileFlags
            let listRuntimeType = fSharpTypeToRuntimeType classMap fSharpType context.CompileFlags
            let runtimeObjVar = Var($"runtimeObj{Guid.NewGuid}", listRuntimeType)

            match arrayKeywords.specific.MinItems, context.CompileFlags.CompileMinItems with
            | Some minItems, true when minItems > 0 ->

                let elemExprs = [ for i in 0 .. minItems - 1 -> Expr.Application(generateForInner, Expr.TupleGet(Expr.Var runtimeObjVar, i)) ]

                let restList = Expr.TupleGet(Expr.Var runtimeObjVar, minItems)
                let mappedRestList = CommonExprs.callListMap generateForInner restList innerRuntimeType typeof<JsonValue>
                let restArray = CommonExprs.callArrayOfList mappedRestList typeof<JsonValue>

                // Concatenate the fixed elements and the rest of the array
                let mandatoryArray = Expr.NewArray(typeof<JsonValue>, elemExprs)
                let fullArray: Expr = CommonExprs.callArrayAppend mandatoryArray restArray typeof<JsonValue>
                Expr.Lambda(runtimeObjVar, CommonExprs.newJsonValueArray fullArray)
                

            | _ -> 
                let mappedList =
                    CommonExprs.callListMap generateForInner (Expr.Var(runtimeObjVar)) innerRuntimeType typeof<JsonValue>

                let listAsArray = CommonExprs.callArrayOfList mappedList typeof<JsonValue>
                Expr.Lambda(runtimeObjVar, CommonExprs.newJsonValueArray listAsArray)
        | FSharpDouble(_) ->
            if optional then
                <@@ fun (runtimeObj: Nullable<double>) -> JsonValue.Float(runtimeObj.Value) @@>
            else
                <@@ fun (runtimeObj: double) -> JsonValue.Float(runtimeObj) @@>
        | FSharpInt(_) ->
            if optional then
                <@@ fun (runtimeObj: Nullable<int>) -> JsonValue.Number(decimal runtimeObj.Value) @@>
            else
                <@@ fun (runtimeObj: int) -> JsonValue.Number(decimal runtimeObj) @@>
        | FSharpString(_) -> <@@ fun (runtimeObj: string) -> JsonValue.String(runtimeObj) @@>

        | FSharpOneOf [single] ->
            generateRuntimeTypeToJsonValConversion context classMap optional single

        | FSharpOneOf (head :: rest) ->
            let headConversion = generateRuntimeTypeToJsonValConversion context classMap false head
            let restConversion = generateRuntimeTypeToJsonValConversion context classMap false (FSharpOneOf rest)

            let choiceType = fSharpTypeToRuntimeType classMap fSharpType context.CompileFlags
            let cases = Reflection.FSharpType.GetUnionCases choiceType
            let choice1 = cases.[0]

            let headRuntimeType = fSharpTypeToRuntimeType classMap head context.CompileFlags
            let tailRuntimeType = fSharpTypeToRuntimeType classMap (FSharpOneOf rest) context.CompileFlags

            let runtimeObjVar = Var($"runtimeObj{Guid.NewGuid()}", choiceType)

            let isChoice1 = Expr.UnionCaseTest(Expr.Var runtimeObjVar, choice1)
            let headValue = CommonExprs.callGetChoice1Of2 headRuntimeType tailRuntimeType (Expr.Var runtimeObjVar)
            let thenBranch = Expr.Application(headConversion, headValue)

            let restValue = CommonExprs.callGetChoice2Of2 headRuntimeType tailRuntimeType (Expr.Var runtimeObjVar)
            let elseBranch = Expr.Application(restConversion, restValue)

            Expr.Lambda(runtimeObjVar, Expr.IfThenElse(isChoice1, thenBranch, elseBranch))
                     

    // only for class
    let generatePropertyGetter
        (context: GenerationContext)
        (classMap: ClassMap)
        (keywords:  JsonObject.Keywords)
        ((name, innertype): PropertyName * FSharpType)
        : Expr list -> Expr =
        let plainPropertyRuntimeType = fSharpTypeToRuntimeType classMap innertype context.CompileFlags

        let convertToRuntimeType =
            generateJsonValToRuntimeTypeConversion context classMap innertype

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
        if optional then
            let isNull = generateIsNullCheck fSharpType arg

            let thenBranch = Expr.NewArray(typeof<string * JsonValue>, [])

            let elseBranch =
                Expr.NewArray(
                    typeof<string * JsonValue>,
                    [ Expr.NewTuple
                          [ Expr.Value(name)
                            Expr.Application(generateRuntimeTypeToJsonValConversion context classMap optional fSharpType, arg) ]
                    ]
                )

            Expr.IfThenElse(isNull, thenBranch, elseBranch)
        else
            Expr.NewArray(
                typeof<string * JsonValue>,
                [ Expr.NewTuple
                      [ Expr.Value(name)
                        Expr.Application(generateRuntimeTypeToJsonValConversion context classMap optional fSharpType, arg) ]
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
        // values (int/string), never `context` itself - see generateStructualMatchExpr's
        // FSharpBool case for why.
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
                let path = keywords.common.Path

                let jsonValExpr =
                    <@@
                        JsonValue.Record(Array.concat (%%fields: (string * JsonValue)[][]))
                    @@>

                <@@
                    let record = NullableJsonValue(%%jsonValExpr: JsonValue)
                    let recordSource = record.ToString()


                    let rootschema = SchemaCache.retrieveSchema schemaHashCode schemaSource

                    // This allow us to validate a nested class on .create if the path is '#' then we are at the root.
                    let subschema =
                        if path = "#" then
                            rootschema
                        else
                            SchemaCache.resolveByPath rootschema path

                    let validationErrors = subschema.Validate recordSource

                    if Seq.isEmpty validationErrors then
                        record
                    else
                        let message =
                            validationErrors
                            |> Seq.map (fun validationError -> validationError.ToString())
                            |> fun msgs -> System.String.Join(", ", msgs) |> sprintf "JSON Schema validation failed: %s"

                        raise (ArgumentException(message, recordSource))
                 @@>

        // Only hitting this branch when the type is at the root of the json Schema 
        // never gets its own Create when nested as a property, so this always validates against
        // the whole schema directly, no path lookup needed.
        | FSharpBool _ | FSharpInt _ | FSharpDouble _ | FSharpString _ | FSharpList _ ->
            fun (args: Expr list) ->
                let toJsonVal = generateRuntimeTypeToJsonValConversion context classMap false fsharptype
                let jsonValExpr = Expr.Application(toJsonVal, args[0])

                // Evaluates to unit: raises on failure, otherwise falls through. Sequenced with
                // args[0] below so the overall Expr's type is just whatever args[0] already is -
                // no need to guess/ascribe which of the four primitive types we're in.
                let validateExpr =
                    <@@
                        let jsonVal = (%%jsonValExpr: JsonValue)
                        let recordSource = jsonVal.ToString()

                        let schema = SchemaCache.retrieveSchema schemaHashCode schemaSource
                        let validationErrors = schema.Validate recordSource

                        if Seq.isEmpty validationErrors then
                            ()
                        else
                            let message =
                                validationErrors
                                |> Seq.map (fun validationError -> validationError.ToString())
                                |> fun msgs -> System.String.Join(", ", msgs) |> sprintf "JSON Schema validation failed: %s"

                            raise (ArgumentException(message, recordSource))
                    @@>

                Expr.Sequential(validateExpr, args[0])

        | _ -> failwith "hmm idk"
