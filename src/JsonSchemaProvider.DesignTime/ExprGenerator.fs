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

    let rec private generateStructualMatchExpr (fsharpType: FSharpType) (jsonValExpr: Expr) =
        match fsharpType with
        | FSharpBool -> <@@ (JsonRefinement.tryBoolean %%jsonValExpr).IsSome @@>
        | FSharpInt -> <@@ (JsonRefinement.tryInteger %%jsonValExpr).IsSome @@>
        | FSharpDouble -> <@@ (JsonRefinement.tryFloat %%jsonValExpr).IsSome @@>
        | FSharpString -> <@@ (JsonRefinement.tryString %%jsonValExpr).IsSome @@>
        | FSharpClass(_) -> <@@ (JsonRefinement.tryObject %%jsonValExpr).IsSome @@>
        | FSharpList(_, arrayKeywords) -> <@@ (JsonRefinement.tryArray %%jsonValExpr arrayKeywords).IsSome @@> // Todo: also check the content of the array to match the inner type
        | FSharpOneOf [single] -> generateStructualMatchExpr single jsonValExpr
        | FSharpOneOf (head :: tail) -> 
            let headMatchExpr = generateStructualMatchExpr head jsonValExpr
            let tailMatchExpr = generateStructualMatchExpr (FSharpOneOf tail) jsonValExpr
            <@@ %%headMatchExpr || %%tailMatchExpr @@>




    let rec private generateJsonValToRuntimeTypeConversion
        (classMap: Map<string, ProvidedTypeDefinition>)
        (fSharpType: FSharpType)
        (compileFlags: ProviderConfiguration.CompileFlags)
        : Expr =
        match fSharpType with
        | FSharpBool -> <@@ fun (jsonVal: JsonValue) -> jsonVal.AsBoolean() @@>
        | FSharpClass(_) -> <@@ fun (jsonVal: JsonValue) -> NullableJsonValue(jsonVal) @@>
        | FSharpList(innerType, arrayKeywords) -> //TODO: An idea would be to greate a file for the List type that holds this conversion as when we add more keywords it will get more complex 
            // Implements: <@@ fun (jsonVal: JsonValue) -> List.ofArray (Array.map %%generateForInner (jsonVal.AsArray())) @@>
            // Recursive call to generateJsonValToRuntimeTypeConversion for the inner type
            let generateForInner: Expr = 
                generateJsonValToRuntimeTypeConversion classMap innerType compileFlags

            // Get the runtime type of the inner type
            let innerRuntimeType: Type = 
                fSharpTypeToRuntimeType classMap innerType compileFlags

            // Declare a variable to hold the JsonValue parameter
            let jsonValVar: Var = 
                Var($"jsonVal{Guid.NewGuid()}", typeof<JsonValue>)


            let jsonValAsArray: Expr = 
                CommonExprs.callJsonValueAsArray (Expr.Var jsonValVar)

            match arrayKeywords.MinItems, compileFlags.CompileMinItems with
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
        | FSharpDouble -> <@@ fun (jsonVal: JsonValue) -> jsonVal.AsFloat() @@>
        | FSharpInt -> <@@ fun (jsonVal: JsonValue) -> jsonVal.AsInteger() @@>
        | FSharpString -> <@@ fun (jsonVal: JsonValue) -> jsonVal.AsString() @@>
        // We can assume that the json value is a valid one, hence we can justify that the first branch of oneOf that matches the json value is the correct one. 
        | FSharpOneOf [single] -> 
            generateJsonValToRuntimeTypeConversion classMap single compileFlags
        | FSharpOneOf (head :: tail) ->

            // Frist we generate the conversion for the head and tail of the oneOf type. (The tail being how to unfold the choise type)
            let headConversion = generateJsonValToRuntimeTypeConversion classMap head compileFlags
            let tailConversion = generateJsonValToRuntimeTypeConversion classMap (FSharpOneOf tail) compileFlags

            // Get the type of the choice ie. something like Choice<_, _> 
            let choiceType = fSharpTypeToRuntimeType classMap fSharpType compileFlags

            // Retrive the types within the choice 
            let cases = Reflection.FSharpType.GetUnionCases choiceType
            let choice1 = cases.[0] // Will always be a FSharpType
            let choice2 = cases.[1] // Can be a FSharpType or another Choice type

            let jsonValVar = Var($"jsonVal{Guid.NewGuid()}", typeof<JsonValue>)
            let jsonValExpr = Expr.Var jsonValVar

            // Generate the expression that checks if the json value matches the head type 
            let headMatches = generateStructualMatchExpr head jsonValExpr
            
            // The expression that will be executed if the head matches - ie. we will convert the json value to the head type
            let thenBranch = Expr.NewUnionCase(choice1, [Expr.Application(headConversion, jsonValExpr) ])

            // The expression that will be executed if the head does not match - ie. we will continue the unfolding of the choice type and try to match the nect type in the choice.
            let elseBranch = Expr.NewUnionCase(choice2, [Expr.Application(tailConversion, jsonValExpr) ])


            Expr.Lambda(jsonValVar, Expr.IfThenElse(headMatches, thenBranch,
            elseBranch))


    let rec private generateRuntimeTypeToJsonValConversion
        (classMap: Map<string, ProvidedTypeDefinition>)
        (optional: bool)
        (fSharpType: FSharpType)
        (compileFlags: ProviderConfiguration.CompileFlags)
        : Expr =
        match fSharpType with
        | FSharpBool ->
            if optional then
                <@@ fun (runtimeObj: Nullable<bool>) -> JsonValue.Boolean(runtimeObj.Value) @@>
            else
                <@@ fun (runtimeObj: bool) -> JsonValue.Boolean(runtimeObj) @@>
        | FSharpClass(_) -> <@@ fun (runtimeObj: NullableJsonValue) -> runtimeObj.JsonVal @@>
        | FSharpList(innerType, arrayKeywords) ->
            // Implements: <@@ fun runtimeObj -> Array.ofList (List.map %%generatoreForInner runtimeObj)@@>
            let generateForInner: Expr =
                generateRuntimeTypeToJsonValConversion classMap false innerType compileFlags

            let innerRuntimeType = fSharpTypeToRuntimeType classMap innerType compileFlags
            let listRuntimeType = fSharpTypeToRuntimeType classMap fSharpType compileFlags
            let runtimeObjVar = Var($"runtimeObj{Guid.NewGuid}", listRuntimeType)

            match arrayKeywords.MinItems, compileFlags.CompileMinItems with
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
        | FSharpDouble ->
            if optional then
                <@@ fun (runtimeObj: Nullable<double>) -> JsonValue.Float(runtimeObj.Value) @@>
            else
                <@@ fun (runtimeObj: double) -> JsonValue.Float(runtimeObj) @@>
        | FSharpInt ->
            if optional then
                <@@ fun (runtimeObj: Nullable<int>) -> JsonValue.Number(decimal runtimeObj.Value) @@>
            else
                <@@ fun (runtimeObj: int) -> JsonValue.Number(decimal runtimeObj) @@>
        | FSharpString -> <@@ fun (runtimeObj: string) -> JsonValue.String(runtimeObj) @@>

        | FSharpOneOf [single] -> 
            generateRuntimeTypeToJsonValConversion classMap optional single compileFlags

        | FSharpOneOf (head :: rest) -> 
            let headConversion = generateRuntimeTypeToJsonValConversion classMap false head compileFlags
            let restConversion = generateRuntimeTypeToJsonValConversion classMap false (FSharpOneOf rest) compileFlags

            let choiceType = fSharpTypeToRuntimeType classMap fSharpType compileFlags
            let cases = Reflection.FSharpType.GetUnionCases choiceType
            let choice1 = cases.[0]

            let headRuntimeType = fSharpTypeToRuntimeType classMap head compileFlags
            let tailRuntimeType = fSharpTypeToRuntimeType classMap (FSharpOneOf rest) compileFlags

            let runtimeObjVar = Var($"runtimeObj{Guid.NewGuid()}", choiceType)

            let isChoice1 = Expr.UnionCaseTest(Expr.Var runtimeObjVar, choice1)
            let headValue = CommonExprs.callGetChoice1Of2 headRuntimeType tailRuntimeType (Expr.Var runtimeObjVar)
            let thenBranch = Expr.Application(headConversion, headValue)

            let restValue = CommonExprs.callGetChoice2Of2 headRuntimeType tailRuntimeType (Expr.Var runtimeObjVar)
            let elseBranch = Expr.Application(restConversion, restValue)

            Expr.Lambda(runtimeObjVar, Expr.IfThenElse(isChoice1, thenBranch, elseBranch))
                     

    let generatePropertyGetter
        (classMap: Map<string, ProvidedTypeDefinition>)
        { Name = name
          Optional = optional
          FSharpType = fSharpType }
        (compileFlags: ProviderConfiguration.CompileFlags)
        : Expr list -> Expr =
        let plainPropertyRuntimeType = fSharpTypeToRuntimeType classMap fSharpType compileFlags

        let convertToRuntimeType =
            generateJsonValToRuntimeTypeConversion classMap fSharpType compileFlags

        if optional then
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
        | FSharpBool -> CommonExprs.callOpNot (CommonExprs.getNullableHasValue typeof<bool> arg)
        | FSharpInt -> CommonExprs.callOpNot (CommonExprs.getNullableHasValue typeof<int> arg)
        | FSharpDouble -> CommonExprs.callOpNot (CommonExprs.getNullableHasValue typeof<double> arg)
        | _ -> CommonExprs.callOpEquality arg (Expr.Value(null))

    let private generatePropertyCreation
        (classMap: Map<string, ProvidedTypeDefinition>)
        (name: string)
        (optional: bool)
        (fSharpType: FSharpType)
        (arg: Expr)
        (compileFlags: ProviderConfiguration.CompileFlags)
        =
        if optional then
            let isNull = generateIsNullCheck fSharpType arg

            let thenBranch = Expr.NewArray(typeof<string * JsonValue>, [])

            let elseBranch =
                Expr.NewArray(
                    typeof<string * JsonValue>,
                    [ Expr.NewTuple(
                          [ Expr.Value(name)
                            Expr.Application(generateRuntimeTypeToJsonValConversion classMap optional fSharpType compileFlags, arg) ]
                      ) ]
                )

            Expr.IfThenElse(isNull, thenBranch, elseBranch)
        else
            Expr.NewArray(
                typeof<string * JsonValue>,
                [ Expr.NewTuple(
                      [ Expr.Value(name)
                        Expr.Application(generateRuntimeTypeToJsonValConversion classMap optional fSharpType compileFlags, arg) ]
                  ) ]
            )

    let generateCreateInvokeCode
        (nestedClass: bool)
        (classMap: Map<string, ProvidedTypeDefinition>)
        (schemaHashCode: int32)
        (schemaSource: string)
        (properties: FSharpProperty list)
        (compileFlags: ProviderConfiguration.CompileFlags)
        : Expr list -> Expr =
        fun (args: Expr list) ->
            let elementType = typedefof<(string * JsonValue)[]>

            let elements =
                [ for ({ Name = name
                         Optional = optional
                         FSharpType = fSharpType },
                       arg) in List.zip properties args ->
                      generatePropertyCreation classMap name optional fSharpType arg compileFlags ]

            let fields = Expr.NewArray(elementType, elements)

            <@@
                let record =
                    NullableJsonValue(JsonValue.Record(Array.concat ((%%fields): (string * JsonValue)[][])))

                if nestedClass then
                    record
                else
                    let recordSource = record.ToString()

                    let schema = SchemaCache.retrieveSchema schemaHashCode schemaSource
                    let validationErrors = schema.Validate(recordSource)

                    if Seq.isEmpty validationErrors then
                        record
                    else
                        let message =
                            validationErrors
                            |> Seq.map (fun validationError -> validationError.ToString())
                            |> fun msgs -> System.String.Join(", ", msgs) |> sprintf "JSON Schema validation failed: %s"

                        raise (ArgumentException(message, recordSource))
            @@>
