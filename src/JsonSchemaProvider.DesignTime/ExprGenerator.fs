// Copyright (c) 2024 Florian Lorenzen

// Permission is hereby granted, free of charge, to any person
// obtaining a copy of this software and associated documentation
// files (the “Software”), to deal in the Software without
// restriction, including without limitation the rights to use,
// copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the
// Software is furnished to do so, subject to the following
// conditions:

// The above copyright notice and this permission notice shall be
// included in all copies or substantial portions of the Software.

// THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
// OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
// HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
// WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
// OTHER DEALINGS IN THE SOFTWARE.

namespace JsonSchemaProvider.DesignTime

module ExprGenerator =
    open FSharp.Quotations
    open FSharp.Data
    open SchemaConversion
    open TypeLevelConversion
    open JsonSchemaProvider
    open System
    open ProviderImplementation.ProvidedTypes

    let rec private generateJsonValToRuntimeTypeConversion
        (classMap: Map<string, ProvidedTypeDefinition>)
        (fSharpType: FSharpType)
        (compileFlags: CompileFlags)
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

    let rec private generateRuntimeTypeToJsonValConversion
        (classMap: Map<string, ProvidedTypeDefinition>)
        (optional: bool)
        (fSharpType: FSharpType)
        (compileFlags: CompileFlags)
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

    let generatePropertyGetter
        (classMap: Map<string, ProvidedTypeDefinition>)
        { Name = name
          Optional = optional
          FSharpType = fSharpType }
        (compileFlags: CompileFlags)
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
        (compileFlags: CompileFlags)
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
        (compileFlags: CompileFlags)
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
