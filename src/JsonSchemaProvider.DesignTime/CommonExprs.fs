namespace JsonSchemaProvider.DesignTime

[<RequireQualifiedAccess>]
module internal CommonExprs =
    open FSharp.Quotations
    open FSharp.Quotations.Patterns
    open FSharp.Reflection
    open FSharp.Data
    open JsonSchemaProvider
    open System
    open System.Reflection

    /// <summary>Fails with a generic error. Used for match branches that reflection guarantees can never be hit.</summary>
    /// <returns>Never returns normally; always throws.</returns>
    let private cannotHappen () : 'T = failwith "Cannot happen."

    /// <summary>The FSharp.Core assembly, used below to look up the Array and List module types by reflection.</summary>
    let private fSharpCore = typeof<List<_>>.Assembly

    /// <summary>The reflection Type for FSharp.Core's internal ArrayModule (backs the Array module functions).</summary>
    let private arrayModuleType =
        fSharpCore.GetTypes() |> Array.find (fun ty -> ty.Name = "ArrayModule")

    /// <summary>The reflection Type for FSharp.Core's internal ListModule (backs the List module functions).</summary>
    let private listModuleType =
        fSharpCore.GetTypes() |> Array.find (fun ty -> ty.Name = "ListModule")

    /// <summary>Looks up a named property on 'elementType option (e.g. "IsSome", "Value") by reflection.</summary>
    /// <param name="elementType">The option's element type.</param>
    /// <param name="propertyName">The property name to look up, e.g. "IsSome" or "Value".</param>
    /// <returns>The PropertyInfo for that property.</returns>
    let private optionPropertyInfo (elementType: Type) (propertyName: string) : PropertyInfo =
        let optionType = (typedefof<_ option>).MakeGenericType([| elementType |])
        let properties = optionType.GetProperties()
        let pi = properties |> Array.filter (fun pi -> pi.Name = propertyName) |> Array.head
        pi

    /// <summary>The IsSome property of 'elementType option.</summary>
    /// <param name="elementType">The option's element type.</param>
    /// <returns>The PropertyInfo for IsSome.</returns>
    let private optionIsSomePropertyInfo (elementType: Type) : PropertyInfo = optionPropertyInfo elementType "IsSome"

    /// <summary>The Value property of 'elementType option.</summary>
    /// <param name="elementType">The option's element type.</param>
    /// <returns>The PropertyInfo for Value.</returns>
    let private optionValuePropertyInfo (elementType: Type) : PropertyInfo = optionPropertyInfo elementType "Value"

    /// <summary>Looks up a named property on Nullable&lt;'elementType&gt; (e.g. "HasValue", "Value") by reflection.</summary>
    /// <param name="elementType">The nullable's underlying value type.</param>
    /// <param name="propertyName">The property name to look up, e.g. "HasValue" or "Value".</param>
    /// <returns>The PropertyInfo for that property.</returns>
    let private nullablePropertyInfo (elementType: Type) (propertyName: string) : PropertyInfo =
        let nullableType = (typedefof<Nullable<_>>).MakeGenericType([| elementType |])
        let properties = nullableType.GetProperties()
        let pi = properties |> Array.filter (fun pi -> pi.Name = propertyName) |> Array.head
        pi

    /// <summary>The HasValue property of Nullable&lt;'elementType&gt;.</summary>
    /// <param name="elementType">The nullable's underlying value type.</param>
    /// <returns>The PropertyInfo for HasValue.</returns>
    let private nullableHasValuePropertyInfo (elementType: Type) : PropertyInfo =
        nullablePropertyInfo elementType "HasValue"

    /// <summary>The Value property of Nullable&lt;'elementType&gt;.</summary>
    /// <param name="elementType">The nullable's underlying value type.</param>
    /// <returns>The PropertyInfo for Value.</returns>
    let private nullableValuePropertyInfo (elementType: Type) : PropertyInfo =
        nullablePropertyInfo elementType "Value"

    /// <summary>The Some union case of 'elementType option.</summary>
    /// <param name="elementType">The option's element type.</param>
    /// <returns>The UnionCaseInfo for Some.</returns>
    let private optionSomeUnionCaseInfo (elementType: Type) : UnionCaseInfo =
        FSharpType.GetUnionCases(typedefof<_ option>.MakeGenericType(elementType))[1]

    /// <summary>The None union case of 'elementType option.</summary>
    /// <param name="elementType">The option's element type.</param>
    /// <returns>The UnionCaseInfo for None.</returns>
    let private optionNoneUnionCaseInfo (elementType: Type) : UnionCaseInfo =
        FSharpType.GetUnionCases(typedefof<_ option>.MakeGenericType(elementType))[0]

    /// <summary>The Array union case of JsonValue.</summary>
    /// <returns>The UnionCaseInfo for JsonValue.Array.</returns>
    let private jsonValueArrayUnionCaseInfo: UnionCaseInfo =
        FSharpType.GetUnionCases(typeof<JsonValue>)
        |> Array.filter (fun uc -> uc.Name = "Array")
        |> Array.head

    /// <summary>The JsonVal property of NullableJsonValue, found by pattern-matching a sample quotation.</summary>
    /// <returns>The PropertyInfo for JsonVal.</returns>
    let private nullableJsonValueJsonValPropertyInfo =
        match <@@ NullableJsonValue(JsonValue.Boolean(true)).JsonVal @@> with
        | PropertyGet(_, pi, _) -> pi
        | _ -> cannotHappen ()

    /// <summary>The MethodInfo for the `not` operator, found by pattern-matching a sample quotation.</summary>
    /// <returns>The MethodInfo for `not`.</returns>
    let private opNotMethodInfo =
        match <@@ not true @@> with
        | Call(_, mi, _) -> mi
        | _ -> cannotHappen ()

    /// <summary>The MethodInfo for JsonValue's string indexer (`jsonValue.["x"]`), found via a sample quotation.</summary>
    /// <returns>The MethodInfo for the indexer.</returns>
    let private jsonValueItemMethodInfo =
        match <@@ JsonValue.Record(Array.empty)["x"] @@> with
        | Call(_, mi, _) -> mi
        | _ -> cannotHappen ()

    /// <summary>The MethodInfo for JsonValue.TryGetProperty, found via a sample quotation.</summary>
    /// <returns>The MethodInfo for TryGetProperty.</returns>
    let private jsonValueTryGetPropertyMethodInfo =
        match <@@ JsonValue.Record(Array.empty).TryGetProperty("x") @@> with
        | Call(_, mi, _) -> mi
        | _ -> cannotHappen ()

    /// <summary>The MethodInfo for JsonValue.AsArray, found via a sample quotation.</summary>
    /// <returns>The MethodInfo for AsArray.</returns>
    let private jsonValueAsArrayMethodInfo =
        match <@@ JsonValue.Array(Array.empty).AsArray() @@> with
        | Call(_, mi, _) -> mi
        | _ -> cannotHappen ()

    /// <summary>Builds the generic Array.map MethodInfo closed over the given source/target element types.</summary>
    /// <param name="fromType">The source array's element type.</param>
    /// <param name="toType">The target array's element type.</param>
    /// <returns>The closed MethodInfo for Array.map&lt;fromType, toType&gt;.</returns>
    let private arrayMapMethodInfo (fromType: Type) (toType: Type) : MethodInfo =
        arrayModuleType.GetMethods()
        |> Array.find (fun methodInfo -> methodInfo.Name = "Map")
        |> fun genericMethodInfo -> genericMethodInfo.MakeGenericMethod(fromType, toType)

    /// <summary>Builds the generic Array.ofList MethodInfo closed over the given element type.</summary>
    /// <param name="elementType">The list/array element type.</param>
    /// <returns>The closed MethodInfo for Array.ofList.</returns>
    let private arrayOfListMethodInfo (elementType: Type) : MethodInfo =
        arrayModuleType.GetMethods()
        |> Array.find (fun methodInfo -> methodInfo.Name = "OfList")
        |> fun genericMethodInfo -> genericMethodInfo.MakeGenericMethod(elementType)

    /// <summary>Builds the generic List.map MethodInfo closed over the given source/target element types.</summary>
    /// <param name="fromType">The source list's element type.</param>
    /// <param name="toType">The target list's element type.</param>
    /// <returns>The closed MethodInfo for List.map&lt;fromType, toType&gt;.</returns>
    let private listMapMethodInfo (fromType: Type) (toType: Type) : MethodInfo =
        listModuleType.GetMethods()
        |> Array.find (fun methodInfo -> methodInfo.Name = "Map")
        |> fun genericMethodInfo -> genericMethodInfo.MakeGenericMethod(fromType, toType)

    /// <summary>Builds the generic List.ofArray MethodInfo closed over the given element type.</summary>
    /// <param name="elementType">The list/array element type.</param>
    /// <returns>The closed MethodInfo for List.ofArray.</returns>
    let private listOfArryMethodInfo (elementType: Type) : MethodInfo =
        listModuleType.GetMethods()
        |> Array.find (fun methodInfo -> methodInfo.Name = "OfArray")
        |> fun genericMethodInfo -> genericMethodInfo.MakeGenericMethod(elementType)

    /// <summary>Builds the generic Array.get MethodInfo closed over the given element type.</summary>
    /// <param name="elementType">The array's element type.</param>
    /// <returns>The closed MethodInfo for Array.get.</returns>
    let private arrayGetMethodInfo (elementType: Type) : MethodInfo =
        arrayModuleType.GetMethods()
        |> Array.find (fun methodInfo -> methodInfo.Name = "Get")
        |> fun genericMethodInfo -> genericMethodInfo.MakeGenericMethod elementType

    /// <summary>Builds the generic Array.skip MethodInfo closed over the given element type.</summary>
    /// <param name="elementType">The array's element type.</param>
    /// <returns>The closed MethodInfo for Array.skip.</returns>
    let private arraySkipMethodInfo (elementType: Type) : MethodInfo =
        arrayModuleType.GetMethods()
        |> Array.find (fun methodInfo -> methodInfo.Name = "Skip")
        |> fun genericMethodInfo -> genericMethodInfo.MakeGenericMethod elementType

    /// <summary>Builds the generic Array.append MethodInfo closed over the given element type.</summary>
    /// <param name="elementType">The array's element type.</param>
    /// <returns>The closed MethodInfo for Array.append.</returns>
    let private arrayAppendMethodInfo (elementType: Type) : MethodInfo =
        arrayModuleType.GetMethods()
        |> Array.find (fun methodInfo -> methodInfo.Name = "Append")
        |> fun genericMethodInfo -> genericMethodInfo.MakeGenericMethod elementType

    /// <summary>The reflection Type for ChoiceAccessors, the module holding getChoice1Of2/getChoice2Of2.</summary>
    let private choiceAccessorsType =
        typeof<NullableJsonValue>.Assembly.GetTypes()
        |> Array.find (fun ty -> ty.Name = "ChoiceAccessors")

    /// <summary>Builds the generic getChoice1Of2 MethodInfo closed over the given head/tail types.</summary>
    /// <param name="headType">The type of the choice's first case.</param>
    /// <param name="tailType">The type of the choice's remaining cases.</param>
    /// <returns>The closed MethodInfo for getChoice1Of2.</returns>
    let private getChoice1Of2MethodInfo (headType: Type) (tailType: Type) : MethodInfo =
        choiceAccessorsType.GetMethod("getChoice1Of2").MakeGenericMethod(headType, tailType)

    /// <summary>Builds the generic getChoice2Of2 MethodInfo closed over the given head/tail types.</summary>
    /// <param name="headType">The type of the choice's first case.</param>
    /// <param name="tailType">The type of the choice's remaining cases.</param>
    /// <returns>The closed MethodInfo for getChoice2Of2.</returns>
    let private getChoice2Of2MethodInfo (headType: Type) (tailType: Type) : MethodInfo =
        choiceAccessorsType.GetMethod("getChoice2Of2").MakeGenericMethod(headType, tailType)

    /// <summary>The MethodInfo for the `(=)` operator, found by pattern-matching a sample quotation.</summary>
    /// <returns>The MethodInfo for `(=)`.</returns>
    let private opEqualityMethodInfo =
        match <@@ (=) @@> with
        | Lambda(_, Lambda(_, Call(_, mi, _))) -> mi
        | _ -> cannotHappen ()

    /// <summary>Builds an Expr that evaluates `fun v -> buildBody v`, where `v` is a fresh Var of `paramType`.</summary>
    /// <param name="namePrefix">A readable prefix for the fresh variable's name (a GUID is appended to keep it unique).</param>
    /// <param name="paramType">The lambda parameter's type.</param>
    /// <param name="buildBody">Given the fresh parameter Var, builds the lambda body expression.</param>
    /// <returns>An Expr evaluating to the lambda.</returns>
    let freshLambda (namePrefix: string) (paramType: Type) (buildBody: Var -> Expr) : Expr =
        let v = Var($"{namePrefix}{Guid.NewGuid()}", paramType)
        Expr.Lambda(v, buildBody v)

    /// <summary>Builds an Expr that evaluates `option.IsSome`.</summary>
    /// <param name="elementType">The option's element type.</param>
    /// <param name="option">The expression producing the option value.</param>
    /// <returns>An Expr evaluating to a bool.</returns>
    let getOptionIsSome (elementType: Type) (option: Expr) : Expr =
        let pi = optionIsSomePropertyInfo elementType
        let propertyGet = Expr.PropertyGet(pi, [ option ])
        propertyGet

    /// <summary>Builds an Expr that evaluates `option.Value`.</summary>
    /// <param name="elementType">The option's element type.</param>
    /// <param name="option">The expression producing the option value.</param>
    /// <returns>An Expr evaluating to the unwrapped 'elementType.</returns>
    let getOptionValue (elementType: Type) (option: Expr) : Expr =
        Expr.PropertyGet(option, optionValuePropertyInfo elementType)

    /// <summary>Builds an Expr that evaluates `nullable.HasValue`.</summary>
    /// <param name="elementType">The nullable's underlying value type.</param>
    /// <param name="nullable">The expression producing the nullable value.</param>
    /// <returns>An Expr evaluating to a bool.</returns>
    let getNullableHasValue (elementType: Type) (nullable: Expr) : Expr =
        Expr.PropertyGet(nullable, nullableHasValuePropertyInfo elementType)

    /// <summary>Builds an Expr that evaluates `nullable.Value`.</summary>
    /// <param name="elementType">The nullable's underlying value type.</param>
    /// <param name="nullable">The expression producing the nullable value.</param>
    /// <returns>An Expr evaluating to the unwrapped 'elementType.</returns>
    let getNullableValue (elementType: Type) (nullable: Expr) : Expr =
        Expr.PropertyGet(nullable, nullableValuePropertyInfo elementType)

    /// <summary>Builds an Expr that evaluates `nullableJsonValue.JsonVal`.</summary>
    /// <param name="nullableJsonValue">The expression producing the NullableJsonValue.</param>
    /// <returns>An Expr evaluating to the underlying JsonValue.</returns>
    let getNullableJsonValueJsonVal (nullableJsonValue: Expr) : Expr =
        Expr.PropertyGet(nullableJsonValue, nullableJsonValueJsonValPropertyInfo)

    /// <summary>Builds an Expr that evaluates `Some value`, typed as 'elementType option.</summary>
    /// <param name="elementType">The option's element type.</param>
    /// <param name="value">The expression to wrap in Some.</param>
    /// <returns>An Expr evaluating to `Some value`.</returns>
    let newOptionSome (elementType: Type) (value: Expr) : Expr =
        Expr.NewUnionCase(optionSomeUnionCaseInfo elementType, [ value ])

    /// <summary>Builds an Expr that evaluates `None`, typed as 'elementType option.</summary>
    /// <param name="elementType">The option's element type.</param>
    /// <returns>An Expr evaluating to `None`.</returns>
    let newOptionNone (elementType: Type) : Expr =
        Expr.NewUnionCase(optionNoneUnionCaseInfo elementType, [])

    /// <summary>Builds an Expr that evaluates `JsonValue.Array value`.</summary>
    /// <param name="value">The expression producing the underlying JsonValue array.</param>
    /// <returns>An Expr evaluating to a JsonValue.Array case.</returns>
    let newJsonValueArray (value: Expr) : Expr =
        Expr.NewUnionCase(jsonValueArrayUnionCaseInfo, [ value ])

    /// <summary>The empty JSON array, `JsonValue.Array [||]`.</summary>
    let emptyJsonValueArray: Expr =
        newJsonValueArray (Expr.NewArray(typeof<JsonValue>, []))

    /// <summary>Builds an Expr that evaluates `jsonValue.[propertyName]`.</summary>
    /// <param name="jsonValue">The expression producing the JsonValue record.</param>
    /// <param name="propertyName">The property name to index by.</param>
    /// <returns>An Expr evaluating to the JsonValue at that property.</returns>
    let callJsonValueItem (jsonValue: Expr) (propertyName: string) : Expr =
        Expr.Call(jsonValueItemMethodInfo, [ jsonValue; Expr.Value(propertyName) ])

    /// <summary>Builds an Expr that evaluates `jsonValue.TryGetProperty(propertyName)`.</summary>
    /// <param name="jsonValue">The expression producing the JsonValue record.</param>
    /// <param name="propertyName">The property name to look up.</param>
    /// <returns>An Expr evaluating to a JsonValue option (None if the property is absent).</returns>
    let callJsonValueTryGetPropertyName (jsonValue: Expr) (propertyName: string) : Expr =
        Expr.Call(jsonValueTryGetPropertyMethodInfo, [ jsonValue; Expr.Value(propertyName) ])

    /// <summary>Builds an Expr that evaluates `jsonValue.AsArray()`.</summary>
    /// <param name="jsonValue">The expression producing the JsonValue.</param>
    /// <returns>An Expr evaluating to a JsonValue array.</returns>
    let callJsonValueAsArray (jsonValue: Expr) : Expr =
        Expr.Call(jsonValueAsArrayMethodInfo, [ jsonValue ])

    /// <summary>Builds an Expr that evaluates `Array.map mapping array`, mapping fromType elements to toType.</summary>
    /// <param name="mapping">The mapping function expression, of type `fromType -> toType`.</param>
    /// <param name="array">The source array expression, of type `fromType array`.</param>
    /// <param name="fromType">The source element type.</param>
    /// <param name="toType">The target element type.</param>
    /// <returns>An Expr evaluating to a `toType array`.</returns>
    let callArrayMap (mapping: Expr) (array: Expr) (fromType: Type) (toType: Type) : Expr =
        Expr.Call(arrayMapMethodInfo fromType toType, [ mapping; array ])

    /// <summary>Builds an Expr that evaluates `Array.ofList list`.</summary>
    /// <param name="list">The source list expression.</param>
    /// <param name="elementType">The list's element type.</param>
    /// <returns>An Expr evaluating to the equivalent array.</returns>
    let callArrayOfList (list: Expr) (elementType: Type) : Expr =
        Expr.Call(arrayOfListMethodInfo elementType, [ list ])

    /// <summary>Builds an Expr that evaluates `List.ofArray array`.</summary>
    /// <param name="array">The source array expression.</param>
    /// <param name="elementType">The array's element type.</param>
    /// <returns>An Expr evaluating to the equivalent list.</returns>
    let callListOfArray (array: Expr) (elementType: Type) : Expr =
        Expr.Call(listOfArryMethodInfo elementType, [ array ])

    /// <summary>Builds an Expr that evaluates `List.map mapping list`, mapping fromType elements to toType.</summary>
    /// <param name="mapping">The mapping function expression, of type `fromType -> toType`.</param>
    /// <param name="list">The source list expression, of type `fromType list`.</param>
    /// <param name="fromType">The source element type.</param>
    /// <param name="toType">The target element type.</param>
    /// <returns>An Expr evaluating to a `toType list`.</returns>
    let callListMap (mapping: Expr) (list: Expr) (fromType: Type) (toType: Type) : Expr =
        Expr.Call(listMapMethodInfo fromType toType, [ mapping; list ])

    /// <summary>Builds an Expr that evaluates `x = y`.</summary>
    /// <param name="x">The left-hand side expression.</param>
    /// <param name="y">The right-hand side expression.</param>
    /// <returns>An Expr evaluating to a bool.</returns>
    let callOpEquality (x: Expr) (y: Expr) : Expr =
        Expr.Call(opEqualityMethodInfo, [ x; y ])

    /// <summary>Builds an Expr that evaluates `not value`.</summary>
    /// <param name="value">The bool expression to negate.</param>
    /// <returns>An Expr evaluating to the negated bool.</returns>
    let callOpNot (value: Expr) : Expr = Expr.Call(opNotMethodInfo, [ value ])

    /// <summary>Builds an Expr that evaluates `Array.get array index` (i.e. `array.[index]`) for a literal index.</summary>
    /// <param name="index">The literal index to read.</param>
    /// <param name="array">The array expression to index into.</param>
    /// <param name="elementType">The array's element type.</param>
    /// <returns>An Expr evaluating to the element at `index`.</returns>
    let callArrayGet (index: int) (array: Expr) (elementType: Type) : Expr =
        Expr.Call(arrayGetMethodInfo elementType, [ array; Expr.Value index ])

    /// <summary>Builds an Expr that evaluates `Array.skip count array` for a literal count.</summary>
    /// <param name="count">The number of leading elements to drop.</param>
    /// <param name="array">The array expression to skip into.</param>
    /// <param name="elementType">The array's element type.</param>
    /// <returns>An Expr evaluating to the remaining array after skipping `count` elements.</returns>
    let callArraySkip (count: int) (array: Expr) (elementType: Type) : Expr =
        Expr.Call(arraySkipMethodInfo elementType, [ Expr.Value count; array ])

    /// <summary>Builds an Expr that evaluates `Array.append array1 array2`.</summary>
    /// <param name="array1">The first array expression.</param>
    /// <param name="array2">The second array expression.</param>
    /// <param name="elementType">The arrays' element type.</param>
    /// <returns>An Expr evaluating to the concatenation of `array1` and `array2`.</returns>
    let callArrayAppend (array1: Expr) (array2: Expr) (elementType: Type) : Expr =
        Expr.Call(arrayAppendMethodInfo elementType, [ array1; array2 ])

    /// <summary>Builds an Expr that evaluates `getChoice1Of2 choice`, extracting the first case of a two-way choice.</summary>
    /// <param name="headType">The type of the choice's first case.</param>
    /// <param name="tailType">The type of the choice's remaining cases.</param>
    /// <param name="choice">The expression producing the choice value.</param>
    /// <returns>An Expr evaluating to the unwrapped `headType` value.</returns>
    let callGetChoice1Of2 (headType: Type) (tailType: Type) (choice: Expr) : Expr =
        Expr.Call(getChoice1Of2MethodInfo headType tailType, [ choice ])

    /// <summary>Builds an Expr that evaluates `getChoice2Of2 choice`, extracting the second case of a two-way choice.</summary>
    /// <param name="headType">The type of the choice's first case.</param>
    /// <param name="tailType">The type of the choice's remaining cases.</param>
    /// <param name="choice">The expression producing the choice value.</param>
    /// <returns>An Expr evaluating to the unwrapped `tailType` value.</returns>
    let callGetChoice2Of2 (headType: Type) (tailType: Type) (choice: Expr) : Expr =
        Expr.Call(getChoice2Of2MethodInfo headType tailType, [ choice ])
