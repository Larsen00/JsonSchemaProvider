namespace JsonSchemaProvider.DesignTime

module ArrayShape =
    open JsonSchemaProvider
    open SchemaConversion

    // An array have multiple shapes depending on its keywords, we are gonna use this type to help diffrent functions agree on the shapes.
    type ArrayShape =
        // minItems > maxItems - not a valid schema.
        | InvalidBounds of JsonSchemaType * JsonArray.Keywords
        // Keywords with no compile-time encoding here (see classifyArrayShape): bare list, runtime-validated.
        | UnsupportedKeywords of JsonSchemaType * JsonArray.Keywords
        // minItems = maxItems = n: exact n-tuple, nothing optional.
        | ExactLength of JsonSchemaType * JsonArray.Keywords * n: int
        // minItems mandatory prefix, maxItems optional tail bound.
        | MinItemsPrefix of JsonSchemaType * JsonArray.Keywords * minItems: int * maxItems: int option
        // maxItems = 1, no minItems floor: option<inner>. Base case of the MaxItemsChain recursion.
        | MaxItemsSingle of JsonSchemaType * JsonArray.Keywords
        // maxItems > 1, no minItems floor: option<inner * tail>, recurses down to MaxItemsSingle.
        | MaxItemsChain of JsonSchemaType * JsonArray.Keywords * maxItems: int
        // No minItems/maxItems bound at all: plain list, already free (real `::`/List.isEmpty).
        | Unbounded of JsonSchemaType * JsonArray.Keywords
        // uniqueItems, no size bound: same plain list as Unbounded, ToJson drops duplicates.
        | UniqueUnbounded of JsonSchemaType * JsonArray.Keywords
        // uniqueItems + maxItems, no minItems: same option chain as MaxItemsSingle/MaxItemsChain, ToJson drops duplicates.
        | UniqueMaxItems of JsonSchemaType * JsonArray.Keywords * maxItems: int

    // True when equal JSON items always give equal JsonValues, so Array.distinct finds every duplicate. Objects can differ in property order.
    let rec hasStructuralEquality (schemaType: JsonSchemaType) : bool =
        match schemaType with
        | JsonBoolean _
        | JsonInteger _
        | JsonNumber _
        | JsonString _ -> true
        | JsonObject _ -> false
        | JsonArray(innerType, _) -> hasStructuralEquality innerType
        | JsonOneOf(_, head, tail) -> List.forall hasStructuralEquality (head :: tail)

    // Helper function that turn a set of array keywords into an ArrayShape value.
    let classifyArrayShape
        (compileFlags: ProviderConfiguration.CompileFlags)
        (innerType: JsonSchemaType)
        (arrayKeywords: JsonArray.Keywords)
        : ArrayShape =
        match arrayKeywords.specific with
        | { MinItems = Some minItems; MaxItems = Some maxItems } when minItems > maxItems ->
            InvalidBounds(innerType, arrayKeywords)

        | keys when compileFlags.IgnoreSpecificKeywords || not keys.AllowAdditionalItems || keys.HasAdditionalItemsSchema ->
            UnsupportedKeywords(innerType, arrayKeywords)

        // Removing duplicates can push the array below minItems, so that combination is validated instead.
        // TODO: minItems 1 is safe (a non-empty array stays non-empty) and could keep its shape.
        | { UniqueItems = true; MinItems = Some _ } ->
            UnsupportedKeywords(innerType, arrayKeywords)

        // Dropping duplicates only shortens the array, so maxItems still holds afterwards.
        | { UniqueItems = true; MaxItems = Some maxItems } when hasStructuralEquality innerType ->
            UniqueMaxItems(innerType, arrayKeywords, maxItems)

        | { UniqueItems = true } when hasStructuralEquality innerType ->
            UniqueUnbounded(innerType, arrayKeywords)

        | { MinItems = Some n; MaxItems = Some n2 } when n = n2 ->
            ExactLength(innerType, arrayKeywords, n)

        | { MinItems = Some minItems; MaxItems = maxItems } ->
            MinItemsPrefix(innerType, arrayKeywords, minItems, maxItems)

        | { MaxItems = Some 1 } ->
            MaxItemsSingle(innerType, arrayKeywords)

        | { MaxItems = Some maxItems } ->
            MaxItemsChain(innerType, arrayKeywords, maxItems)

        | _ ->
            Unbounded(innerType, arrayKeywords)
