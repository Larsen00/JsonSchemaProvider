namespace JsonSchemaProvider.DesignTime

module ArrayShape =
    open JsonSchemaProvider
    open SchemaConversion

    // An array have multiple shapes depending on its keywords, we are gonna use this type to help diffrent functions agree on the shapes.
    type ArrayShape =
        // minItems > maxItems - not a valid schema.
        | InvalidBounds of FSharpType * JsonArray.Keywords
        // UniqueItems / additionalItems keywords, or CompileFlags.IgnoreSpecificKeywords, force a
        // runtime-only fallback even though no size bound applies structurally. Same compile-time
        // encoding as Unbounded (bare list) - only FullyCompilable differs.
        | UnsupportedKeywords of FSharpType * JsonArray.Keywords
        // minItems = maxItems = n: exact n-tuple, nothing optional.
        | ExactLength of FSharpType * JsonArray.Keywords * n: int
        // minItems mandatory prefix, maxItems optional tail bound.
        | MinItemsPrefix of FSharpType * JsonArray.Keywords * minItems: int * maxItems: int option
        // maxItems = 1, no minItems floor: option<inner>. Base case of the MaxItemsChain recursion.
        | MaxItemsSingle of FSharpType * JsonArray.Keywords
        // maxItems > 1, no minItems floor: option<inner * tail>, recurses down to MaxItemsSingle.
        | MaxItemsChain of FSharpType * JsonArray.Keywords * maxItems: int
        // No minItems/maxItems bound at all: plain list, already free (real `::`/List.isEmpty).
        | Unbounded of FSharpType * JsonArray.Keywords

    // Helper function that turn a set of array keywords into an ArrayShape value.
    let classifyArrayShape
        (compileFlags: ProviderConfiguration.CompileFlags)
        (innerType: FSharpType)
        (arrayKeywords: JsonArray.Keywords)
        : ArrayShape =
        match arrayKeywords.specific with
        | { MinItems = Some minItems; MaxItems = Some maxItems } when minItems > maxItems ->
            InvalidBounds(innerType, arrayKeywords)

        | keys when compileFlags.IgnoreSpecificKeywords || keys.UniqueItems || not keys.AllowAdditionalItems || keys.HasAdditionalItemsSchema ->
            UnsupportedKeywords(innerType, arrayKeywords)

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
