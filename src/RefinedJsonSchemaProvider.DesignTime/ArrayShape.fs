namespace RefinedJsonSchemaProvider.DesignTime

module ArrayShape =
    open RefinedJsonSchemaProvider
    open SchemaConversion

    // The shapes an array node can have, decided once from its keywords.
    type ArrayShape =
        // minItems > maxItems: no array is valid.
        | InvalidBounds
        // Keywords without a compiled type: a plain list, validated at runtime.
        | UnsupportedKeywords
        // minItems = maxItems = n: n items as nested pairs.
        | ExactLength of n: int
        // minItems only: minItems items as nested pairs, ending in a plain list.
        | MinItemsPrefix of minItems: int
        // minItems < maxItems: minItems items, then the option chain of the remaining maxItems - minItems.
        | MinMaxItems of minItems: int * maxItems: int
        // maxItems = 1 only: an option of one item, the base case of MaxItemsChain.
        | MaxItemsSingle
        // maxItems > 1 only: an option of an item and a tail with maxItems - 1.
        | MaxItemsChain of maxItems: int
        // No size keywords: a plain list.
        | Unbounded
        // uniqueItems only: the plain list of Unbounded, and ToJson drops duplicates.
        | UniqueUnbounded
        // uniqueItems and maxItems: the option chain of the maxItems shapes, and ToJson drops duplicates.
        | UniqueMaxItems of maxItems: int

    // Sorts an array node into its ArrayShape.
    let classifyArrayShape
        (compileFlags: ProviderConfiguration.CompileFlags)
        (arrayKeywords: JsonArray.Keywords)
        : ArrayShape =
        match arrayKeywords.specific with
        | { MinItems = Some minItems; MaxItems = Some maxItems } when minItems > maxItems ->
            InvalidBounds

        // Also when refinedTypes is off or the node has a keyword outside knownKeywordsFor (contains, enum, ...).
        | keys when not compileFlags.RefinedTypes
                    || not arrayKeywords.common.CanBeCompiled
                    || not keys.AllowAdditionalItems
                    || keys.HasAdditionalItemsSchema ->
            UnsupportedKeywords

        // Removing duplicates can push the array below minItems, so that combination is validated instead.
        // TODO: minItems 1 is safe (a non-empty array stays non-empty) and could keep its shape.
        | { UniqueItems = true; MinItems = Some _ } ->
            UnsupportedKeywords

        // Dropping duplicates only shortens the array, so maxItems still holds afterwards.
        | { UniqueItems = true; MaxItems = Some maxItems } ->
            UniqueMaxItems maxItems

        | { UniqueItems = true } ->
            UniqueUnbounded

        | { MinItems = Some n; MaxItems = Some n2 } when n = n2 ->
            ExactLength n

        | { MinItems = Some minItems; MaxItems = Some maxItems } ->
            MinMaxItems(minItems, maxItems)

        | { MinItems = Some minItems } ->
            MinItemsPrefix minItems

        | { MaxItems = Some 1 } ->
            MaxItemsSingle

        | { MaxItems = Some maxItems } ->
            MaxItemsChain maxItems

        | _ ->
            Unbounded
