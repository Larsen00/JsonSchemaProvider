namespace JsonSchemaProvider.DesignTime

module IntegerShape =
    open JsonSchemaProvider

    // The shapes an integer can have depending on its keywords; each picks its F# type in NodeConversions.fs.
    type IntegerShape =
        // Keywords with no compile-time encoding here (see classifyIntegerShape): int, runtime-validated.
        | UnsupportedIntegerKeywords of JsonNumber.Keywords
        // Smallest allowed integer is exactly 0, from minimum 0 or exclusiveMinimum -1: uint32, any other keywords validated.
        | NonNegativeInteger of JsonNumber.Keywords
        // No numeric keywords at all: plain int.
        | AnyInteger of JsonNumber.Keywords

    // The smallest integer minimum and exclusiveMinimum together allow, e.g. exclusiveMinimum -1 gives 0.
    let private smallestAllowedInteger (specific: JsonNumber.Specific) : float option =
        let fromMinimum = specific.Minimum |> Option.map ceil
        let fromExclusiveMinimum = specific.ExclusiveMinimum |> Option.map (fun bound -> floor bound + 1.0)
        match List.choose id [ fromMinimum; fromExclusiveMinimum ] with
        | [] -> None
        | bounds -> Some(List.max bounds)

    // Helper function that turns a set of integer keywords into an IntegerShape value.
    let classifyIntegerShape
        (compileFlags: ProviderConfiguration.CompileFlags)
        (keywords: JsonNumber.Keywords)
        : IntegerShape =
        match keywords.specific with
        | { Minimum = None; ExclusiveMinimum = None; Maximum = None; ExclusiveMaximum = None; MultipleOf = None } ->
            AnyInteger keywords

        | _ when compileFlags.IgnoreSpecificKeywords -> UnsupportedIntegerKeywords keywords

        // maximum/multipleOf alongside are not encoded, so CanBeCompiled is false and they are validated at runtime.
        | specific when smallestAllowedInteger specific = Some 0.0 -> NonNegativeInteger keywords

        | _ -> UnsupportedIntegerKeywords keywords
