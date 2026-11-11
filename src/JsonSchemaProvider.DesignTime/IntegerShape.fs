namespace JsonSchemaProvider.DesignTime

module IntegerShape =
    open System
    open JsonSchemaProvider

    // The F# integer types an integer node can map to. Qualified, so the names don't hide System.Int32 etc.
    [<RequireQualifiedAccess>]
    type IntegerType =
        | Int8
        | UInt8
        | Int16
        | UInt16
        | Int32
        | UInt32
        | Int64
        | UInt64

    // Smallest and largest term of each integer type.
    let private rangeOf (integerType: IntegerType) : decimal * decimal =
        match integerType with
        | IntegerType.Int8 -> decimal SByte.MinValue, decimal SByte.MaxValue
        | IntegerType.UInt8 -> decimal Byte.MinValue, decimal Byte.MaxValue
        | IntegerType.Int16 -> decimal Int16.MinValue, decimal Int16.MaxValue
        | IntegerType.UInt16 -> decimal UInt16.MinValue, decimal UInt16.MaxValue
        | IntegerType.Int32 -> decimal Int32.MinValue, decimal Int32.MaxValue
        | IntegerType.UInt32 -> decimal UInt32.MinValue, decimal UInt32.MaxValue
        | IntegerType.Int64 -> decimal Int64.MinValue, decimal Int64.MaxValue
        | IntegerType.UInt64 -> decimal UInt64.MinValue, decimal UInt64.MaxValue

    // The shapes an integer can have depending on its keywords.
    type IntegerShape =
        // A lower bound above an upper bound: no integer is valid, so the provider reports a compile error.
        | InvalidIntegerBounds of JsonNumber.Keywords
        // ignoreSpecificKeywords with bounds present: int, runtime-validated.
        | UnsupportedIntegerKeywords of JsonNumber.Keywords
        // The integer type picked from the bounds; validated at runtime unless the type alone enforces them.
        | SizedInteger of JsonNumber.Keywords * IntegerType * typeEnforcesBounds: bool

    // Each lower bound as the smallest integer it allows, e.g. exclusiveMinimum -1 gives 0.
    let private lowerBounds (specific: JsonNumber.Specific) : decimal list =
        List.choose id [ specific.Minimum |> Option.map ceil; specific.ExclusiveMinimum |> Option.map (fun bound -> floor bound + 1m) ]

    // Each upper bound as the largest integer it allows, e.g. exclusiveMaximum 256 gives 255.
    let private upperBounds (specific: JsonNumber.Specific) : decimal list =
        List.choose id [ specific.Maximum |> Option.map floor; specific.ExclusiveMaximum |> Option.map (fun bound -> ceil bound - 1m) ]

    let private tryMin (bounds: decimal list) = if List.isEmpty bounds then None else Some(List.min bounds)
    let private tryMax (bounds: decimal list) = if List.isEmpty bounds then None else Some(List.max bounds)

    let private allFitIn (integerType: IntegerType) (bounds: decimal list) : bool =
        let typeMin, typeMax = rangeOf integerType
        List.forall (fun bound -> typeMin <= bound && bound <= typeMax) bounds

    // The smallest type every bound fits in: unsigned when the loosest lower bound is >= 0, else signed.
    // With both a lower and an upper bound every size is a candidate; with only one the smallest is 32 bits,
    // since the open side would otherwise exclude valid numbers just above a small type's range.
    let private chooseIntegerType (lowers: decimal list) (uppers: decimal list) : IntegerType =
        let unsigned =
            match tryMin lowers with
            | Some lower -> lower >= 0m
            | None -> false

        let ladder =
            if unsigned then
                [ IntegerType.UInt8; IntegerType.UInt16; IntegerType.UInt32; IntegerType.UInt64 ]
            else
                [ IntegerType.Int8; IntegerType.Int16; IntegerType.Int32; IntegerType.Int64 ]

        let bothSides = not (List.isEmpty lowers || List.isEmpty uppers)
        let candidates = if bothSides then ladder else List.skip 2 ladder

        // The 64-bit type is the fallback when even it can't hold a bound; values beyond it throw like int's do.
        candidates
        |> List.tryFind (fun integerType -> allFitIn integerType (lowers @ uppers))
        |> Option.defaultValue (List.last candidates)

    // Helper function that turns a set of integer keywords into an IntegerShape value.
    // The loosest bounds pick the type, the strictest are validated (e.g. minimum 0, maximum 11 and exclusiveMaximum 256 give byte).
    let classifyIntegerShape
        (compileFlags: ProviderConfiguration.CompileFlags)
        (keywords: JsonNumber.Keywords)
        : IntegerShape =
        let lowers = lowerBounds keywords.specific
        let uppers = upperBounds keywords.specific

        match lowers, uppers with
        | [], [] -> SizedInteger(keywords, IntegerType.Int32, true)

        // Checked before the flag, like minItems > maxItems for arrays.
        | _ :: _, _ :: _ when List.max lowers > List.min uppers -> InvalidIntegerBounds keywords

        | _ when compileFlags.IgnoreSpecificKeywords -> UnsupportedIntegerKeywords keywords

        | _ ->
            let integerType = chooseIntegerType lowers uppers
            let typeMin, typeMax = rangeOf integerType
            let enforced (strictBound: decimal option) (typeLimit: decimal) = Option.forall ((=) typeLimit) strictBound
            SizedInteger(keywords, integerType, enforced (tryMax lowers) typeMin && enforced (tryMin uppers) typeMax)
