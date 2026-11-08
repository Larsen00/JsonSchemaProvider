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

    let private allIntegerTypes =
        [ IntegerType.Int8; IntegerType.UInt8; IntegerType.Int16; IntegerType.UInt16
          IntegerType.Int32; IntegerType.UInt32; IntegerType.Int64; IntegerType.UInt64 ]

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

    // The type whose range is exactly the loosest bounds, else the 32- or 64-bit type every bound fits in (unsigned when lower >= 0).
    let private chooseIntegerType (lowers: decimal list) (uppers: decimal list) : IntegerType =
        let exact =
            match tryMin lowers, tryMax uppers with
            | Some lower, Some upper -> allIntegerTypes |> List.tryFind (fun integerType -> rangeOf integerType = (lower, upper))
            | _ -> None

        let fits integerType =
            let typeMin, typeMax = rangeOf integerType
            List.forall (fun bound -> typeMin <= bound && bound <= typeMax) (lowers @ uppers)

        // The 64-bit type is the fallback when even it can't hold a bound; values beyond it throw like int's do.
        let narrow, wide =
            match tryMin lowers with
            | Some lower when lower >= 0m -> IntegerType.UInt32, IntegerType.UInt64
            | _ -> IntegerType.Int32, IntegerType.Int64

        exact |> Option.defaultWith (fun () -> if fits narrow then narrow else wide)

    // Helper function that turns a set of integer keywords into an IntegerShape value.
    // The loosest bounds pick the type, the strictest are validated (e.g. maximum 11 with exclusiveMaximum 256 gives byte).
    let classifyIntegerShape
        (compileFlags: ProviderConfiguration.CompileFlags)
        (keywords: JsonNumber.Keywords)
        : IntegerShape =
        let lowers = lowerBounds keywords.specific
        let uppers = upperBounds keywords.specific

        match lowers, uppers with
        | [], [] -> SizedInteger(keywords, IntegerType.Int32, true)

        | _ when compileFlags.IgnoreSpecificKeywords -> UnsupportedIntegerKeywords keywords

        | _ ->
            let integerType = chooseIntegerType lowers uppers
            let typeMin, typeMax = rangeOf integerType
            let enforced (strictBound: decimal option) (typeLimit: decimal) = Option.forall ((=) typeLimit) strictBound
            SizedInteger(keywords, integerType, enforced (tryMax lowers) typeMin && enforced (tryMin uppers) typeMax)
