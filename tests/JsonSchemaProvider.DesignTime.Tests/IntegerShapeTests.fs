namespace JsonSchemaProvider.Tests

module IntegerShapeTests =
    open Expecto
    open JsonSchemaProvider.DesignTime.SchemaConversion
    open JsonSchemaProvider.DesignTime.IntegerShape
    open JsonSchemaProvider.DesignTime.ProviderConfiguration
    open TestHelpers

    let private classifyWith (flags: CompileFlags) (schemaText: string) =
        match parseJsonSchema schemaText with
        | JsonInteger keywords -> classifyIntegerShape flags keywords
        | other -> failwithf "Expected an integer schema, got %A" other

    let private classify = classifyWith defaultFlags

    // "Unsupported", or the picked type plus "enforced" when the type alone enforces the bounds.
    let private shapeName (shape: IntegerShape) =
        match shape with
        | InvalidIntegerBounds _ -> "InvalidBounds"
        | UnboundedInteger(_, NoBounds) -> "NoBounds"
        | UnboundedInteger(_, NoLowerBound integerType) -> $"NoLowerBound %A{integerType}"
        | UnboundedInteger(_, NoUpperBound integerType) -> $"NoUpperBound %A{integerType}"
        | UnboundedInteger(_, RangeTooWide integerType) -> $"RangeTooWide %A{integerType}"
        | UnsupportedIntegerKeywords _ -> "Unsupported"
        | SizedInteger(_, integerType, true) -> $"%A{integerType} enforced"
        | SizedInteger(_, integerType, false) -> $"%A{integerType} validated"

    // An integer schema; `keywords` is spliced in after "type", e.g. ", \"minimum\": 0".
    let private integer (keywords: string) = """{ "type": "integer" """ + keywords + " }"

    let private expectShape (keywords: string) (expected: string) =
        Expect.equal (shapeName (classify (integer keywords))) expected keywords

    let private expectStrictShape (keywords: string) (expected: string) =
        Expect.equal (shapeName (classifyWith strictFlags (integer keywords))) expected keywords

    let private int32Range = ", \"minimum\": -2147483648, \"maximum\": 2147483647"

    // With requireIntegerBounds: only types that hold every valid instance compile.
    [<Tests>]
    let strictTests =
        testList
            "IntegerShapeTests (strict)"
            [ test "no numeric keywords is NoBounds" { expectStrictShape "" "NoBounds" }

              // The reason carries the type the existing bound picks, which the error's hint is for.
              test "only a lower bound is NoUpperBound with the type that bound picks" {
                  expectStrictShape ", \"minimum\": 0" "NoUpperBound UInt32"
                  expectStrictShape ", \"exclusiveMinimum\": -1" "NoUpperBound UInt32"
                  expectStrictShape ", \"minimum\": -1" "NoUpperBound Int32"
                  expectStrictShape ", \"minimum\": -9223372036854775808" "NoUpperBound Int64"
              }

              test "only an upper bound is NoLowerBound with the type that bound picks" {
                  expectStrictShape ", \"maximum\": 10" "NoLowerBound Int32"
                  expectStrictShape ", \"maximum\": 9223372036854775807" "NoLowerBound Int64"
              }

              test "adding the hinted limit gives an enforced type" {
                  expectStrictShape ", \"minimum\": 0, \"maximum\": 4294967295" "UInt32 enforced"
                  expectStrictShape ", \"minimum\": -2147483648, \"maximum\": 10" "Int32 validated"
                  expectStrictShape ", \"minimum\": -2147483648, \"maximum\": 2147483647" "Int32 enforced"
              }

              test "a draft 4 boolean exclusiveMinimum is not an upper bound" {
                  expectStrictShape ", \"minimum\": 0, \"exclusiveMinimum\": true" "NoUpperBound UInt32"
              }

              test "the int range gives an enforced Int32" { expectStrictShape int32Range "Int32 enforced" }

              test "exclusive bounds count as bounds" {
                  expectStrictShape ", \"exclusiveMinimum\": -2147483649, \"exclusiveMaximum\": 2147483648" "Int32 enforced"
                  expectStrictShape ", \"exclusiveMinimum\": -1, \"exclusiveMaximum\": 256" "UInt8 enforced"
                  expectStrictShape ", \"minimum\": 0, \"exclusiveMaximum\": 100" "UInt8 validated"
              }

              test "both bounds pick the same type as with the flag" {
                  expectStrictShape ", \"minimum\": 0, \"maximum\": 255" "UInt8 enforced"
                  expectStrictShape ", \"minimum\": 1, \"maximum\": 100" "UInt8 validated"
                  expectStrictShape ", \"minimum\": -1, \"maximum\": 1000" "Int16 validated"
                  expectStrictShape ", \"minimum\": 0, \"maximum\": 4294967295" "UInt32 enforced"
                  expectStrictShape ", \"minimum\": 0, \"maximum\": 5000000000" "UInt64 validated"
                  expectStrictShape ", \"minimum\": -9223372036854775808, \"maximum\": 9223372036854775807" "Int64 enforced"
                  expectStrictShape ", \"minimum\": 0, \"maximum\": 18446744073709551615" "UInt64 enforced"
              }

              test "a valid range no integer type holds is RangeTooWide" {
                  expectStrictShape ", \"minimum\": 0, \"maximum\": 18446744073709551616" "RangeTooWide UInt64"
                  expectStrictShape ", \"minimum\": -1, \"maximum\": 9223372036854775808" "RangeTooWide Int64"
                  expectStrictShape ", \"minimum\": -9223372036854775809, \"maximum\": 0" "RangeTooWide Int64"
              }

              test "the strictest bounds decide whether the valid range fits" {
                  // The looser maximum is outside uint64, but the valid range 0..10 fits.
                  expectStrictShape ", \"minimum\": 0, \"maximum\": 10, \"exclusiveMaximum\": 1e20" "UInt64 validated"
              }

              test "InvalidBounds wins over the missing-bound checks" {
                  expectStrictShape ", \"minimum\": 10, \"maximum\": 5" "InvalidBounds"
              }

              test "IgnoreSpecificKeywords needs bounds inside int" {
                  let flags = { strictFlags with IgnoreSpecificKeywords = true }
                  let shape keywords = shapeName (classifyWith flags (integer keywords))
                  Expect.equal (shape int32Range) "Unsupported" "int range"
                  Expect.equal (shape ", \"minimum\": 0, \"maximum\": 255") "Unsupported" "byte range"
                  Expect.equal (shape ", \"minimum\": 0, \"maximum\": 4294967295") "RangeTooWide Int32" "uint32 range"
                  Expect.equal (shape "") "NoBounds" "no bounds"
                  Expect.equal (shape ", \"minimum\": 0") "NoUpperBound Int32" "lower bound only, the hint is for int"
              }

              test "SkipRuntimeValidation does not lift the strict rule" {
                  let flags = { strictFlags with SkipRuntimeValidation = true }
                  Expect.equal (shapeName (classifyWith flags (integer ""))) "NoBounds" ""
              } ]

    [<Tests>]
    let tests =
        testList
            "IntegerShapeTests"
            [ test "no numeric keywords is an enforced Int32" { expectShape "" "Int32 enforced" }

              test "minimum 0 alone is an enforced UInt32" { expectShape ", \"minimum\": 0" "UInt32 enforced" }

              test "exclusiveMinimum -1 alone is an enforced UInt32" { expectShape ", \"exclusiveMinimum\": -1" "UInt32 enforced" }

              test "fractional bounds round to the integers they allow" {
                  expectShape ", \"minimum\": -0.5" "UInt32 enforced"
                  expectShape ", \"exclusiveMinimum\": -0.5" "UInt32 enforced"
              }

              test "any non-negative lower bound gives UInt32, validated when it is above 0" {
                  expectShape ", \"minimum\": 1" "UInt32 validated"
                  expectShape ", \"exclusiveMinimum\": 0" "UInt32 validated"
              }

              test "a negative lower bound gives Int32" { expectShape ", \"minimum\": -1" "Int32 validated" }

              test "an upper bound alone gives Int32" { expectShape ", \"maximum\": 10" "Int32 validated" }

              test "a bounded range gives the smallest type it fits in, validated unless it is the type's range" {
                  expectShape ", \"minimum\": 0, \"maximum\": 10" "UInt8 validated"
                  expectShape ", \"minimum\": 0, \"exclusiveMaximum\": 10" "UInt8 validated"
                  expectShape ", \"minimum\": 1, \"maximum\": 100" "UInt8 validated"
                  expectShape ", \"minimum\": 0, \"maximum\": 1000" "UInt16 validated"
                  expectShape ", \"minimum\": 0, \"maximum\": 100000" "UInt32 validated"
                  expectShape ", \"minimum\": -1, \"maximum\": 100" "Int8 validated"
                  expectShape ", \"minimum\": -1, \"maximum\": 1000" "Int16 validated"
                  expectShape ", \"minimum\": -40000, \"maximum\": 0" "Int32 validated"
              }

              test "a one-sided bound starts at 32 bits" {
                  expectShape ", \"minimum\": 5" "UInt32 validated"
                  expectShape ", \"maximum\": 10" "Int32 validated"
                  expectShape ", \"minimum\": -1" "Int32 validated"
              }

              test "multipleOf does not change the type" {
                  // The type still enforces the bounds; multipleOf makes CanBeCompiled false instead.
                  expectShape ", \"minimum\": 0, \"multipleOf\": 2" "UInt32 enforced"
              }

              test "bounds that are exactly a type's range give that type, enforced" {
                  expectShape ", \"minimum\": -128, \"maximum\": 127" "Int8 enforced"
                  expectShape ", \"minimum\": 0, \"maximum\": 255" "UInt8 enforced"
                  expectShape ", \"minimum\": -32768, \"maximum\": 32767" "Int16 enforced"
                  expectShape ", \"minimum\": 0, \"maximum\": 65535" "UInt16 enforced"
                  expectShape ", \"minimum\": -2147483648, \"maximum\": 2147483647" "Int32 enforced"
                  expectShape ", \"minimum\": 0, \"maximum\": 4294967295" "UInt32 enforced"
                  expectShape ", \"minimum\": -9223372036854775808, \"maximum\": 9223372036854775807" "Int64 enforced"
                  expectShape ", \"minimum\": 0, \"maximum\": 18446744073709551615" "UInt64 enforced"
              }

              test "exclusive bounds matching a type's range give an enforced type" {
                  expectShape ", \"exclusiveMinimum\": -1, \"exclusiveMaximum\": 256" "UInt8 enforced"
              }

              test "a redundant looser bound picks the smaller type and the stricter bound is validated" {
                  expectShape ", \"minimum\": 0, \"maximum\": 11, \"exclusiveMaximum\": 256" "UInt8 validated"
                  expectShape ", \"minimum\": 5, \"exclusiveMinimum\": -1, \"maximum\": 255" "UInt8 validated"
              }

              test "a looser lower bound picks UInt32 even when the stricter one is higher" {
                  expectShape ", \"minimum\": 0, \"exclusiveMinimum\": 3" "UInt32 validated"
              }

              test "bounds outside 32 bits widen to 64 bits" {
                  expectShape ", \"minimum\": 0, \"maximum\": 5000000000" "UInt64 validated"
                  expectShape ", \"minimum\": -5000000000" "Int64 validated"
                  expectShape ", \"maximum\": 5000000000" "Int64 validated"
              }

              test "a half-open range beyond 32 bits widens to 64 bits" {
                  expectShape ", \"minimum\": 5000000000" "UInt64 validated"
                  expectShape ", \"maximum\": -5000000000" "Int64 validated"
              }

              test "a stricter bound beyond 32 bits widens the type picked by the looser one" {
                  expectShape ", \"minimum\": 0, \"exclusiveMinimum\": 5000000000" "UInt64 validated"
              }

              test "a bound one off Int64 MaxValue is validated (decimal, not float)" {
                  expectShape ", \"minimum\": -9223372036854775808, \"maximum\": 9223372036854775806" "Int64 validated"
              }

              test "bounds no integer type holds fall back to the validated 64-bit type" {
                  expectShape ", \"minimum\": 0, \"maximum\": 18446744073709551616" "UInt64 validated"
                  expectShape ", \"minimum\": -1, \"maximum\": 9223372036854775808" "Int64 validated"
              }

              test "fractional upper bounds round down to the integers they allow" {
                  expectShape ", \"minimum\": 0, \"maximum\": 255.5" "UInt8 enforced"
                  expectShape ", \"minimum\": 0, \"exclusiveMaximum\": 255.5" "UInt8 enforced"
              }

              test "an upper bound alone at Int32 MaxValue is an enforced Int32" {
                  expectShape ", \"maximum\": 2147483647" "Int32 enforced"
              }

              test "a looser lower bound below zero picks a signed type even when the strict bounds fit byte" {
                  // The strict bounds are exactly byte's range, but the loosest lower bound is -4.
                  expectShape ", \"minimum\": 0, \"exclusiveMinimum\": -5, \"maximum\": 255" "Int16 validated"
              }

              test "a lower bound above an upper bound is InvalidBounds" {
                  expectShape ", \"minimum\": 10, \"maximum\": 5" "InvalidBounds"
                  expectShape ", \"minimum\": 0, \"exclusiveMinimum\": 5000, \"maximum\": 10" "InvalidBounds"
                  expectShape ", \"exclusiveMinimum\": 5, \"exclusiveMaximum\": 6" "InvalidBounds"
              }

              test "equal lower and upper bounds are a valid range" {
                  expectShape ", \"minimum\": 5, \"maximum\": 5" "UInt8 validated"
              }

              test "InvalidBounds wins over IgnoreSpecificKeywords" {
                  let flags = { defaultFlags with IgnoreSpecificKeywords = true }
                  Expect.equal (shapeName (classifyWith flags (integer ", \"minimum\": 10, \"maximum\": 5"))) "InvalidBounds" ""
              }

              test "draft 4 boolean exclusiveMaximum counts towards an enforced type" {
                  expectShape ", \"minimum\": 0, \"maximum\": 256, \"exclusiveMaximum\": true" "UInt8 enforced"
              }

              test "IgnoreSpecificKeywords turns an exact byte range into Unsupported" {
                  let flags = { defaultFlags with IgnoreSpecificKeywords = true }
                  Expect.equal (shapeName (classifyWith flags (integer ", \"minimum\": 0, \"maximum\": 255"))) "Unsupported" ""
              }

              // Draft 4 writes an exclusive bound as a boolean next to minimum.
              test "draft 4 minimum 0 with exclusiveMinimum true excludes 0 and is a validated UInt32" {
                  expectShape ", \"minimum\": 0, \"exclusiveMinimum\": true" "UInt32 validated"
              }

              test "draft 4 minimum -1 with exclusiveMinimum true is an enforced UInt32" {
                  expectShape ", \"minimum\": -1, \"exclusiveMinimum\": true" "UInt32 enforced"
              }

              test "IgnoreSpecificKeywords turns bounds into Unsupported" {
                  let flags = { defaultFlags with IgnoreSpecificKeywords = true }
                  Expect.equal (shapeName (classifyWith flags (integer ", \"minimum\": 0"))) "Unsupported" ""
              }

              test "IgnoreSpecificKeywords keeps int even for bounds outside int" {
                  let flags = { defaultFlags with IgnoreSpecificKeywords = true }
                  Expect.equal (shapeName (classifyWith flags (integer ", \"maximum\": 1000000000000"))) "Unsupported" ""
              }

              test "IgnoreSpecificKeywords leaves an integer without numeric keywords an enforced Int32" {
                  let flags = { defaultFlags with IgnoreSpecificKeywords = true }
                  Expect.equal (shapeName (classifyWith flags (integer ""))) "Int32 enforced" ""
              } ]
