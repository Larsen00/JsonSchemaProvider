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
        | UnsupportedIntegerKeywords _ -> "Unsupported"
        | SizedInteger(_, integerType, true) -> $"%A{integerType} enforced"
        | SizedInteger(_, integerType, false) -> $"%A{integerType} validated"

    // An integer schema; `keywords` is spliced in after "type", e.g. ", \"minimum\": 0".
    let private integer (keywords: string) = """{ "type": "integer" """ + keywords + " }"

    let private expectShape (keywords: string) (expected: string) =
        Expect.equal (shapeName (classify (integer keywords))) expected keywords

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

              test "a bounded non-negative range that matches no type exactly gives a validated UInt32" {
                  expectShape ", \"minimum\": 0, \"maximum\": 10" "UInt32 validated"
                  expectShape ", \"minimum\": 0, \"exclusiveMaximum\": 10" "UInt32 validated"
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

              test "exclusive bounds count towards an exact match" {
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

              test "a bound one off Int64 MaxValue is not an exact match (decimal, not float)" {
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

              test "a looser lower bound outside a type's range keeps it from being an exact match" {
                  // The strict bounds are exactly byte's range, but the loosest lower bound is -4.
                  expectShape ", \"minimum\": 0, \"exclusiveMinimum\": -5, \"maximum\": 255" "Int32 validated"
              }

              test "minimum above maximum is a validated type no term satisfies" {
                  expectShape ", \"minimum\": 10, \"maximum\": 5" "UInt32 validated"
              }

              test "draft 4 boolean exclusiveMaximum counts towards an exact match" {
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

              test "IgnoreSpecificKeywords leaves an integer without numeric keywords an enforced Int32" {
                  let flags = { defaultFlags with IgnoreSpecificKeywords = true }
                  Expect.equal (shapeName (classifyWith flags (integer ""))) "Int32 enforced" ""
              } ]
