namespace JsonSchemaProvider.Tests

// number/integer keywords beyond plain minimum/maximum: exclusiveMinimum, exclusiveMaximum,
// multipleOf. Validation runs via NJsonSchema's own schema.Validate against the real schema
// text, independent of which keywords SchemaConversion.fs's FSharpType/Keywords model happens
// to capture or act on at the type level - so it enforces these just as correctly as the ones
// it actively acts on (minimum/maximum on FSharpInt, which nothing enforces at compile time
// either - see ideas/compile-time-*-constraints.md).
module NumberKeywordTests =
    open Expecto
    open JsonSchemaProvider

    [<Literal>]
    let exclusiveRangeSchema =
        """
        { "type": "object", "properties": { "value": { "type": "number", "exclusiveMinimum": 0, "exclusiveMaximum": 10 } }, "required": ["value"] }"""

    type ExclusiveRange = JsonSchemaProvider<schema=exclusiveRangeSchema>

    [<Literal>]
    let multipleOfSchema =
        """
        { "type": "object", "properties": { "value": { "type": "integer", "multipleOf": 5 } }, "required": ["value"] }"""

    type MultipleOf = JsonSchemaProvider<schema=multipleOfSchema>

    let exclusiveMinimumBoundaryIsRejected =
        test "exclusiveMinimum rejects the boundary value itself" {
            Expect.isError
                (ExclusiveRange.Create(value = 0.0))
                "0 should be rejected - exclusiveMinimum excludes the bound"
        }

    let exclusiveMaximumBoundaryIsRejected =
        test "exclusiveMaximum rejects the boundary value itself" {
            Expect.isError
                (ExclusiveRange.Create(value = 10.0))
                "10 should be rejected - exclusiveMaximum excludes the bound"
        }

    let insideExclusiveRangeIsAccepted =
        test "a value strictly inside an exclusive range is accepted" {
            let result = Expect.wantOk (ExclusiveRange.Create(value = 5.0)) "Create should succeed"
            Expect.equal result.value 5.0 "5 is strictly between the exclusive bounds"
        }

    let nonMultipleIsRejected =
        test "multipleOf rejects a non-multiple value" {
            Expect.isError
                (MultipleOf.Create(value = 7))
                "7 is not a multiple of 5"
        }

    let multipleIsAccepted =
        test "multipleOf accepts a genuine multiple" {
            let result = Expect.wantOk (MultipleOf.Create(value = 10)) "Create should succeed"
            Expect.equal result.value 10 "10 is a multiple of 5"
        }

    [<Tests>]
    let tests =
        testList
            "JsonSchemaProvider.Tests.NumberKeywordTests"
            [ exclusiveMinimumBoundaryIsRejected
              exclusiveMaximumBoundaryIsRejected
              insideExclusiveRangeIsAccepted
              nonMultipleIsRejected
              multipleIsAccepted ]
