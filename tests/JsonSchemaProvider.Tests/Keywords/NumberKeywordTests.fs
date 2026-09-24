namespace JsonSchemaProvider.Tests

// Number/integer keywords at the schema root. Object-nested versions live in ObjectKeywordTests.fs.
module NumberKeywordTests =
    open Expecto
    open JsonSchemaProvider

    [<Literal>]
    let exclusiveRangeSchema = """{ "type": "number", "exclusiveMinimum": 0, "exclusiveMaximum": 10 }"""
    type ExclusiveRange = JsonSchemaProvider<schema=exclusiveRangeSchema>

    let exclusiveMinimumBoundaryIsRejected =
        test "exclusiveMinimum rejects the boundary value itself" {
            Expect.isError (ExclusiveRange.Create(0.0)) "0 is excluded by exclusiveMinimum"
        }

    let exclusiveMaximumBoundaryIsRejected =
        test "exclusiveMaximum rejects the boundary value itself" {
            Expect.isError (ExclusiveRange.Create(10.0)) "10 is excluded by exclusiveMaximum"
        }

    let insideExclusiveRangeIsAccepted =
        test "a value strictly inside an exclusive range is accepted" {
            let result = Expect.wantOk (ExclusiveRange.Create(5.0)) "Create should succeed"
            Expect.equal result 5.0 "5 is strictly between the bounds"
        }

    [<Literal>]
    let multipleOfSchema = """{ "type": "integer", "multipleOf": 5 }"""
    type MultipleOf = JsonSchemaProvider<schema=multipleOfSchema>

    let nonMultipleIsRejected =
        test "multipleOf rejects a non-multiple value" {
            Expect.isError (MultipleOf.Create(7)) "7 is not a multiple of 5"
        }

    let multipleIsAccepted =
        test "multipleOf accepts a genuine multiple" {
            let result = Expect.wantOk (MultipleOf.Create(10)) "Create should succeed"
            Expect.equal result 10 "10 is a multiple of 5"
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
