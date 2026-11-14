namespace JsonSchemaProvider.Tests

// Number/integer keywords at the schema root. Object-nested versions live in ObjectKeywordTests.fs.
module NumberKeywordTests =
    open Xunit
    open FsCheck
    open FsCheck.Xunit
    open JsonSchemaProvider

    [<Literal>]
    let exclusiveRangeSchema = """{ "type": "number", "exclusiveMinimum": 0, "exclusiveMaximum": 10 }"""
    type ExclusiveRange = JsonSchemaProvider<schema=exclusiveRangeSchema>

    [<Fact>]
    let ``exclusiveMinimum rejects the boundary value itself`` () =
        expectError (ExclusiveRange.Create(0.0)) // 0 is excluded by exclusiveMinimum

    [<Fact>]
    let ``exclusiveMaximum rejects the boundary value itself`` () =
        expectError (ExclusiveRange.Create(10.0)) // 10 is excluded by exclusiveMaximum

    [<Fact>]
    let ``a value strictly inside an exclusive range is accepted`` () =
        let result = expectOk (ExclusiveRange.Create(5.0))
        expectEqual result 5.0

    [<Property>]
    let ``exclusive range Create accepts exactly the values strictly between the bounds`` (NormalFloat x) =
        expectEqual (Result.isOk (ExclusiveRange.Create x)) (0.0 < x && x < 10.0)

    [<Literal>]
    let multipleOfSchema = """{ "type": "integer", "multipleOf": 5 }"""
    type MultipleOf = JsonSchemaProvider<schema=multipleOfSchema>

    [<Fact>]
    let ``multipleOf rejects a non-multiple value`` () =
        expectError (MultipleOf.Create(7)) // 7 is not a multiple of 5

    [<Fact>]
    let ``multipleOf accepts a genuine multiple`` () =
        let result = expectOk (MultipleOf.Create(10))
        expectEqual result 10

    [<Property>]
    let ``multipleOf Create accepts exactly the multiples of 5`` (n: int) =
        expectEqual (Result.isOk (MultipleOf.Create n)) (n % 5 = 0)
