namespace RefinedJsonSchemaProvider.Tests

// Integers beyond int64/uint64 don't fit any integer node's type, and NJsonSchema throws on an
// integer literal outside int64 (see IntegerTypeTests.fs). These tests show what a user can write
// instead: a string node with an integer pattern, or a number node (a float).
module LargeIntegerWorkaroundTests =
    open Xunit
    open RefinedJsonSchemaProvider

    // Workaround 1: the integer as a string, its digits checked by a pattern.
    [<Literal>]
    let stringRootSchema = """{ "type": "string", "pattern": "^-?[0-9]+$" }"""
    type StringRoot = RefinedJsonSchemaProvider<schema=stringRootSchema>

    [<Fact>]
    let ``a string root with an integer pattern holds an integer beyond uint64`` () =
        expectEqual (expectOk (StringRoot.Create "123456789012345678901234567890")) "123456789012345678901234567890"
        expectEqual (expectOk (StringRoot.Create "-123456789012345678901234567890")) "-123456789012345678901234567890"
        expectEqual (expectOk (StringRoot.Parse("\"123456789012345678901234567890\""))) "123456789012345678901234567890"

    [<Fact>]
    let ``a string root with an integer pattern rejects a non-integer`` () =
        expectError (StringRoot.Create "12a")
        expectError (StringRoot.Parse("\"12a\""))

    [<Fact>]
    let ``a string root with an integer pattern rejects the integer written as a JSON number`` () =
        expectError (StringRoot.Parse("123"))

    [<Fact>]
    let ``the string converts to a BigInteger in user code`` () =
        let value = expectOk (StringRoot.Parse("\"123456789012345678901234567890\""))
        expectEqual (System.Numerics.BigInteger.Parse value) (System.Numerics.BigInteger.Parse "123456789012345678901234567890")

    [<Literal>]
    let stringPropertySchema =
        """{ "type": "object", "properties": { "id": { "type": "string", "pattern": "^-?[0-9]+$" } }, "required": ["id"] }"""
    type StringProperty = RefinedJsonSchemaProvider<schema=stringPropertySchema>

    [<Fact>]
    let ``a string property with an integer pattern holds an integer beyond uint64`` () =
        expectEqual (expectOk (StringProperty.Create(id = "123456789012345678901234567890"))).id "123456789012345678901234567890"
        expectEqual (expectOk (StringProperty.Parse("""{"id": "123456789012345678901234567890"}"""))).id "123456789012345678901234567890"

    [<Fact>]
    let ``a string property with an integer pattern rejects a non-integer`` () =
        expectError (StringProperty.Create(id = "12a"))
        expectError (StringProperty.Parse("""{"id": "12a"}"""))

    // Workaround 2: a number node, provided as float. What Parse does with a large integer literal
    // depends on how it is written.
    [<Literal>]
    let numberRootSchema = """{ "type": "number" }"""
    type NumberRoot = RefinedJsonSchemaProvider<schema=numberRootSchema>
    type UncheckedNumberRoot = RefinedJsonSchemaProvider<schema=numberRootSchema, allowNonFiniteNumbers=true>

    [<Fact>]
    let ``a number root Parse gives the int64 Error on a large integer literal without exponent`` () =
        match NumberRoot.Parse("123456789012345678901234567890") with
        | Error [ message ] ->
            Assert.Contains("The JSON contains the integer 123456789012345678901234567890, which is outside the int64 range", message)
        | other -> failwithf "Expected one NJsonSchema-limit Error, got %A" other

    [<Fact>]
    let ``a number root Parse accepts a large integer written with an exponent`` () =
        expectEqual (expectOk (NumberRoot.Parse("1.5e20"))) 1.5e20

    [<Fact>]
    let ``a number root Parse rounds a long integer written with an exponent to the nearest float`` () =
        let value = expectOk (NumberRoot.Parse("1.23456789012345678901234567890e29"))
        expectEqual value 1.2345678901234568e29
        // The float is not the integer that was written: the last digits are lost.
        Assert.NotEqual(System.Numerics.BigInteger.Parse "123456789012345678901234567890", System.Numerics.BigInteger value)

    [<Fact>]
    let ``a number root Create takes the large integer as a float`` () =
        expectEqual (UncheckedNumberRoot.Create 123456789012345678901234567890.0) 1.2345678901234568e29

    [<Fact>]
    let ``a number root Create takes the large integer as a float and gives Ok by default`` () =
        expectEqual (expectOk (NumberRoot.Create 123456789012345678901234567890.0)) 1.2345678901234568e29

    [<Literal>]
    let numberPropertySchema = """{ "type": "object", "properties": { "id": { "type": "number" } }, "required": ["id"] }"""
    type NumberProperty = RefinedJsonSchemaProvider<schema=numberPropertySchema>

    [<Fact>]
    let ``a number property behaves like the number root`` () =
        match NumberProperty.Parse("""{"id": 123456789012345678901234567890}""") with
        | Error [ message ] -> Assert.Contains("outside the int64 range", message)
        | other -> failwithf "Expected one NJsonSchema-limit Error, got %A" other

        expectEqual (expectOk (NumberProperty.Parse("""{"id": 1.5e20}"""))).id 1.5e20

    // The Error's fix B: without runtime validation NJsonSchema never sees the literal.
    type NumberRootUnvalidated = RefinedJsonSchemaProvider<schema=numberRootSchema, skipRuntimeValidation=true>

    [<Fact>]
    let ``with skipRuntimeValidation a number root Parse reads the literal without exponent as a float`` () =
        expectEqual (expectOk (NumberRootUnvalidated.Parse("123456789012345678901234567890"))) 1.2345678901234568e29
