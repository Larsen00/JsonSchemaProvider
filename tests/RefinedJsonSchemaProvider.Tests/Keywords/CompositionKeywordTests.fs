namespace RefinedJsonSchemaProvider.Tests

// anyOf, allOf, not (and oneOf) next to "type": the type comes from "type", and the composition
// keyword is never compiled, so it is checked by the runtime fallback. Composition keywords
// without "type" (except oneOf) stop the provider; see the design-time KnownGaps/KnownGapTests.fs.
module CompositionKeywordTests =
    open Xunit
    open RefinedJsonSchemaProvider

    [<Literal>]
    let anyOfStringSchema = """{ "type": "string", "anyOf": [ { "minLength": 5 }, { "pattern": "^a" } ] }"""
    type AnyOfString = RefinedJsonSchemaProvider<schema = anyOfStringSchema>

    [<Fact>]
    let ``anyOf: Create accepts a string matching only the first branch`` () =
        expectEqual (expectOk (AnyOfString.Create "bcdef")) "bcdef"

    [<Fact>]
    let ``anyOf: Create accepts a string matching only the second branch`` () =
        expectEqual (expectOk (AnyOfString.Create "ab")) "ab"

    [<Fact>]
    let ``anyOf: Create accepts a string matching both branches`` () =
        expectEqual (expectOk (AnyOfString.Create "abcdef")) "abcdef"

    [<Fact>]
    let ``anyOf: Create rejects a string matching no branch`` () = expectError (AnyOfString.Create "bc")

    [<Fact>]
    let ``anyOf: Parse rejects a string matching no branch`` () = expectError (AnyOfString.Parse "\"bc\"")

    [<Literal>]
    let allOfIntegerSchema = """{ "type": "integer", "allOf": [ { "minimum": 0 }, { "maximum": 10 } ] }"""
    type AllOfInteger = RefinedJsonSchemaProvider<schema = allOfIntegerSchema>

    [<Fact>]
    let ``allOf: Create accepts an integer matching every branch`` () =
        expectEqual (expectOk (AllOfInteger.Create 5)) 5

    [<Fact>]
    let ``allOf: Create rejects an integer failing one branch`` () = expectError (AllOfInteger.Create 11)

    [<Fact>]
    let ``allOf: Parse rejects an integer failing one branch`` () = expectError (AllOfInteger.Parse "-1")

    [<Literal>]
    let notStringSchema = """{ "type": "string", "not": { "enum": [ "admin" ] } }"""
    type NotString = RefinedJsonSchemaProvider<schema = notStringSchema>

    [<Fact>]
    let ``not: Create accepts a string the subschema rejects`` () =
        expectEqual (expectOk (NotString.Create "bob")) "bob"

    [<Fact>]
    let ``not: Create rejects a string the subschema accepts`` () = expectError (NotString.Create "admin")

    [<Fact>]
    let ``not: Parse rejects a string the subschema accepts`` () = expectError (NotString.Parse "\"admin\"")

    [<Literal>]
    let oneOfIntegerSchema = """{ "type": "integer", "oneOf": [ { "multipleOf": 3 }, { "multipleOf": 5 } ] }"""
    type OneOfInteger = RefinedJsonSchemaProvider<schema = oneOfIntegerSchema>

    [<Fact>]
    let ``oneOf next to type: Create accepts an integer matching exactly one branch`` () =
        expectEqual (expectOk (OneOfInteger.Create 3)) 3

    [<Fact>]
    let ``oneOf next to type: Create rejects an integer matching both branches`` () =
        expectError (OneOfInteger.Create 15)

    [<Fact>]
    let ``oneOf next to type: Create rejects an integer matching no branch`` () =
        expectError (OneOfInteger.Create 7)

    [<Literal>]
    let anyOfPropertySchema =
        """
        {
            "type": "object",
            "properties": {
                "name": { "type": "string", "anyOf": [ { "minLength": 5 }, { "pattern": "^a" } ] }
            },
            "required": [ "name" ]
        }
        """
    type AnyOfProperty = RefinedJsonSchemaProvider<schema = anyOfPropertySchema>

    [<Fact>]
    let ``anyOf on a property: Create accepts a matching name`` () =
        expectEqual (expectOk (AnyOfProperty.Create(name = "ab"))).name "ab"

    [<Fact>]
    let ``anyOf on a property: Create rejects a name matching no branch`` () =
        expectError (AnyOfProperty.Create(name = "bc"))

    [<Fact>]
    let ``anyOf on a property: Parse rejects a name matching no branch`` () =
        expectError (AnyOfProperty.Parse """{ "name": "bc" }""")
