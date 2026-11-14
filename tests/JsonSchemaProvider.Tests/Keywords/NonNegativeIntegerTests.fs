namespace JsonSchemaProvider.Tests

// An integer whose smallest allowed value is exactly 0 (minimum 0 or exclusiveMinimum -1) maps
// to uint32, so a negative value can't be passed to Create at all. Create needs no Result unless
// other keywords (maximum, multipleOf, enum, ...) sit alongside, which are then validated.
// Without a maximum this compiles only by default (requireIntegerBounds=false); the strict versions with maximum
// 4294967295 are in Mechanisms/RequireIntegerBoundsTests.
module NonNegativeIntegerTests =
    open System
    open Xunit
    open JsonSchemaProvider

    [<Literal>]
    let minimumRootSchema = """{ "type": "integer", "minimum": 0 }"""
    type MinimumRoot = JsonSchemaProvider<schema=minimumRootSchema>

    [<Literal>]
    let exclusiveMinimumRootSchema = """{ "type": "integer", "exclusiveMinimum": -1 }"""
    type ExclusiveMinimumRoot = JsonSchemaProvider<schema=exclusiveMinimumRootSchema>

    [<Fact>]
    let ``minimum 0 root Create takes a uint32 and needs no Result`` () =
        expectEqual (MinimumRoot.Create 0u) 0u
        expectEqual (MinimumRoot.Create UInt32.MaxValue) UInt32.MaxValue

    [<Fact>]
    let ``exclusiveMinimum -1 root Create takes a uint32 and needs no Result`` () =
        expectEqual (ExclusiveMinimumRoot.Create 0u) 0u

    [<Fact>]
    let ``non-negative root Parse accepts the uint32 range beyond Int32`` () =
        expectEqual (expectOk (MinimumRoot.Parse("0"))) 0u
        expectEqual (expectOk (MinimumRoot.Parse("3000000000"))) 3000000000u
        expectEqual (expectOk (MinimumRoot.Parse("4294967295"))) UInt32.MaxValue

    [<Fact>]
    let ``non-negative root Parse rejects a negative number`` () =
        expectError (MinimumRoot.Parse("-1"))
        expectError (ExclusiveMinimumRoot.Parse("-1"))

    // Validation only checks the lower bound, so a number above UInt32.MaxValue passes it and
    // the checked conversion throws, the same way AsInteger does for int (see IntegerRangeTests).
    [<Fact>]
    let ``non-negative root Parse throws above UInt32 MaxValue`` () =
        Assert.Throws<OverflowException>(fun () -> MinimumRoot.Parse("4294967296") |> ignore) |> ignore

    // Adding maximum UInt32.MaxValue to the schema turns that throw into an Error.
    [<Literal>]
    let uint32RangeSchema = """{ "type": "integer", "minimum": 0, "maximum": 4294967295 }"""
    type UInt32Range = JsonSchemaProvider<schema=uint32RangeSchema>

    [<Fact>]
    let ``maximum UInt32 MaxValue makes Parse reject larger numbers instead of throwing`` () =
        expectEqual (expectOk (UInt32Range.Parse("4294967295"))) UInt32.MaxValue
        expectError (UInt32Range.Parse("4294967296"))

    [<Literal>]
    let boundedSchema = """{ "type": "integer", "minimum": 0, "maximum": 10 }"""
    type Bounded = JsonSchemaProvider<schema=boundedSchema>

    [<Fact>]
    let ``minimum 0 with maximum shrinks to byte and Create validates the maximum`` () =
        expectEqual (expectOk (Bounded.Create 10uy)) 10uy
        expectError (Bounded.Create 11uy)

    [<Literal>]
    let multipleOfSchema = """{ "type": "integer", "minimum": 0, "multipleOf": 2 }"""
    type MultipleOf = JsonSchemaProvider<schema=multipleOfSchema>

    [<Fact>]
    let ``minimum 0 with multipleOf keeps uint32 and Create validates multipleOf`` () =
        expectEqual (expectOk (MultipleOf.Create 4u)) 4u
        expectError (MultipleOf.Create 3u)

    [<Literal>]
    let propertySchema =
        """{ "type": "object", "properties": { "count": { "type": "integer", "minimum": 0 }, "limit": { "type": "integer", "exclusiveMinimum": -1 } }, "required": ["count"] }"""
    type Property = JsonSchemaProvider<schema=propertySchema>

    [<Fact>]
    let ``required non-negative property is a uint32`` () =
        let v = Property.Create(count = 3u)
        expectEqual v.count 3u
        expectEqual (v.ToString() |> String.filter (not << Char.IsWhiteSpace)) """{"count":3}"""

    [<Fact>]
    let ``optional non-negative property is a uint32 option`` () =
        expectEqual (Property.Create(count = 1u, limit = 5u)).limit (Some 5u)
        expectEqual (Property.Create(count = 1u)).limit None

    [<Fact>]
    let ``non-negative property Parse rejects a negative number`` () =
        expectError (Property.Parse("""{"count": -1}"""))
        expectError (Property.Parse("""{"count": 1, "limit": -1}"""))

    [<Literal>]
    let itemsSchema = """{ "type": "array", "items": { "type": "integer", "minimum": 0 } }"""
    type Items = JsonSchemaProvider<schema=itemsSchema>

    [<Fact>]
    let ``non-negative items give a uint32 list`` () =
        expectEqual (Items.Create [ 0u; 7u ]) [ 0u; 7u ]
        expectEqual (expectOk (Items.Parse("[0, 3000000000]"))) [ 0u; 3000000000u ]
        expectError (Items.Parse("[1, -1]"))

    [<Literal>]
    let tupleSchema = """{ "type": "array", "items": { "type": "integer", "minimum": 0 }, "minItems": 2, "maxItems": 2 }"""
    type Tuple = JsonSchemaProvider<schema=tupleSchema>

    [<Fact>]
    let ``non-negative items work inside a compiled array shape`` () =
        expectEqual (Tuple.Create((1u, 2u))) (1u, 2u)

    [<Literal>]
    let oneOfSchema = """{ "oneOf": [ { "type": "integer", "minimum": 0 }, { "type": "string" } ] }"""
    type OneOf = JsonSchemaProvider<schema=oneOfSchema>

    [<Fact>]
    let ``non-negative integer works as a oneOf branch`` () =
        expectEqual (expectOk (OneOf.Parse("5"))) (Choice1Of2 5u)
        expectEqual (expectOk (OneOf.Parse("\"x\""))) (Choice2Of2 "x")

    [<Literal>]
    let overlappingOneOfSchema = """{ "oneOf": [ { "type": "integer", "minimum": 0 }, { "type": "integer" } ] }"""
    type OverlappingOneOf = JsonSchemaProvider<schema=overlappingOneOfSchema>

    [<Fact>]
    let ``oneOf with overlapping uint32 and int branches rejects values matching both`` () =
        expectError (OverlappingOneOf.Parse("5"))
        expectEqual (expectOk (OverlappingOneOf.Parse("-5"))) (Choice2Of2 -5)

    [<Literal>]
    let maxItemsSchema = """{ "type": "array", "items": { "type": "integer", "minimum": 0 }, "maxItems": 2 }"""
    type MaxItems = JsonSchemaProvider<schema=maxItemsSchema>

    [<Fact>]
    let ``non-negative items work in a maxItems option chain`` () =
        expectEqual (expectOk (MaxItems.Parse("[1]"))) (Some(1u, None))
        expectEqual (MaxItems.ToList(Some(1u, Some 2u))) [ 1u; 2u ]

    [<Literal>]
    let nestedSchema =
        """{ "type": "object", "properties": { "inner": { "type": "object", "properties": { "n": { "type": "integer", "minimum": 0 } }, "required": ["n"] } }, "required": ["inner"] }"""
    type Nested = JsonSchemaProvider<schema=nestedSchema>

    [<Fact>]
    let ``non-negative property of a nested object makes the nested Create need no Result`` () =
        expectEqual (Nested.innerObj.Create(n = 4u)).n 4u
        expectError (Nested.Parse("""{"inner": {"n": -3}}"""))

    [<Literal>]
    let enumSchema = """{ "type": "integer", "minimum": 0, "enum": [1, 2] }"""
    type Enum = JsonSchemaProvider<schema=enumSchema>

    [<Fact>]
    let ``a stray keyword keeps uint32 but Create validates`` () =
        expectEqual (expectOk (Enum.Create 1u)) 1u
        expectError (Enum.Create 3u)

    // Skipping validation lets a negative number reach the checked conversion, which throws.
    [<Literal>]
    let skippedSchema = """{ "type": "integer", "minimum": 0 }"""
    type Skipped = JsonSchemaProvider<schema=skippedSchema, skipRuntimeValidation=true>

    [<Fact>]
    let ``skipRuntimeValidation Parse throws on a negative number`` () =
        Assert.Throws<OverflowException>(fun () -> Skipped.Parse("-1") |> ignore) |> ignore

    // Every other integer bound has no matching F# type, so these stay int with a validating Create.

    [<Literal>]
    let minimumOneSchema = """{ "type": "integer", "minimum": 1, "exclusiveMinimum": -2147483649 }"""
    type MinimumOne = JsonSchemaProvider<schema=minimumOneSchema>

    [<Fact>]
    let ``minimum 1 stays int and Create validates`` () =
        expectEqual (expectOk (MinimumOne.Create 1)) 1
        expectError (MinimumOne.Create 0)

    [<Literal>]
    let exclusiveZeroSchema = """{ "type": "integer", "minimum": -2147483648, "exclusiveMinimum": 0 }"""
    type ExclusiveZero = JsonSchemaProvider<schema=exclusiveZeroSchema>

    [<Fact>]
    let ``exclusiveMinimum 0 stays int and Create rejects 0`` () =
        expectError (ExclusiveZero.Create 0)
        expectEqual (expectOk (ExclusiveZero.Create 1)) 1

    // Draft 4: "exclusiveMinimum": true makes "minimum": 0 exclusive, so 0 itself is not allowed.
    // Draft 4 has no second lower bound to keep it int, so this one is uint32.
    [<Literal>]
    let draft4ExclusiveZeroSchema =
        """{ "$schema": "http://json-schema.org/draft-04/schema#", "type": "integer", "minimum": 0, "exclusiveMinimum": true }"""
    type Draft4ExclusiveZero = JsonSchemaProvider<schema=draft4ExclusiveZeroSchema>

    [<Fact>]
    let ``draft 4 exclusive minimum 0 gives uint32 and Create rejects 0`` () =
        expectError (Draft4ExclusiveZero.Create 0u)
        expectEqual (expectOk (Draft4ExclusiveZero.Create 1u)) 1u

    [<Literal>]
    let ignoredSchema = """{ "type": "integer", "minimum": 0 }"""
    type Ignored = JsonSchemaProvider<schema=ignoredSchema, ignoreSpecificKeywords=true>

    [<Fact>]
    let ``ignoreSpecificKeywords keeps minimum 0 as int and Create validates`` () =
        expectEqual (expectOk (Ignored.Create 0)) 0
        expectError (Ignored.Create -1)
