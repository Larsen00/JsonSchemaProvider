namespace JsonSchemaProvider.Tests

// With requireIntegerBounds=true an integer node only compiles when its F# type holds every valid
// instance, so a number outside the type is a Parse Error instead of an exception. These are the
// strict versions of the default (requireIntegerBounds=false) tests in Keywords/, with the int (or uint32)
// range written out. An unbounded integer with the flag is a compile error, covered by the
// design-time tests since it can't be written here.
module RequireIntegerBoundsTests =
    open System
    open Xunit
    open JsonSchemaProvider

    [<Literal>]
    let rootSchema = """{ "type": "integer", "minimum": -2147483648, "maximum": 2147483647 }"""
    type Root = JsonSchemaProvider<schema = rootSchema, requireIntegerBounds = true>

    [<Fact>]
    let ``an int-ranged root Create needs no Result`` () =
        expectEqual (Root.Create Int32.MaxValue) Int32.MaxValue
        expectEqual (Root.Create Int32.MinValue) Int32.MinValue

    [<Fact>]
    let ``an int-ranged root Parse returns Error outside int instead of throwing`` () =
        expectEqual (expectOk (Root.Parse("-2147483648"))) Int32.MinValue
        expectError (Root.Parse("2147483648"))
        expectError (Root.Parse("-2147483649"))

    [<Literal>]
    let propertySchema =
        """{ "type": "object", "properties": { "n": { "type": "integer", "minimum": -2147483648, "maximum": 2147483647 }, "m": { "type": "integer", "minimum": -2147483648, "maximum": 2147483647 } }, "required": ["n"] }"""
    type Property = JsonSchemaProvider<schema = propertySchema, requireIntegerBounds = true>

    [<Fact>]
    let ``int-ranged properties, required and optional, need no Result`` () =
        let v = Property.Create(n = Int32.MaxValue, m = -3)
        expectEqual v.n Int32.MaxValue
        expectEqual v.m (Some -3)
        expectEqual (Property.Create(n = 1)).m None

    [<Fact>]
    let ``an int-ranged property Parse returns Error outside int instead of reading it throwing`` () =
        expectError (Property.Parse("""{"n": 2147483648}"""))
        expectError (Property.Parse("""{"n": 1, "m": -2147483649}"""))

    [<Literal>]
    let itemsSchema = """{ "type": "array", "items": { "type": "integer", "minimum": -2147483648, "maximum": 2147483647 } }"""
    type Items = JsonSchemaProvider<schema = itemsSchema, requireIntegerBounds = true>

    [<Fact>]
    let ``int-ranged items give an int list and Parse returns Error outside int`` () =
        expectEqual (Items.Create [ Int32.MinValue; Int32.MaxValue ]) [ Int32.MinValue; Int32.MaxValue ]
        expectEqual (expectOk (Items.Parse("[2147483647, -2147483648]"))) [ Int32.MaxValue; Int32.MinValue ]
        expectError (Items.Parse("[1, 2147483648]"))

    [<Literal>]
    let tupleSchema =
        """{ "type": "array", "items": { "type": "integer", "minimum": -2147483648, "maximum": 2147483647 }, "minItems": 2, "maxItems": 2 }"""
    type Tuple = JsonSchemaProvider<schema = tupleSchema, requireIntegerBounds = true>

    [<Fact>]
    let ``int-ranged items work inside an exact-length array`` () =
        expectEqual (Tuple.Create((1, 2))) (1, 2)
        expectError (Tuple.Parse("[1, 2147483648]"))

    [<Literal>]
    let maxItemsSchema =
        """{ "type": "array", "items": { "type": "integer", "minimum": -2147483648, "maximum": 2147483647 }, "maxItems": 2 }"""
    type MaxItems = JsonSchemaProvider<schema = maxItemsSchema, requireIntegerBounds = true>

    [<Fact>]
    let ``int-ranged items work in a maxItems option chain`` () =
        expectEqual (expectOk (MaxItems.Parse("[1]"))) (Some(1, None))
        expectEqual (MaxItems.ToList(Some(1, Some 2))) [ 1; 2 ]
        expectError (MaxItems.Parse("[2147483648]"))

    [<Literal>]
    let uniqueItemsSchema =
        """{ "type": "array", "items": { "type": "integer", "minimum": -2147483648, "maximum": 2147483647 }, "uniqueItems": true }"""
    type UniqueItems = JsonSchemaProvider<schema = uniqueItemsSchema, requireIntegerBounds = true>

    [<Fact>]
    let ``int-ranged items keep uniqueItems dropping duplicates`` () =
        expectEqual (UniqueItems.Create [ 1; 1; 2 ]) [ 1; 2 ]

    [<Literal>]
    let oneOfSchema = """{ "oneOf": [ { "type": "integer", "minimum": -2147483648, "maximum": 2147483647 }, { "type": "string" } ] }"""
    type OneOf = JsonSchemaProvider<schema = oneOfSchema, requireIntegerBounds = true>

    [<Fact>]
    let ``an int-ranged integer works as a oneOf branch and a number outside int matches no branch`` () =
        expectEqual (expectOk (OneOf.Parse("5"))) (Choice1Of2 5)
        expectEqual (expectOk (OneOf.Parse("\"x\""))) (Choice2Of2 "x")
        expectError (OneOf.Parse("2147483648"))

    [<Literal>]
    let nestedSchema =
        """{ "type": "object", "properties": { "inner": { "type": "object", "properties": { "n": { "type": "integer", "minimum": -2147483648, "maximum": 2147483647 } }, "required": ["n"] } }, "required": ["inner"] }"""
    type Nested = JsonSchemaProvider<schema = nestedSchema, requireIntegerBounds = true>

    [<Fact>]
    let ``an int-ranged property of a nested object makes the nested Create need no Result`` () =
        expectEqual (Nested.innerObj.Create(n = 4)).n 4
        expectError (Nested.Parse("""{"inner": {"n": 2147483648}}"""))

    [<Literal>]
    let multipleOfSchema = """{ "type": "integer", "minimum": -2147483648, "maximum": 2147483647, "multipleOf": 2 }"""
    type MultipleOf = JsonSchemaProvider<schema = multipleOfSchema, requireIntegerBounds = true>

    [<Fact>]
    let ``an int-ranged integer with multipleOf keeps int and Create validates multipleOf`` () =
        expectEqual (expectOk (MultipleOf.Create 4)) 4
        expectError (MultipleOf.Create 3)

    [<Literal>]
    let nonNegativeSchema = """{ "type": "integer", "minimum": 0, "maximum": 4294967295 }"""
    type NonNegative = JsonSchemaProvider<schema = nonNegativeSchema, requireIntegerBounds = true>

    [<Fact>]
    let ``the uint32 range is the strict version of minimum 0 alone`` () =
        expectEqual (NonNegative.Create UInt32.MaxValue) UInt32.MaxValue
        expectEqual (expectOk (NonNegative.Parse("3000000000"))) 3000000000u
        expectError (NonNegative.Parse("-1"))
        expectError (NonNegative.Parse("4294967296"))

    [<Literal>]
    let nonNegativeItemsSchema = """{ "type": "array", "items": { "type": "integer", "minimum": 0, "maximum": 4294967295 } }"""
    type NonNegativeItems = JsonSchemaProvider<schema = nonNegativeItemsSchema, requireIntegerBounds = true>

    [<Fact>]
    let ``uint32-ranged items give a uint32 list and Parse returns Error above uint32`` () =
        expectEqual (expectOk (NonNegativeItems.Parse("[0, 3000000000]"))) [ 0u; 3000000000u ]
        expectError (NonNegativeItems.Parse("[4294967296]"))

    // ignoreSpecificKeywords always gives int, so the strict rule only lets through bounds inside int.
    [<Literal>]
    let ignoredSchema = """{ "type": "integer", "minimum": 0, "maximum": 2147483647 }"""
    type Ignored = JsonSchemaProvider<schema = ignoredSchema, ignoreSpecificKeywords = true, requireIntegerBounds = true>

    [<Fact>]
    let ``ignoreSpecificKeywords with bounds inside int gives a validated int`` () =
        expectEqual (expectOk (Ignored.Create 0)) 0
        expectError (Ignored.Create -1)
        expectError (Ignored.Parse("2147483648"))
