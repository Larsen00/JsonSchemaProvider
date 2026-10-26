namespace JsonSchemaProvider.Tests

// An integer node maps to int (32-bit), but a JSON integer can be any size. Bounding the node to
// the Int32 range with minimum/maximum makes validation reject everything int can't hold; without
// the bounds, Parse throws on an out-of-range integer instead of returning Error.
module IntegerRangeTests =
    open System
    open Xunit
    open JsonSchemaProvider

    // AsInteger in FSharp.Data throws a plain Exception "Not an int: <n>" when n doesn't fit.
    let expectNotAnInt (parse: unit -> 'a) =
        let ex = Assert.Throws<Exception>(fun () -> parse () |> ignore)
        Assert.StartsWith("Not an int", ex.Message)

    [<Literal>]
    let boundedPropertySchema =
        """{ "type": "object", "properties": { "n": { "type": "integer", "minimum": -2147483648, "maximum": 2147483647 } }, "required": ["n"] }"""
    type BoundedProperty = JsonSchemaProvider<schema=boundedPropertySchema>

    [<Literal>]
    let unboundedPropertySchema =
        """{ "type": "object", "properties": { "n": { "type": "integer" } }, "required": ["n"] }"""
    type UnboundedProperty = JsonSchemaProvider<schema=unboundedPropertySchema>

    [<Fact>]
    let ``bounded property Parse accepts Int32 MaxValue and MinValue`` () =
        expectEqual (expectOk (BoundedProperty.Parse("""{"n": 2147483647}"""))).n Int32.MaxValue
        expectEqual (expectOk (BoundedProperty.Parse("""{"n": -2147483648}"""))).n Int32.MinValue

    [<Fact>]
    let ``bounded property Parse rejects integers just outside Int32`` () =
        expectError (BoundedProperty.Parse("""{"n": 2147483648}"""))
        expectError (BoundedProperty.Parse("""{"n": -2147483649}"""))

    [<Fact>]
    let ``bounded property Create accepts Int32 MaxValue and MinValue`` () =
        expectEqual (expectOk (BoundedProperty.Create(n = Int32.MaxValue))).n Int32.MaxValue
        expectEqual (expectOk (BoundedProperty.Create(n = Int32.MinValue))).n Int32.MinValue

    [<Fact>]
    let ``unbounded property Parse is Ok but reading the property throws outside Int32`` () =
        let tooBig = expectOk (UnboundedProperty.Parse("""{"n": 2147483648}"""))
        expectNotAnInt (fun () -> tooBig.n)
        let tooSmall = expectOk (UnboundedProperty.Parse("""{"n": -2147483649}"""))
        expectNotAnInt (fun () -> tooSmall.n)

    [<Literal>]
    let boundedRootSchema = """{ "type": "integer", "minimum": -2147483648, "maximum": 2147483647 }"""
    type BoundedRoot = JsonSchemaProvider<schema=boundedRootSchema>

    [<Literal>]
    let unboundedRootSchema = """{ "type": "integer" }"""
    type UnboundedRoot = JsonSchemaProvider<schema=unboundedRootSchema>

    [<Fact>]
    let ``bounded root Parse accepts Int32 MaxValue and MinValue`` () =
        expectEqual (expectOk (BoundedRoot.Parse("2147483647"))) Int32.MaxValue
        expectEqual (expectOk (BoundedRoot.Parse("-2147483648"))) Int32.MinValue

    [<Fact>]
    let ``bounded root Parse rejects integers just outside Int32`` () =
        expectError (BoundedRoot.Parse("2147483648"))
        expectError (BoundedRoot.Parse("-2147483649"))

    [<Fact>]
    let ``bounded root Create accepts Int32 MaxValue and MinValue`` () =
        expectEqual (expectOk (BoundedRoot.Create Int32.MaxValue)) Int32.MaxValue
        expectEqual (expectOk (BoundedRoot.Create Int32.MinValue)) Int32.MinValue

    [<Fact>]
    let ``unbounded root Parse throws on integers outside Int32`` () =
        expectNotAnInt (fun () -> UnboundedRoot.Parse("2147483648"))

    [<Literal>]
    let boundedItemsSchema =
        """{ "type": "array", "items": { "type": "integer", "minimum": -2147483648, "maximum": 2147483647 } }"""
    type BoundedItems = JsonSchemaProvider<schema=boundedItemsSchema>

    [<Literal>]
    let unboundedItemsSchema = """{ "type": "array", "items": { "type": "integer" } }"""
    type UnboundedItems = JsonSchemaProvider<schema=unboundedItemsSchema>

    [<Fact>]
    let ``bounded items Parse accepts Int32 MaxValue and MinValue`` () =
        expectEqual (expectOk (BoundedItems.Parse("[2147483647, -2147483648]"))) [ Int32.MaxValue; Int32.MinValue ]

    [<Fact>]
    let ``bounded items Parse rejects integers just outside Int32`` () =
        expectError (BoundedItems.Parse("[1, 2147483648]"))
        expectError (BoundedItems.Parse("[-2147483649]"))

    [<Fact>]
    let ``unbounded items Parse throws on integers outside Int32`` () =
        expectNotAnInt (fun () -> UnboundedItems.Parse("[1, 2147483648]"))
