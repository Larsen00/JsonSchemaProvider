namespace JsonSchemaProvider.Tests

// The loosest integer bounds pick the F# type: exactly a type's range gives that type with no
// Result, a range beyond 32 bits widens to int64/uint64, and a redundant looser bound next to a
// stricter one asks for a smaller type while the stricter bound is still validated.
module IntegerTypeTests =
    open System
    open Xunit
    open JsonSchemaProvider

    [<Literal>]
    let byteSchema = """{ "type": "integer", "minimum": 0, "maximum": 255 }"""
    type ByteRoot = JsonSchemaProvider<schema=byteSchema>

    [<Fact>]
    let ``the exact byte range gives byte and Create needs no Result`` () =
        expectEqual (ByteRoot.Create 255uy) 255uy
        expectEqual (expectOk (ByteRoot.Parse("255"))) 255uy
        expectError (ByteRoot.Parse("256"))

    [<Literal>]
    let sbyteSchema = """{ "type": "integer", "exclusiveMinimum": -129, "exclusiveMaximum": 128 }"""
    type SByteRoot = JsonSchemaProvider<schema=sbyteSchema>

    [<Fact>]
    let ``the exact sbyte range given with exclusive bounds gives sbyte`` () =
        expectEqual (SByteRoot.Create -128y) -128y
        expectError (SByteRoot.Parse("128"))
        expectError (SByteRoot.Parse("-129"))

    // Skipping validation lets an out-of-range number reach the checked conversion, which throws.
    [<Literal>]
    let skippedByteSchema = """{ "type": "integer", "minimum": 0, "maximum": 255 }"""
    type SkippedByte = JsonSchemaProvider<schema=skippedByteSchema, skipRuntimeValidation=true>

    [<Fact>]
    let ``skipRuntimeValidation byte Parse throws above 255`` () =
        Assert.Throws<OverflowException>(fun () -> SkippedByte.Parse("256") |> ignore) |> ignore

    [<Literal>]
    let byteItemsSchema = """{ "type": "array", "items": { "type": "integer", "minimum": 0, "maximum": 255 }, "uniqueItems": true }"""
    type ByteItems = JsonSchemaProvider<schema=byteItemsSchema>

    [<Fact>]
    let ``byte items give a byte list and uniqueItems still drops duplicates`` () =
        expectEqual (ByteItems.Create [ 1uy; 1uy; 2uy ]) [ 1uy; 2uy ]
        expectError (ByteItems.Parse("[1, 256]"))

    [<Literal>]
    let sizedOneOfSchema =
        """{ "oneOf": [ { "type": "integer", "minimum": 0, "maximum": 255 }, { "type": "integer", "minimum": -128, "maximum": -1 } ] }"""
    type SizedOneOf = JsonSchemaProvider<schema=sizedOneOfSchema>

    [<Fact>]
    let ``sized integers work as oneOf branches`` () =
        expectEqual (expectOk (SizedOneOf.Parse("5"))) (Choice1Of2 5uy)
        expectEqual (expectOk (SizedOneOf.Parse("-3"))) (Choice2Of2 -3)
        expectError (SizedOneOf.Parse("300"))

    [<Literal>]
    let int16Schema = """{ "type": "integer", "minimum": -32768, "maximum": 32767 }"""
    type Int16Root = JsonSchemaProvider<schema=int16Schema>

    [<Fact>]
    let ``the exact int16 range gives int16`` () =
        expectEqual (Int16Root.Create Int16.MinValue) Int16.MinValue
        expectEqual (expectOk (Int16Root.Parse("32767"))) Int16.MaxValue

    [<Literal>]
    let uint16Schema = """{ "type": "integer", "minimum": 0, "maximum": 65535 }"""
    type UInt16Root = JsonSchemaProvider<schema=uint16Schema>

    [<Fact>]
    let ``the exact uint16 range gives uint16`` () =
        expectEqual (UInt16Root.Create UInt16.MaxValue) UInt16.MaxValue

    [<Literal>]
    let uint32Schema = """{ "type": "integer", "minimum": 0, "maximum": 4294967295 }"""
    type UInt32Root = JsonSchemaProvider<schema=uint32Schema>

    [<Fact>]
    let ``the exact uint32 range gives uint32, Create needs no Result and Parse rejects larger numbers`` () =
        expectEqual (UInt32Root.Create UInt32.MaxValue) UInt32.MaxValue
        expectError (UInt32Root.Parse("4294967296"))

    [<Literal>]
    let noKeywordsSchema = """{ "type": "integer" }"""
    type NoKeywords = JsonSchemaProvider<schema=noKeywordsSchema>

    [<Fact>]
    let ``no keywords gives int and Create needs no Result`` () =
        expectEqual (NoKeywords.Create -5) -5

    [<Literal>]
    let int32Schema = """{ "type": "integer", "minimum": -2147483648, "maximum": 2147483647 }"""
    type Int32Root = JsonSchemaProvider<schema=int32Schema>

    [<Fact>]
    let ``the exact int range gives int and Create needs no Result`` () =
        expectEqual (Int32Root.Create Int32.MinValue) Int32.MinValue
        expectError (Int32Root.Parse("2147483648"))

    [<Literal>]
    let int64MaximumSchema = """{ "type": "integer", "maximum": 9223372036854775807 }"""
    type Int64Maximum = JsonSchemaProvider<schema=int64MaximumSchema>

    [<Literal>]
    let int64MinimumSchema = """{ "type": "integer", "minimum": -9223372036854775808 }"""
    type Int64Minimum = JsonSchemaProvider<schema=int64MinimumSchema>

    [<Fact>]
    let ``a one-sided bound at the Int64 limit gives int64 and Create needs no Result`` () =
        expectEqual (Int64Maximum.Create Int64.MinValue) Int64.MinValue
        expectEqual (Int64Minimum.Create Int64.MaxValue) Int64.MaxValue
        expectEqual (expectOk (Int64Maximum.Parse("-9223372036854775808"))) Int64.MinValue

    [<Literal>]
    let int64Schema = """{ "type": "integer", "minimum": -9223372036854775808, "maximum": 9223372036854775807 }"""
    type Int64Root = JsonSchemaProvider<schema=int64Schema>

    [<Fact>]
    let ``the exact int64 range gives int64`` () =
        expectEqual (Int64Root.Create Int64.MinValue) Int64.MinValue
        expectEqual (expectOk (Int64Root.Parse("9223372036854775807"))) Int64.MaxValue

    // NJsonSchema's Validate throws on any integer above Int64.MaxValue (it can't convert the
    // BigInteger Newtonsoft parses it into), so Parse throws there instead of returning Error.
    [<Fact>]
    let ``Parse throws above Int64 MaxValue because NJsonSchema's validation does`` () =
        Assert.Throws<InvalidCastException>(fun () -> Int64Root.Parse("9223372036854775808") |> ignore) |> ignore

    [<Literal>]
    let uint64Schema = """{ "type": "integer", "minimum": 0, "maximum": 18446744073709551615 }"""
    type UInt64Root = JsonSchemaProvider<schema=uint64Schema>

    [<Fact>]
    let ``the exact uint64 range gives uint64 and Create takes its upper half`` () =
        expectEqual (UInt64Root.Create UInt64.MaxValue) UInt64.MaxValue
        expectEqual (expectOk (UInt64Root.Parse("9223372036854775807"))) 9223372036854775807UL
        // Same NJsonSchema limitation as above: the upper half can be created but not parsed.
        Assert.Throws<InvalidCastException>(fun () -> UInt64Root.Parse("18446744073709551615") |> ignore) |> ignore

    [<Literal>]
    let redundantBoundSchema = """{ "type": "integer", "minimum": 0, "maximum": 11, "exclusiveMaximum": 256 }"""
    type RedundantBound = JsonSchemaProvider<schema=redundantBoundSchema>

    [<Fact>]
    let ``a redundant looser bound gives byte and Create validates the stricter bound`` () =
        expectEqual (expectOk (RedundantBound.Create 11uy)) 11uy
        expectError (RedundantBound.Create 12uy)

    [<Literal>]
    let wideUnsignedSchema = """{ "type": "integer", "minimum": 0, "maximum": 10000000000 }"""
    type WideUnsigned = JsonSchemaProvider<schema=wideUnsignedSchema>

    [<Fact>]
    let ``a non-negative range beyond 32 bits widens to uint64`` () =
        expectEqual (expectOk (WideUnsigned.Create 10000000000UL)) 10000000000UL
        expectError (WideUnsigned.Create 10000000001UL)

    [<Literal>]
    let wideSignedSchema = """{ "type": "integer", "minimum": -10000000000 }"""
    type WideSigned = JsonSchemaProvider<schema=wideSignedSchema>

    [<Fact>]
    let ``a signed range beyond 32 bits widens to int64`` () =
        expectEqual (expectOk (WideSigned.Parse("-10000000000"))) -10000000000L
        expectError (WideSigned.Create -10000000001L)

    [<Literal>]
    let propertySchema =
        """{ "type": "object", "properties": { "level": { "type": "integer", "minimum": 0, "maximum": 255 }, "offset": { "type": "integer", "minimum": -32768, "maximum": 32767 } }, "required": ["level"] }"""
    type Property = JsonSchemaProvider<schema=propertySchema>

    [<Fact>]
    let ``sized integer properties, required and optional, need no Result`` () =
        let v = Property.Create(level = 7uy, offset = -3s)
        expectEqual v.level 7uy
        expectEqual v.offset (Some -3s)
        expectEqual (Property.Create(level = 1uy)).offset None
