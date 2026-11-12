namespace JsonSchemaProvider.Tests

// The loosest integer bounds pick the F# type: exactly a type's range gives that type with no
// Result, a range beyond 32 bits widens to int64/uint64, and a redundant looser bound next to a
// stricter one asks for a smaller type while the stricter bound is still validated.
module IntegerTypeTests =
    open System
    open Xunit
    open JsonSchemaProvider

    // ignoreSpecificKeywords always gives int, also for bounds outside int.
    [<Literal>]
    let ignoredWideSchema = """{ "type": "integer", "maximum": 1000000000000 }"""
    type IgnoredWide = JsonSchemaProvider<schema=ignoredWideSchema, ignoreSpecificKeywords=true, allowUnboundedIntegers=true>

    [<Fact>]
    let ``ignoreSpecificKeywords with a bound outside int still gives a validated int`` () =
        expectEqual (expectOk (IgnoredWide.Create 5)) 5

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
        expectEqual (expectOk (SizedOneOf.Parse("-3"))) (Choice2Of2 -3y)
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
    type NoKeywords = JsonSchemaProvider<schema=noKeywordsSchema, allowUnboundedIntegers=true>

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
    type Int64Maximum = JsonSchemaProvider<schema=int64MaximumSchema, allowUnboundedIntegers=true>

    [<Literal>]
    let int64MinimumSchema = """{ "type": "integer", "minimum": -9223372036854775808 }"""
    type Int64Minimum = JsonSchemaProvider<schema=int64MinimumSchema, allowUnboundedIntegers=true>

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

    // NJsonSchema's Validate throws on any integer outside int64 (it can't convert the BigInteger
    // Newtonsoft parses it into), so the provider checks for one first and returns an Error explaining it.
    let private expectNJsonSchemaLimit (number: string) (result: Result<'T, string list>) =
        match result with
        | Error [ message ] ->
            Assert.Contains($"The JSON contains the integer {number}, which is outside the int64 range", message)
            Assert.Contains("Pick ONE of these fixes", message)
            Assert.Contains("\"maximum\": 18446744073709551615 for uint64", message)
            Assert.Contains("B) Or set skipRuntimeValidation=true", message)
        | other -> failwithf "Expected one NJsonSchema-limit Error, got %A" other

    [<Fact>]
    let ``Parse outside int64 is an Error explaining NJsonSchema's limit`` () =
        expectNJsonSchemaLimit "9223372036854775808" (Int64Root.Parse("9223372036854775808"))
        expectNJsonSchemaLimit "-9223372036854775809" (Int64Root.Parse("-9223372036854775809"))

    [<Literal>]
    let uint64PropertySchema =
        """{ "type": "object", "properties": { "id": { "type": "integer", "minimum": 0, "maximum": 18446744073709551615 }, "name": { "type": "string" } } }"""
    type UInt64Property = JsonSchemaProvider<schema=uint64PropertySchema>

    [<Fact>]
    let ``a too-large integer nested in an object is reported by the same Error`` () =
        expectNJsonSchemaLimit "18446744073709551615" (UInt64Property.Parse("""{"id": 18446744073709551615, "name": "x"}"""))

    [<Literal>]
    let uint64Schema = """{ "type": "integer", "minimum": 0, "maximum": 18446744073709551615 }"""
    type UInt64Root = JsonSchemaProvider<schema=uint64Schema>

    [<Fact>]
    let ``the exact uint64 range gives uint64 and Create takes its upper half`` () =
        expectEqual (UInt64Root.Create UInt64.MaxValue) UInt64.MaxValue
        expectEqual (expectOk (UInt64Root.Parse("9223372036854775807"))) 9223372036854775807UL
        // Same NJsonSchema limitation as above: the upper half can be created but not parsed.
        expectNJsonSchemaLimit "18446744073709551615" (UInt64Root.Parse("18446744073709551615"))

    // One below UInt64.MaxValue, so the type doesn't enforce the bound and Create validates.
    [<Literal>]
    let validatedUInt64Schema = """{ "type": "integer", "minimum": 0, "maximum": 18446744073709551614 }"""
    type ValidatedUInt64 = JsonSchemaProvider<schema=validatedUInt64Schema>

    [<Fact>]
    let ``a validating Create in the upper half of uint64 hits the same NJsonSchema limit`` () =
        expectEqual (expectOk (ValidatedUInt64.Create 5UL)) 5UL
        expectNJsonSchemaLimit "18446744073709551614" (ValidatedUInt64.Create 18446744073709551614UL)

    [<Literal>]
    let redundantBoundSchema = """{ "type": "integer", "minimum": 0, "maximum": 11, "exclusiveMaximum": 256 }"""
    type RedundantBound = JsonSchemaProvider<schema=redundantBoundSchema>

    [<Fact>]
    let ``a redundant looser bound gives byte and Create validates the stricter bound`` () =
        expectEqual (expectOk (RedundantBound.Create 11uy)) 11uy
        expectError (RedundantBound.Create 12uy)

    [<Literal>]
    let percentSchema = """{ "type": "integer", "minimum": 1, "maximum": 100 }"""
    type Percent = JsonSchemaProvider<schema=percentSchema>

    [<Fact>]
    let ``a range inside byte gives byte and Create validates both bounds`` () =
        expectEqual (expectOk (Percent.Create 100uy)) 100uy
        expectError (Percent.Create 0uy)
        expectError (Percent.Create 101uy)

    [<Literal>]
    let smallSignedSchema = """{ "type": "integer", "minimum": -1, "maximum": 1000 }"""
    type SmallSigned = JsonSchemaProvider<schema=smallSignedSchema>

    [<Fact>]
    let ``a signed range inside int16 gives int16`` () =
        expectEqual (expectOk (SmallSigned.Parse("-1"))) -1s
        expectError (SmallSigned.Create 1001s)

    [<Literal>]
    let wideUnsignedSchema = """{ "type": "integer", "minimum": 0, "maximum": 10000000000 }"""
    type WideUnsigned = JsonSchemaProvider<schema=wideUnsignedSchema>

    [<Fact>]
    let ``a non-negative range beyond 32 bits widens to uint64`` () =
        expectEqual (expectOk (WideUnsigned.Create 10000000000UL)) 10000000000UL
        expectError (WideUnsigned.Create 10000000001UL)

    [<Literal>]
    let wideSignedSchema = """{ "type": "integer", "minimum": -10000000000 }"""
    type WideSigned = JsonSchemaProvider<schema=wideSignedSchema, allowUnboundedIntegers=true>

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
