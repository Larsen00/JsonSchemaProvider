namespace RefinedJsonSchemaProvider.Tests

// With allowNullStrings=true a string node counts as fully compilable: Create doesn't check for
// null, so it gives the value directly instead of a Result. A null is stored as JSON null and the
// getter reads it back as "" (FSharp.Data's AsString). The default (false) is in NullInputTests.
module AllowNullStringsTests =
    open Xunit
    open FSharp.Data
    open RefinedJsonSchemaProvider

    [<Literal>]
    let requiredStringSchema =
        """{ "type": "object", "properties": { "name": {"type": "string"} }, "required": ["name"] }"""
    type RequiredString = RefinedJsonSchemaProvider<schema=requiredStringSchema, allowNullStrings=true>

    [<Fact>]
    let ``object Create gives the value directly for a required string`` () =
        let v: RequiredString = RequiredString.Create(name = "x")
        expectEqual v.name "x"

    [<Fact>]
    let ``object Create accepts null in a required string without throwing`` () =
        let v = RequiredString.Create(name = null)
        expectEqual v.JsonVal (JsonValue.Record [| "name", JsonValue.Null |])
        expectEqual v.name ""

    [<Literal>]
    let stringRootSchema = """{ "type": "string" }"""
    type StringRoot = RefinedJsonSchemaProvider<schema=stringRootSchema, allowNullStrings=true>

    [<Fact>]
    let ``string root Create gives the value directly`` () =
        let v: string = StringRoot.Create "x"
        expectEqual v "x"

    // A keyword string can't encode still needs validation, so Create keeps its Result.
    [<Literal>]
    let minLengthSchema = """{ "type": "string", "minLength": 2 }"""
    type MinLength = RefinedJsonSchemaProvider<schema=minLengthSchema, allowNullStrings=true>

    [<Fact>]
    let ``a string with minLength still gives a Result`` () =
        expectError (MinLength.Create "x")
        expectOk (MinLength.Create "xy") |> ignore
