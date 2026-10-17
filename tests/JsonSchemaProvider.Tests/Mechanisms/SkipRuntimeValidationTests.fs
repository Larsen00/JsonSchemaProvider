namespace JsonSchemaProvider.Tests

// The skipRuntimeValidation static parameter: Create/Parse skip schema.Validate entirely, so
// constraint violations pass through instead of becoming Result errors or exceptions.
module SkipRuntimeValidationTests =
    open Xunit
    open JsonSchemaProvider

    [<Literal>]
    let constrainedObjectSchema =
        """
        {
          "type": "object",
          "properties": {
            "age": { "type": "integer", "minimum": 18 },
            "name": { "type": "string", "pattern": "^[a-z]+$" },
            "tags": { "type": "array", "items": { "type": "string" }, "uniqueItems": true }
          },
          "required": ["age", "name", "tags"]
        }"""
    type UnsafeObject = JsonSchemaProvider<schema = constrainedObjectSchema, skipRuntimeValidation = true>

    [<Fact>]
    let ``skipRuntimeValidation preserves normal object creation`` () =
        let value = UnsafeObject.Create(age = 42, name = "alice", tags = [ "one"; "two" ])
        expectEqual value.age 42
        expectEqual value.name "alice"
        expectEqual value.tags [ "one"; "two" ]

    [<Fact>]
    let ``skipRuntimeValidation accepts invalid object property values`` () =
        let value = UnsafeObject.Create(age = 1, name = "NOT_LOWERCASE", tags = [ "dup"; "dup" ])
        expectEqual value.age 1
        expectEqual value.name "NOT_LOWERCASE"
        expectEqual value.tags [ "dup"; "dup" ]

    [<Literal>]
    let constrainedIntegerSchema = """{ "type": "integer", "minimum": 10 }"""
    type UnsafeInteger = JsonSchemaProvider<schema = constrainedIntegerSchema, skipRuntimeValidation = true>

    [<Fact>]
    let ``skipRuntimeValidation accepts an integer below minimum`` () =
        let value: int = UnsafeInteger.Create 0
        expectEqual value 0

    [<Fact>]
    let ``skipRuntimeValidation skips schema validation in Parse too`` () =
        let value = expectOk (UnsafeInteger.Parse("3"))
        expectEqual value 3

    [<Fact>]
    let ``skipRuntimeValidation makes Parse raise on JSON of the wrong shape`` () =
        // no validation runs, so converting a string to int raises
        Assert.ThrowsAny<exn>(fun () -> UnsafeInteger.Parse("\"not an integer\"") |> ignore) |> ignore

    [<Fact>]
    let ``skipRuntimeValidation still reports malformed JSON as Error in Parse`` () =
        expectError (UnsafeInteger.Parse("{")) // the syntax check still runs

    [<Literal>]
    let constrainedStringSchema = """{ "type": "string", "pattern": "^[a-z]+$" }"""
    type UnsafeString = JsonSchemaProvider<schema = constrainedStringSchema, skipRuntimeValidation = true>

    [<Fact>]
    let ``skipRuntimeValidation accepts a string outside its pattern`` () =
        let value: string = UnsafeString.Create "NOT_LOWERCASE"
        expectEqual value "NOT_LOWERCASE"

    [<Literal>]
    let constrainedArraySchema = """{ "type": "array", "items": { "type": "integer" }, "uniqueItems": true }"""
    type UnsafeArray = JsonSchemaProvider<schema = constrainedArraySchema, skipRuntimeValidation = true>

    [<Fact>]
    let ``skipRuntimeValidation preserves valid primitive arrays`` () =
        let value: int list = UnsafeArray.Create [ 1; 2; 3 ]
        expectEqual value [ 1; 2; 3 ]

    [<Fact>]
    let ``skipRuntimeValidation accepts an array with duplicate items`` () =
        let value: int list = UnsafeArray.Create [ 1; 1; 2 ]
        expectEqual value [ 1; 1; 2 ]
