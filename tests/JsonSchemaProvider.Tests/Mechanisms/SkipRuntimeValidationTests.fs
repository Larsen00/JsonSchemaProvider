namespace JsonSchemaProvider.Tests

// The skipRuntimeValidation static parameter: Create/Parse skip schema.Validate entirely, so
// constraint violations pass through instead of becoming Result errors or exceptions.
module SkipRuntimeValidationTests =
    open Expecto
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

    let validObjectStillCreatesNormally =
        test "skipRuntimeValidation preserves normal object creation" {
            let value = UnsafeObject.Create(age = 42, name = "alice", tags = [ "one"; "two" ])
            Expect.equal value.age 42 "the integer property is preserved"
            Expect.equal value.name "alice" "the string property is preserved"
            Expect.equal value.tags [ "one"; "two" ] "the array property is preserved"
        }

    let invalidObjectValuesAreAccepted =
        test "skipRuntimeValidation accepts invalid object property values" {
            let value = UnsafeObject.Create(age = 1, name = "NOT_LOWERCASE", tags = [ "dup"; "dup" ])
            Expect.equal value.age 1 "minimum validation is skipped"
            Expect.equal value.name "NOT_LOWERCASE" "pattern validation is skipped"
            Expect.equal value.tags [ "dup"; "dup" ] "uniqueItems validation is skipped"
        }

    [<Literal>]
    let constrainedIntegerSchema = """{ "type": "integer", "minimum": 10 }"""
    type UnsafeInteger = JsonSchemaProvider<schema = constrainedIntegerSchema, skipRuntimeValidation = true>

    let invalidIntegerIsAccepted =
        test "skipRuntimeValidation accepts an integer below minimum" {
            let value: int = UnsafeInteger.Create 0
            Expect.equal value 0 "the underlying integer is returned directly"
        }

    let constraintViolationIsAcceptedByParse =
        test "skipRuntimeValidation skips schema validation in Parse too" {
            let value = Expect.wantOk (UnsafeInteger.Parse("3")) "minimum validation is skipped"
            Expect.equal value 3 "the below-minimum integer is returned as-is"
        }

    let wrongShapeJsonRaisesDuringParse =
        test "skipRuntimeValidation makes Parse raise on JSON of the wrong shape" {
            Expect.throws
                (fun () -> UnsafeInteger.Parse("\"not an integer\"") |> ignore)
                "no validation runs, so converting a string to int raises"
        }

    let malformedJsonIsStillErrorInParse =
        test "skipRuntimeValidation still reports malformed JSON as Error in Parse" {
            Expect.isError (UnsafeInteger.Parse("{")) "the syntax check still runs"
        }

    [<Literal>]
    let constrainedStringSchema = """{ "type": "string", "pattern": "^[a-z]+$" }"""
    type UnsafeString = JsonSchemaProvider<schema = constrainedStringSchema, skipRuntimeValidation = true>

    let invalidStringIsAccepted =
        test "skipRuntimeValidation accepts a string outside its pattern" {
            let value: string = UnsafeString.Create "NOT_LOWERCASE"
            Expect.equal value "NOT_LOWERCASE" "the underlying string is returned directly"
        }

    [<Literal>]
    let constrainedArraySchema = """{ "type": "array", "items": { "type": "integer" }, "uniqueItems": true }"""
    type UnsafeArray = JsonSchemaProvider<schema = constrainedArraySchema, skipRuntimeValidation = true>

    let validArrayStillCreatesNormally =
        test "skipRuntimeValidation preserves valid primitive arrays" {
            let value: int list = UnsafeArray.Create [ 1; 2; 3 ]
            Expect.equal value [ 1; 2; 3 ] "the integer array is returned directly"
        }

    let invalidArrayIsAccepted =
        test "skipRuntimeValidation accepts an array with duplicate items" {
            let value: int list = UnsafeArray.Create [ 1; 1; 2 ]
            Expect.equal value [ 1; 1; 2 ] "uniqueItems validation is skipped"
        }

    [<Tests>]
    let tests =
        testList
            "JsonSchemaProvider.Tests.SkipRuntimeValidationTests"
            [ validObjectStillCreatesNormally
              invalidObjectValuesAreAccepted
              invalidIntegerIsAccepted
              constraintViolationIsAcceptedByParse
              wrongShapeJsonRaisesDuringParse
              malformedJsonIsStillErrorInParse
              invalidStringIsAccepted
              validArrayStillCreatesNormally
              invalidArrayIsAccepted ]
