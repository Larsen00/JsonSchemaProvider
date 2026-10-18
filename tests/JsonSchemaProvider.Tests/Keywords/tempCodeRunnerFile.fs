    // integer and number overlap: every integer is also a number.
    [<Literal>]
    let integerOrNumberSchema =
        """
        {
            "type": "object",
            "properties": {
                "value": {"oneOf": [{"type": "integer"}, {"type": "number"}]}
            },
            "required": ["value"]
        }
        """
    type IntegerOrNumber = JsonSchemaProvider<schema = integerOrNumberSchema>

    // Create skips validation here (oneOf is treated as fully compilable), so it builds JSON
    // that violates the oneOf, and Parse then rejects its own output.
    [<Fact>]
    let ``integer|number oneOf: Create with integer 5 builds JSON that Parse rejects`` () =
        let created = IntegerOrNumber.Create(value = Choice1Of2 5)
        expectEqual created.value (Choice1Of2 5)
        expectError (IntegerOrNumber.Parse(created.ToString())) // 5 matches both branches

    [<Fact>]
    let ``integer|number oneOf: Create with number 5.0 builds the same JSON as integer 5`` () =
        let fromNumber = IntegerOrNumber.Create(value = Choice2Of2 5.0)
        let fromInteger = IntegerOrNumber.Create(value = Choice1Of2 5)
        expectEqual (fromNumber.ToString()) (fromInteger.ToString())

    [<Fact>]
    let ``integer|number oneOf: Parse rejects 5 because both branches match`` () =
        expectError (IntegerOrNumber.Parse("""{"value": 5}""")) // 5 is an integer and a number

    [<Fact>]
    let ``integer|number oneOf: Parse accepts 5.5 into the number branch`` () =
        let v = expectOk (IntegerOrNumber.Parse("""{"value": 5.5}"""))
        expectEqual v.value (Choice2Of2 5.5)
