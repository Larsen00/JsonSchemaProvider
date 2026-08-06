namespace JsonSchemaProvider.Tests

module OneOfTests =
    open Expecto
    open JsonSchemaProvider

    [<Literal>]
    let stringOrIntSchema =
        """
        {
          "type": "object",
          "properties": {
            "value": {
              "oneOf": [
                {"type": "string"},
                {"type": "integer"}
              ]
            }
          },
          "required": ["value"]
        }"""

    [<Literal>]
    let stringOrIntOrBoolSchema =
        """
        {
          "type": "object",
          "properties": {
            "value": {
              "oneOf": [
                {"type": "string"},
                {"type": "integer"},
                {"type": "boolean"}
              ]
            }
          },
          "required": ["value"]
        }"""

    [<Literal>]
    let stringOrIntArraySchema =
        """
        {
          "type": "object",
          "properties": {
            "value": {
              "oneOf": [
                {"type": "string"},
                {"type": "array", "items": {"type": "integer"}}
              ]
            }
          },
          "required": ["value"]
        }"""

    // Structurally the same alternatives as stringOrIntOrBoolSchema, just grouped as
    // string | (int | bool) instead of string | int | bool - exercises recursion into
    // a nested FSharpOneOf inside generateStructualMatchExpr's head position.
    [<Literal>]
    let nestedOneOfSchema =
        """
        {
          "type": "object",
          "properties": {
            "value": {
              "oneOf": [
                {"type": "string"},
                {
                  "oneOf": [
                    {"type": "integer"},
                    {"type": "boolean"}
                  ]
                }
              ]
            }
          },
          "required": ["value"]
        }"""

    type StringOrInt = JsonSchemaProvider<schema = stringOrIntSchema>
    type StringOrIntOrBool = JsonSchemaProvider<schema = stringOrIntOrBoolSchema>
    type StringOrIntArray = JsonSchemaProvider<schema = stringOrIntArraySchema>
    type NestedOneOf = JsonSchemaProvider<schema = nestedOneOfSchema>

    let parseStringBranchOfTwoWayOneOf =
        test "parse picks string branch of a string|int oneOf" {
            let v = StringOrInt.Parse("""{"value": "hello"}""")
            Expect.equal v.value (Choice1Of2 "hello") "value = Choice1Of2 \"hello\""
        }

    let parseIntBranchOfTwoWayOneOf =
        test "parse picks int branch of a string|int oneOf" {
            let v = StringOrInt.Parse("""{"value": 42}""")
            Expect.equal v.value (Choice2Of2 42) "value = Choice2Of2 42"
        }

    let createStringBranchRoundTrips =
        test "create with string branch round-trips through Parse" {
            let created = StringOrInt.Create(value = Choice1Of2 "abc")
            let reparsed = StringOrInt.Parse(created.ToString())
            Expect.equal reparsed.value (Choice1Of2 "abc") "round-tripped value = Choice1Of2 \"abc\""
        }

    let createIntBranchRoundTrips =
        test "create with int branch round-trips through Parse" {
            let created = StringOrInt.Create(value = Choice2Of2 99)
            let reparsed = StringOrInt.Parse(created.ToString())
            Expect.equal reparsed.value (Choice2Of2 99) "round-tripped value = Choice2Of2 99"
        }

    let parseStringBranchOfThreeWayOneOf =
        test "parse picks string branch of a string|int|bool oneOf" {
            let v = StringOrIntOrBool.Parse("""{"value": "x"}""")
            Expect.equal v.value (Choice1Of2 "x") "value = Choice1Of2 \"x\""
        }

    let parseIntBranchOfThreeWayOneOf =
        test "parse picks int branch of a string|int|bool oneOf" {
            let v = StringOrIntOrBool.Parse("""{"value": 7}""")
            Expect.equal v.value (Choice2Of2(Choice1Of2 7)) "value = Choice2Of2(Choice1Of2 7)"
        }

    let parseBoolBranchOfThreeWayOneOf =
        test "parse picks bool branch of a string|int|bool oneOf" {
            let v = StringOrIntOrBool.Parse("""{"value": true}""")
            Expect.equal v.value (Choice2Of2(Choice2Of2 true)) "value = Choice2Of2(Choice2Of2 true)"
        }

    let parseStringBranchOfStringOrArrayOneOf =
        test "parse picks string branch of a string|array oneOf" {
            let v = StringOrIntArray.Parse("""{"value": "hi"}""")
            Expect.equal v.value (Choice1Of2 "hi") "value = Choice1Of2 \"hi\""
        }

    let parseArrayBranchOfStringOrArrayOneOf =
        test "parse picks array branch of a string|array oneOf" {
            let v = StringOrIntArray.Parse("""{"value": [1, 2, 3]}""")
            Expect.equal v.value (Choice2Of2 [ 1; 2; 3 ]) "value = Choice2Of2 [1;2;3]"
        }

    let nestedOneOfMatchesOuterAlternative =
        test "nested oneOf: outer string alternative matches" {
            let v = NestedOneOf.Parse("""{"value": "s"}""")
            Expect.equal v.value (Choice1Of2 "s") "value = Choice1Of2 \"s\""
        }

    let nestedOneOfMatchesInnerFirstAlternative =
        test "nested oneOf: inner int alternative matches" {
            let v = NestedOneOf.Parse("""{"value": 3}""")
            Expect.equal v.value (Choice2Of2(Choice1Of2 3)) "value = Choice2Of2(Choice1Of2 3)"
        }

    let nestedOneOfMatchesInnerSecondAlternative =
        test "nested oneOf: inner bool alternative matches" {
            let v = NestedOneOf.Parse("""{"value": false}""")
            Expect.equal v.value (Choice2Of2(Choice2Of2 false)) "value = Choice2Of2(Choice2Of2 false)"
        }

    [<Tests>]
    let tests =
        testList
            "JsonSchemaProvider.Tests.OneOfTests"
            [ parseStringBranchOfTwoWayOneOf
              parseIntBranchOfTwoWayOneOf
              createStringBranchRoundTrips
              createIntBranchRoundTrips
              parseStringBranchOfThreeWayOneOf
              parseIntBranchOfThreeWayOneOf
              parseBoolBranchOfThreeWayOneOf
              parseStringBranchOfStringOrArrayOneOf
              parseArrayBranchOfStringOrArrayOneOf
              nestedOneOfMatchesOuterAlternative
              nestedOneOfMatchesInnerFirstAlternative
              nestedOneOfMatchesInnerSecondAlternative ]
