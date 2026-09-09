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

    // Branches below are only tellable apart by validating each branch's own constraints, not by
    // JSON kind alone - the old shape-only check (tryInteger/tryFloat/tryString, kind-only) always
    // picked the first branch of matching *kind* regardless of these constraints, so these are
    // the cases that specifically exercise the switch to real per-branch NJsonSchema validation.
    [<Literal>]
    let numberRangeOneOfSchema =
        """
        {
          "type": "object",
          "properties": {
            "value": {
              "oneOf": [
                {"type": "number", "maximum": 0},
                {"type": "number", "minimum": 0.1}
              ]
            }
          },
          "required": ["value"]
        }"""

    [<Literal>]
    let stringPatternOneOfSchema =
        """
        {
          "type": "object",
          "properties": {
            "value": {
              "oneOf": [
                {"type": "string", "pattern": "^[0-9]+$"},
                {"type": "string", "pattern": "^[a-z]+$"}
              ]
            }
          },
          "required": ["value"]
        }"""

    type StringOrInt = JsonSchemaProvider<schema = stringOrIntSchema>
    type StringOrIntOrBool = JsonSchemaProvider<schema = stringOrIntOrBoolSchema>
    type StringOrIntArray = JsonSchemaProvider<schema = stringOrIntArraySchema>
    type NestedOneOf = JsonSchemaProvider<schema = nestedOneOfSchema>
    type NumberRangeOneOf = JsonSchemaProvider<schema = numberRangeOneOfSchema>
    type StringPatternOneOf = JsonSchemaProvider<schema = stringPatternOneOfSchema>

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

    // Old shape-only check (tryFloat, kind-only) would always report Choice1Of2 here, regardless
    // of value, since both branches are the same JSON kind (number).
    let numberRangeOneOfPicksNonPositiveBranch =
        test "number|number oneOf: value <= 0 picks the maximum-0 branch" {
            let v = NumberRangeOneOf.Parse("""{"value": -5.5}""")
            Expect.equal v.value (Choice1Of2 -5.5) "value = Choice1Of2 -5.5"
        }

    let numberRangeOneOfPicksPositiveBranch =
        test "number|number oneOf: value > 0 picks the minimum-0.1 branch" {
            let v = NumberRangeOneOf.Parse("""{"value": 3.2}""")
            Expect.equal v.value (Choice2Of2 3.2) "value = Choice2Of2 3.2"
        }

    let numberRangeOneOfBoundaryPicksInclusiveBranch =
        test "number|number oneOf: the maximum-0 boundary itself picks the inclusive branch" {
            let v = NumberRangeOneOf.Parse("""{"value": 0}""")
            Expect.equal v.value (Choice1Of2 0.0) "0 satisfies maximum: 0 (inclusive) but not minimum: 0.1"
        }

    // 0.05 satisfies neither branch (maximum: 0 nor minimum: 0.1) - not a disambiguation case at
    // all, just confirms the whole document is validated (and rejected) before any oneOf branch
    // selection runs, per Parse's upfront schema.Validate call.
    let numberRangeOneOfGapValueFailsWholeDocumentValidation =
        test "number|number oneOf: a value matching neither branch fails Parse validation" {
            Expect.throws
                (fun () -> NumberRangeOneOf.Parse("""{"value": 0.05}""") |> ignore)
                "0.05 satisfies neither maximum: 0 nor minimum: 0.1"
        }

    // Old shape-only check (tryString, kind-only) would always report Choice1Of2 here, regardless
    // of value, since both branches are the same JSON kind (string).
    let stringPatternOneOfPicksDigitsBranch =
        test "string|string oneOf: digits-only value picks the numeric-pattern branch" {
            let v = StringPatternOneOf.Parse("""{"value": "123"}""")
            Expect.equal v.value (Choice1Of2 "123") "value = Choice1Of2 \"123\""
        }

    let stringPatternOneOfPicksLettersBranch =
        test "string|string oneOf: lowercase-only value picks the alphabetic-pattern branch" {
            let v = StringPatternOneOf.Parse("""{"value": "abc"}""")
            Expect.equal v.value (Choice2Of2 "abc") "value = Choice2Of2 \"abc\""
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
              nestedOneOfMatchesInnerSecondAlternative
              numberRangeOneOfPicksNonPositiveBranch
              numberRangeOneOfPicksPositiveBranch
              numberRangeOneOfBoundaryPicksInclusiveBranch
              numberRangeOneOfGapValueFailsWholeDocumentValidation
              stringPatternOneOfPicksDigitsBranch
              stringPatternOneOfPicksLettersBranch ]
