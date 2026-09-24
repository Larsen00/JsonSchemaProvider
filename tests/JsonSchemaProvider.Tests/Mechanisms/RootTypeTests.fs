namespace JsonSchemaProvider.Tests

// A schema whose root is a primitive, list, or oneOf, not just an object. Create and Parse both
// work with the plain typed value (bool, int, string list, Choice<...>); Parse always wraps it
// in Result.
module RootTypeTests =
    open Expecto
    open JsonSchemaProvider

    [<Literal>]
    let boolRootSchema = """{ "type": "boolean" }"""
    type BoolRoot = JsonSchemaProvider<schema=boolRootSchema>

    let boolRootShouldBeCreated =
        test "boolean root Create builds the value" {
            Expect.equal (BoolRoot.Create true) true "BoolRoot.Create(true) = true"
        }

    let boolRootShouldBeParsed =
        test "boolean root Parse builds the value" {
            let result = Expect.wantOk (BoolRoot.Parse("true")) "Parse should succeed"
            Expect.equal result true "BoolRoot.Parse(\"true\") = Ok true"
        }

    [<Literal>]
    let intRootSchema = """{ "type": "integer" }"""
    type IntRoot = JsonSchemaProvider<schema=intRootSchema>

    let intRootShouldBeCreated =
        test "integer root Create builds the value" {
            Expect.equal (IntRoot.Create 42) 42 "IntRoot.Create(42) = 42"
        }

    let intRootShouldBeParsed =
        test "integer root Parse builds the value" {
            let result = Expect.wantOk (IntRoot.Parse("42")) "Parse should succeed"
            Expect.equal result 42 "IntRoot.Parse(\"42\") = Ok 42"
        }

    [<Literal>]
    let numberRootSchema = """{ "type": "number" }"""
    type NumberRoot = JsonSchemaProvider<schema=numberRootSchema>

    let numberRootShouldBeCreated =
        test "number root Create builds the value" {
            Expect.equal (NumberRoot.Create 3.14) 3.14 "NumberRoot.Create(3.14) = 3.14"
        }

    let numberRootShouldBeParsed =
        test "number root Parse builds the value" {
            let result = Expect.wantOk (NumberRoot.Parse("3.14")) "Parse should succeed"
            Expect.equal result 3.14 "NumberRoot.Parse(\"3.14\") = Ok 3.14"
        }

    [<Literal>]
    let stringRootSchema = """{ "type": "string" }"""
    type StringRoot = JsonSchemaProvider<schema=stringRootSchema>

    let stringRootShouldBeCreated =
        test "string root Create builds the value" {
            Expect.equal (StringRoot.Create "hello") "hello" "StringRoot.Create(\"hello\") = \"hello\""
        }

    let stringRootShouldBeParsed =
        test "string root Parse builds the value" {
            let result = Expect.wantOk (StringRoot.Parse("\"hello\"")) "Parse should succeed"
            Expect.equal result "hello" "StringRoot.Parse of a JSON string literal = Ok \"hello\""
        }

    // minimum/maximum don't compile into the type (int stays int), so this root is never
    // FullyCompilable and Create returns Result, unlike the unconstrained roots above.
    [<Literal>]
    let constrainedIntRootSchema = """{ "type": "integer", "minimum": 5, "maximum": 10 }"""
    type ConstrainedIntRoot = JsonSchemaProvider<schema=constrainedIntRootSchema>

    let inRangeConstrainedIntRootShouldBeAccepted =
        test "in-range integer root value is accepted by Create" {
            let value = Expect.wantOk (ConstrainedIntRoot.Create(7)) "Create should succeed"
            Expect.equal value 7 "ConstrainedIntRoot.Create(7) = 7"
        }

    let belowMinimumConstrainedIntRootShouldBeRejectedByCreate =
        test "below-minimum integer root value is rejected by Create" {
            Expect.isError (ConstrainedIntRoot.Create(3)) "below minimum"
        }

    let aboveMaximumConstrainedIntRootShouldBeRejectedByCreate =
        test "above-maximum integer root value is rejected by Create" {
            Expect.isError (ConstrainedIntRoot.Create(20)) "above maximum"
        }

    let belowMinimumConstrainedIntRootShouldBeRejectedByParse =
        test "below-minimum integer root value is rejected by Parse" {
            Expect.isError (ConstrainedIntRoot.Parse("3")) "below minimum"
        }

    // Confirms primitive-root validation runs the whole sub-schema through NJsonSchema's own
    // Validate, not just whatever SchemaConversion.fs's FSharpType model happens to act on.
    [<Literal>]
    let patternConstrainedStringRootSchema = """{ "type": "string", "pattern": "^[a-z]+$" }"""
    type PatternConstrainedStringRoot = JsonSchemaProvider<schema=patternConstrainedStringRootSchema>

    let matchingPatternStringRootShouldBeAccepted =
        test "string root value matching pattern is accepted by Create" {
            let value = Expect.wantOk (PatternConstrainedStringRoot.Create("hello")) "Create should succeed"
            Expect.equal value "hello" "accepted"
        }

    let nonMatchingPatternStringRootShouldBeRejected =
        test "string root value not matching pattern is rejected by Create" {
            Expect.isError (PatternConstrainedStringRoot.Create("HELLO")) "doesn't match the pattern"
        }

    [<Literal>]
    let listRootSchema = """{ "type": "array", "items": { "type": "string" } }"""
    type ListRoot = JsonSchemaProvider<schema=listRootSchema>

    // A minItems/maxItems-constrained list root is covered in Keywords/ArrayKeywordTests.fs, not
    // here - this file is about root support existing at all.
    let listRootShouldBeParsed =
        test "array root Parse builds the value" {
            let result = Expect.wantOk (ListRoot.Parse("""["a", "b"]""")) "Parse should succeed"
            Expect.equal result [ "a"; "b" ] "evaluates to the typed string list"
        }

    let listRootParseShouldRejectInvalidItem =
        test "array root Parse rejects an item of the wrong type" {
            Expect.isError (ListRoot.Parse("""["a", 1]""")) "non-string item"
        }

    let malformedJsonParseShouldBeError =
        test "Parse of syntactically malformed JSON evaluates to Error instead of raising" {
            Expect.isError (ListRoot.Parse("""["a", """)) "malformed JSON should be an Error"
        }

    // A list of objects, not just of primitives - the item class is exposed as PlaceListRoot.Item
    // ("Item" suffix, vs "Obj" for a nested object property; no property name to prefix at the root).
    [<Literal>]
    let placeListRootSchema =
        """
        {
          "type": "array",
          "items": {
            "type": "object",
            "properties": { "name": { "type": "string" }, "lat": { "type": "number" }, "lng": { "type": "number" } },
            "required": ["name", "lat", "lng"]
          }
        }"""
    type PlaceListRoot = JsonSchemaProvider<schema=placeListRootSchema>

    let emptyPlaceListRootShouldBeCreated =
        test "array-of-objects root Create builds an empty list" {
            Expect.equal (List.length (PlaceListRoot.Create([]))) 0 "no elements"
        }

    let nonEmptyPlaceListRootShouldBeCreated =
        test "array-of-objects root Create builds a non-empty list" {
            let place = PlaceListRoot.Item.Create(name = "Copenhagen", lat = 55.6761, lng = 12.5683)
            let result = PlaceListRoot.Create([ place ])
            Expect.equal (List.length result) 1 "one element"
            Expect.equal result.[0].name "Copenhagen" "name roundtrips"
            Expect.equal result.[0].lat 55.6761 "lat roundtrips"
            Expect.equal result.[0].lng 12.5683 "lng roundtrips"
        }

    let placeListRootShouldBeParsed =
        test "array-of-objects root Parse builds the value" {
            let result = Expect.wantOk (PlaceListRoot.Parse("""[{"name": "Copenhagen", "lat": 55.6761, "lng": 12.5683}]""")) "Parse should succeed"
            Expect.equal result.[0].name "Copenhagen" "name roundtrips through the typed Item class"
        }

    // Both unconstrained, so this oneOf root is FullyCompilable - Create returns the bare
    // Choice<string, int> directly, not Result.
    [<Literal>]
    let stringOrIntRootSchema = """{ "oneOf": [{ "type": "string" }, { "type": "integer" }] }"""
    type StringOrIntRoot = JsonSchemaProvider<schema=stringOrIntRootSchema>

    let stringOrIntRootShouldBeCreatedFromStringBranch =
        test "oneOf root Create builds the value from the string branch" {
            Expect.equal (StringOrIntRoot.Create(value = Choice1Of2 "hello")) (Choice1Of2 "hello") "string branch"
        }

    let stringOrIntRootShouldBeCreatedFromIntBranch =
        test "oneOf root Create builds the value from the int branch" {
            Expect.equal (StringOrIntRoot.Create(value = Choice2Of2 42)) (Choice2Of2 42) "int branch"
        }

    let stringOrIntRootShouldBeParsed =
        test "oneOf root Parse builds the value" {
            let result = Expect.wantOk (StringOrIntRoot.Parse("42")) "Parse should succeed"
            Expect.equal result (Choice2Of2 42) "picks the int branch"
        }

    [<Literal>]
    let stringOrIntOrBoolRootSchema = """{ "oneOf": [{ "type": "string" }, { "type": "integer" }, { "type": "boolean" }] }"""
    type StringOrIntOrBoolRoot = JsonSchemaProvider<schema=stringOrIntOrBoolRootSchema>

    let threeWayOneOfRootShouldBeCreatedFromStringBranch =
        test "3-way oneOf root Create builds the value from the string branch" {
            Expect.equal (StringOrIntOrBoolRoot.Create(value = Choice1Of2 "x")) (Choice1Of2 "x") "string branch"
        }

    let threeWayOneOfRootShouldBeCreatedFromIntBranch =
        test "3-way oneOf root Create builds the value from the int branch" {
            Expect.equal (StringOrIntOrBoolRoot.Create(value = Choice2Of2(Choice1Of2 7))) (Choice2Of2(Choice1Of2 7)) "int branch"
        }

    let threeWayOneOfRootShouldBeCreatedFromBoolBranch =
        test "3-way oneOf root Create builds the value from the bool branch" {
            Expect.equal (StringOrIntOrBoolRoot.Create(value = Choice2Of2(Choice2Of2 true))) (Choice2Of2(Choice2Of2 true)) "bool branch"
        }

    // One branch is an object - the generated class is named "Case" (empty root name + "Case"
    // suffix) and, being an unconstrained string property, is FullyCompilable.
    [<Literal>]
    let placeOrIdRootSchema =
        """{ "oneOf": [{ "type": "object", "properties": { "name": { "type": "string" } }, "required": ["name"] }, { "type": "integer" }] }"""
    type PlaceOrIdRoot = JsonSchemaProvider<schema=placeOrIdRootSchema>

    let oneOfRootWithObjectBranchShouldBeCreatedFromObjectBranch =
        test "oneOf root with an object branch Create builds the value from the object branch" {
            let place = PlaceOrIdRoot.Case.Create(name = "Copenhagen")
            match PlaceOrIdRoot.Create(value = Choice1Of2 place) with
            | Choice1Of2 case -> Expect.equal case.name "Copenhagen" "name roundtrips"
            | Choice2Of2 _ -> failtest "expected the object branch (Choice1Of2)"
        }

    let oneOfRootWithObjectBranchShouldBeCreatedFromIntBranch =
        test "oneOf root with an object branch Create builds the value from the int branch" {
            Expect.equal (PlaceOrIdRoot.Create(value = Choice2Of2 5)) (Choice2Of2 5) "int branch"
        }

    [<Literal>]
    let stringOrIntArrayRootSchema = """{ "oneOf": [{ "type": "string" }, { "type": "array", "items": { "type": "integer" } }] }"""
    type StringOrIntArrayRoot = JsonSchemaProvider<schema=stringOrIntArrayRootSchema>

    let oneOfRootWithArrayBranchShouldBeCreatedFromStringBranch =
        test "oneOf root with an array branch Create builds the value from the string branch" {
            Expect.equal (StringOrIntArrayRoot.Create(value = Choice1Of2 "hi")) (Choice1Of2 "hi") "string branch"
        }

    let oneOfRootWithArrayBranchShouldBeCreatedFromArrayBranch =
        test "oneOf root with an array branch Create builds the value from the array branch" {
            Expect.equal (StringOrIntArrayRoot.Create(value = Choice2Of2 [ 1; 2; 3 ])) (Choice2Of2 [ 1; 2; 3 ]) "array branch"
        }

    // minimum:5 makes the int branch (and so the oneOf node itself) not FullyCompilable - Create
    // returns Result here, unlike the schemas above.
    [<Literal>]
    let constrainedIntOrStringRootSchema = """{ "oneOf": [{ "type": "integer", "minimum": 5 }, { "type": "string" }] }"""
    type ConstrainedIntOrStringRoot = JsonSchemaProvider<schema=constrainedIntOrStringRootSchema>

    let constrainedOneOfRootShouldAcceptInRangeIntBranch =
        test "constrained oneOf root Create accepts an in-range int branch value" {
            let value = Expect.wantOk (ConstrainedIntOrStringRoot.Create(value = Choice1Of2 7)) "Create should succeed"
            Expect.equal value (Choice1Of2 7) "in-range int"
        }

    let constrainedOneOfRootShouldAcceptStringBranch =
        test "constrained oneOf root Create accepts the string branch" {
            let value = Expect.wantOk (ConstrainedIntOrStringRoot.Create(value = Choice2Of2 "hello")) "Create should succeed"
            Expect.equal value (Choice2Of2 "hello") "string branch"
        }

    let constrainedOneOfRootShouldRejectBelowMinimumIntBranch =
        test "constrained oneOf root Create rejects a below-minimum int branch value" {
            Expect.isError (ConstrainedIntOrStringRoot.Create(value = Choice1Of2 3)) "3 satisfies neither branch"
        }

    let constrainedOneOfRootParseShouldRejectNoMatchingBranch =
        test "oneOf root Parse rejects a value matching no branch" {
            Expect.isError (ConstrainedIntOrStringRoot.Parse("3")) "matches no oneOf branch"
        }

    // string | (int | bool) - same shape as OneOfTests.fs's nestedOneOfSchema but at the root.
    [<Literal>]
    let nestedOneOfRootSchema = """{ "oneOf": [{ "type": "string" }, { "oneOf": [{ "type": "integer" }, { "type": "boolean" }] }] }"""
    type NestedOneOfRoot = JsonSchemaProvider<schema=nestedOneOfRootSchema>

    let nestedOneOfRootShouldBeCreatedFromOuterStringBranch =
        test "nested oneOf root Create builds the value from the outer string branch" {
            Expect.equal (NestedOneOfRoot.Create(value = Choice1Of2 "s")) (Choice1Of2 "s") "outer string branch"
        }

    let nestedOneOfRootShouldBeCreatedFromInnerIntBranch =
        test "nested oneOf root Create builds the value from the inner int branch" {
            Expect.equal (NestedOneOfRoot.Create(value = Choice2Of2(Choice1Of2 3))) (Choice2Of2(Choice1Of2 3)) "inner int branch"
        }

    let nestedOneOfRootShouldBeCreatedFromInnerBoolBranch =
        test "nested oneOf root Create builds the value from the inner bool branch" {
            Expect.equal (NestedOneOfRoot.Create(value = Choice2Of2(Choice2Of2 false))) (Choice2Of2(Choice2Of2 false)) "inner bool branch"
        }

    [<Tests>]
    let tests =
        testList
            "JsonSchemaProvider.Tests.RootTypeTests"
            [ boolRootShouldBeCreated
              boolRootShouldBeParsed
              intRootShouldBeCreated
              intRootShouldBeParsed
              numberRootShouldBeCreated
              numberRootShouldBeParsed
              stringRootShouldBeCreated
              stringRootShouldBeParsed
              inRangeConstrainedIntRootShouldBeAccepted
              belowMinimumConstrainedIntRootShouldBeRejectedByCreate
              aboveMaximumConstrainedIntRootShouldBeRejectedByCreate
              belowMinimumConstrainedIntRootShouldBeRejectedByParse
              matchingPatternStringRootShouldBeAccepted
              nonMatchingPatternStringRootShouldBeRejected
              listRootShouldBeParsed
              listRootParseShouldRejectInvalidItem
              malformedJsonParseShouldBeError
              emptyPlaceListRootShouldBeCreated
              nonEmptyPlaceListRootShouldBeCreated
              placeListRootShouldBeParsed
              stringOrIntRootShouldBeCreatedFromStringBranch
              stringOrIntRootShouldBeCreatedFromIntBranch
              stringOrIntRootShouldBeParsed
              threeWayOneOfRootShouldBeCreatedFromStringBranch
              threeWayOneOfRootShouldBeCreatedFromIntBranch
              threeWayOneOfRootShouldBeCreatedFromBoolBranch
              oneOfRootWithObjectBranchShouldBeCreatedFromObjectBranch
              oneOfRootWithObjectBranchShouldBeCreatedFromIntBranch
              oneOfRootWithArrayBranchShouldBeCreatedFromStringBranch
              oneOfRootWithArrayBranchShouldBeCreatedFromArrayBranch
              constrainedOneOfRootShouldAcceptInRangeIntBranch
              constrainedOneOfRootShouldAcceptStringBranch
              constrainedOneOfRootShouldRejectBelowMinimumIntBranch
              constrainedOneOfRootParseShouldRejectNoMatchingBranch
              nestedOneOfRootShouldBeCreatedFromOuterStringBranch
              nestedOneOfRootShouldBeCreatedFromInnerIntBranch
              nestedOneOfRootShouldBeCreatedFromInnerBoolBranch ]
