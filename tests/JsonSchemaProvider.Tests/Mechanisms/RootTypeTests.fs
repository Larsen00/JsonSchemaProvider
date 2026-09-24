namespace JsonSchemaProvider.Tests

// Covers the "any type as root" work in notes/any-type-as-root-refactor.md: a schema whose
// root is a primitive, list or oneOf, not just an object. Create and Parse both work with the
// plain typed value (bool, int, string list, Choice<...>); Parse always wraps it in Result.
module RootTypeTests =
    open Expecto
    open JsonSchemaProvider
    open FSharp.Data

    [<Literal>]
    let boolRootSchema = """{ "type": "boolean" }"""

    [<Literal>]
    let intRootSchema = """{ "type": "integer" }"""

    [<Literal>]
    let numberRootSchema = """{ "type": "number" }"""

    [<Literal>]
    let stringRootSchema = """{ "type": "string" }"""

    [<Literal>]
    let constrainedIntRootSchema =
        """{ "type": "integer", "minimum": 5, "maximum": 10 }"""

    // FSharpString's JsonString.Keywords captures `pattern` as data, but nothing reads it for
    // enforcement yet - this schema exists to confirm primitive-root validation genuinely runs
    // the whole sub-schema through NJsonSchema's own Validate, not something limited to whatever
    // SchemaConversion.fs's FSharpType model happens to act on.
    [<Literal>]
    let patternConstrainedStringRootSchema =
        """{ "type": "string", "pattern": "^[a-z]+$" }"""

    [<Literal>]
    let listRootSchema = """{ "type": "array", "items": { "type": "string" } }"""

    [<Literal>]
    let listRootMinItems2Schema =
        """{ "type": "array", "items": { "type": "string" }, "minItems": 2 }"""

    // A more complex root: a list of objects, not just a list of primitives.
    [<Literal>]
    let placeListRootSchema =
        """
        {
          "type": "array",
          "items": {
            "type": "object",
            "properties": {
              "name": { "type": "string" },
              "lat": { "type": "number" },
              "lng": { "type": "number" }
            },
            "required": ["name", "lat", "lng"]
          }
        }"""

    type BoolRoot = JsonSchemaProvider<schema=boolRootSchema>
    type IntRoot = JsonSchemaProvider<schema=intRootSchema>
    type NumberRoot = JsonSchemaProvider<schema=numberRootSchema>
    type StringRoot = JsonSchemaProvider<schema=stringRootSchema>
    type ConstrainedIntRoot = JsonSchemaProvider<schema=constrainedIntRootSchema>
    type PatternConstrainedStringRoot = JsonSchemaProvider<schema=patternConstrainedStringRootSchema>
    type ListRoot = JsonSchemaProvider<schema=listRootSchema>
    type ListRootMinItems2 = JsonSchemaProvider<schema=listRootMinItems2Schema>

    // Compile-time constrained: root value is string * string * string list
    type ListRootMinItems2Compiled = JsonSchemaProvider<schema=listRootMinItems2Schema, ignoreSpecificKeywords = true>

    type PlaceListRoot = JsonSchemaProvider<schema=placeListRootSchema>

    // string and integer are both FullyCompilable with no constraints, so this oneOf node itself
    // is FullyCompilable too - Create returns the bare Choice<string, int> directly, not Result.
    [<Literal>]
    let stringOrIntRootSchema =
        """{ "oneOf": [{ "type": "string" }, { "type": "integer" }] }"""

    // 3-way oneOf root - exercises the nested Choice<T1, Choice<T2, T3>> shape at the root.
    [<Literal>]
    let stringOrIntOrBoolRootSchema =
        """{ "oneOf": [{ "type": "string" }, { "type": "integer" }, { "type": "boolean" }] }"""

    // One branch is an object - exercises buildClassMapHelper/extractNestedClasses generating a
    // nested class ("Case", same suffix rule as any other oneOf branch) directly off a root that
    // is itself FSharpOneOf, not reached through a property this time.
    [<Literal>]
    let placeOrIdRootSchema =
        """
        {
          "oneOf": [
            {
              "type": "object",
              "properties": { "name": { "type": "string" } },
              "required": ["name"]
            },
            { "type": "integer" }
          ]
        }"""

    // One branch is an array - exercises the FSharpList-inside-FSharpOneOf-root combination.
    [<Literal>]
    let stringOrIntArrayRootSchema =
        """{ "oneOf": [{ "type": "string" }, { "type": "array", "items": { "type": "integer" } }] }"""

    // minimum:5 makes the integer branch not FullyCompilable, so the oneOf node itself isn't
    // either - Create returns Result<Choice<int,string>, string list> here, unlike the schemas
    // above. A value satisfying neither branch (e.g. 3: fails minimum, isn't a string at all)
    // fails oneOf's "exactly one branch matches" rule and comes back as Error.
    [<Literal>]
    let constrainedIntOrStringRootSchema =
        """{ "oneOf": [{ "type": "integer", "minimum": 5 }, { "type": "string" }] }"""

    // A oneOf root whose second branch is itself a oneOf - string | (int | bool), same shape as
    // OneOfTests.fs's nestedOneOfSchema but with no wrapping object/property, so this is the
    // literal FSharpOneOf(_, _, [FSharpOneOf _]) root shape.
    [<Literal>]
    let nestedOneOfRootSchema =
        """
        {
          "oneOf": [
            { "type": "string" },
            { "oneOf": [{ "type": "integer" }, { "type": "boolean" }] }
          ]
        }"""

    type StringOrIntRoot = JsonSchemaProvider<schema=stringOrIntRootSchema>
    type StringOrIntOrBoolRoot = JsonSchemaProvider<schema=stringOrIntOrBoolRootSchema>
    type PlaceOrIdRoot = JsonSchemaProvider<schema=placeOrIdRootSchema>
    type StringOrIntArrayRoot = JsonSchemaProvider<schema=stringOrIntArrayRootSchema>
    type ConstrainedIntOrStringRoot = JsonSchemaProvider<schema=constrainedIntOrStringRootSchema>
    type NestedOneOfRoot = JsonSchemaProvider<schema=nestedOneOfRootSchema>

    let boolRootShouldBeCreated =
        test "boolean root Create builds the value" {
            let value = BoolRoot.Create true
            Expect.equal  value true "BoolRoot.Create(true).JsonVal = true"
        }

    let boolRootShouldBeParsed =
        test "boolean root Parse builds the value" {
            let result = Expect.wantOk (BoolRoot.Parse("true")) "Parse should succeed"
            Expect.equal result true "BoolRoot.Parse(\"true\") = Ok true"
        }

    let intRootShouldBeCreated =
        test "integer root Create builds the value" {
            let value = IntRoot.Create 42
            Expect.equal value 42 "IntRoot.Create(42).JsonVal = 42"
        }

    let intRootShouldBeParsed =
        test "integer root Parse builds the value" {
            let result = Expect.wantOk (IntRoot.Parse("42")) "Parse should succeed"
            Expect.equal result 42 "IntRoot.Parse(\"42\") = Ok 42"
        }

    let numberRootShouldBeCreated =
        test "number root Create builds the value" {
            let value = NumberRoot.Create 3.14
            Expect.equal value 3.14 "NumberRoot.Create(3.14).JsonVal = 3.14"
        }

    let numberRootShouldBeParsed =
        test "number root Parse builds the value" {
            let result = Expect.wantOk (NumberRoot.Parse("3.14")) "Parse should succeed"
            Expect.equal result 3.14 "NumberRoot.Parse(\"3.14\") = Ok 3.14"
        }

    let stringRootShouldBeCreated =
        test "string root Create builds the value" {
            let value = StringRoot.Create "hello"
            Expect.equal value "hello" "StringRoot.Create(\"hello\").JsonVal = \"hello\""
        }

    let stringRootShouldBeParsed =
        test "string root Parse builds the value" {
            let result = Expect.wantOk (StringRoot.Parse("\"hello\"")) "Parse should succeed"
            Expect.equal result "hello" "StringRoot.Parse of a JSON string literal = Ok \"hello\""
        }

    // minimum/maximum are known keywords (JsonNumber.Specific has fields for them) but nothing
    // compiles a range into the type - int is int regardless - so this node is never
    // FullyCompilable and Create still returns Result, unlike the unconstrained roots above.
    let inRangeConstrainedIntRootShouldBeAccepted =
        test "in-range integer root value is accepted by Create" {
            let value = Expect.wantOk (ConstrainedIntRoot.Create(7)) "Create should succeed"
            Expect.equal value 7 "ConstrainedIntRoot.Create(7).JsonVal = 7"
        }

    let belowMinimumConstrainedIntRootShouldBeRejectedByCreate =
        test "below-minimum integer root value is rejected by Create" {
            Expect.isError (ConstrainedIntRoot.Create(3)) "Create should reject a root value below minimum"
        }

    let aboveMaximumConstrainedIntRootShouldBeRejectedByCreate =
        test "above-maximum integer root value is rejected by Create" {
            Expect.isError (ConstrainedIntRoot.Create(20)) "Create should reject a root value above maximum"
        }

    let belowMinimumConstrainedIntRootShouldBeRejectedByParse =
        test "below-minimum integer root value is rejected by Parse" {
            Expect.isError
                (ConstrainedIntRoot.Parse("3"))
                "Parse should reject a root value below minimum"
        }

    let matchingPatternStringRootShouldBeAccepted =
        test "string root value matching pattern is accepted by Create" {
            let value = Expect.wantOk (PatternConstrainedStringRoot.Create("hello")) "Create should succeed"
            Expect.equal value "hello" "PatternConstrainedStringRoot.Create(\"hello\") should be accepted"
        }

    let nonMatchingPatternStringRootShouldBeRejected =
        test "string root value not matching pattern is rejected by Create" {
            Expect.isError
                (PatternConstrainedStringRoot.Create("HELLO"))
                "Create should reject a root value that doesn't match the pattern"
        }

    // Nothing in placeListRootSchema is unmodeled or left uncompiled (name/lat/lng are plain
    // string/number with no constraints, no additionalProperties/patternProperties, no
    // minItems/maxItems/uniqueItems on the array) - so the list root itself is FullyCompilable
    // and Create returns List<Item> directly, not Result.
    let emptyPlaceListRootShouldBeCreated =
        test "array-of-objects root Create builds an empty list" {
            let result = PlaceListRoot.Create([])
            Expect.equal (List.length result) 0 "PlaceListRoot.Create([]) has no elements"
        }

    // The array's item class is exposed as PlaceListRoot.Item (suffix "Item" for classes reached
    // through a list, vs "Obj" for a nested object property) - see TypeProvider.fs's
    // buildClassMapHelper/extractNestedClasses. At the root there's no property name to prefix the
    // suffix with (unlike a property-nested list, e.g. valuesItem), so it's just "Item". This is the
    // case the empty-list test above can't cover: constructing actual elements, not just an empty list.
    //
    let nonEmptyPlaceListRootShouldBeCreated =
        test "array-of-objects root Create builds a non-empty list" {
            let place = PlaceListRoot.Item.Create(name = "Copenhagen", lat = 55.6761, lng = 12.5683)
            let result = PlaceListRoot.Create([ place ])
            Expect.equal (List.length result) 1 "one element"
            Expect.equal result.[0].name "Copenhagen" "name roundtrips"
            Expect.equal result.[0].lat 55.6761 "lat roundtrips"
            Expect.equal result.[0].lng 12.5683 "lng roundtrips"
        }

    let stringOrIntRootShouldBeCreatedFromStringBranch =
        test "oneOf root Create builds the value from the string branch" {
            let value = StringOrIntRoot.Create(value = Choice1Of2 "hello")
            Expect.equal value (Choice1Of2 "hello") "StringOrIntRoot.Create(Choice1Of2 \"hello\")"
        }

    let stringOrIntRootShouldBeCreatedFromIntBranch =
        test "oneOf root Create builds the value from the int branch" {
            let value = StringOrIntRoot.Create(value = Choice2Of2 42)
            Expect.equal value (Choice2Of2 42) "StringOrIntRoot.Create(Choice2Of2 42)"
        }

    let threeWayOneOfRootShouldBeCreatedFromStringBranch =
        test "3-way oneOf root Create builds the value from the string branch" {
            let value = StringOrIntOrBoolRoot.Create(value = Choice1Of2 "x")
            Expect.equal value (Choice1Of2 "x") "StringOrIntOrBoolRoot.Create(Choice1Of2 \"x\")"
        }

    let threeWayOneOfRootShouldBeCreatedFromIntBranch =
        test "3-way oneOf root Create builds the value from the int branch" {
            let value = StringOrIntOrBoolRoot.Create(value = Choice2Of2(Choice1Of2 7))
            Expect.equal value (Choice2Of2(Choice1Of2 7)) "StringOrIntOrBoolRoot.Create(Choice2Of2(Choice1Of2 7))"
        }

    let threeWayOneOfRootShouldBeCreatedFromBoolBranch =
        test "3-way oneOf root Create builds the value from the bool branch" {
            let value = StringOrIntOrBoolRoot.Create(value = Choice2Of2(Choice2Of2 true))
            Expect.equal value (Choice2Of2(Choice2Of2 true)) "StringOrIntOrBoolRoot.Create(Choice2Of2(Choice2Of2 true))"
        }

    // The object branch's generated class is named "Case" (empty root name + "Case" suffix, same
    // rule as PlaceListRoot.Item above) and is FullyCompilable (an unconstrained string property),
    // so PlaceOrIdRoot.Case.Create returns the class directly, not Result.
    let oneOfRootWithObjectBranchShouldBeCreatedFromObjectBranch =
        test "oneOf root with an object branch Create builds the value from the object branch" {
            let place = PlaceOrIdRoot.Case.Create(name = "Copenhagen")
            let value = PlaceOrIdRoot.Create(value = Choice1Of2 place)
            match value with
            | Choice1Of2 case -> Expect.equal case.name "Copenhagen" "name roundtrips"
            | Choice2Of2 _ -> failtest "expected the object branch (Choice1Of2)"
        }

    let oneOfRootWithObjectBranchShouldBeCreatedFromIntBranch =
        test "oneOf root with an object branch Create builds the value from the int branch" {
            let value = PlaceOrIdRoot.Create(value = Choice2Of2 5)
            Expect.equal value (Choice2Of2 5) "PlaceOrIdRoot.Create(Choice2Of2 5)"
        }

    let oneOfRootWithArrayBranchShouldBeCreatedFromStringBranch =
        test "oneOf root with an array branch Create builds the value from the string branch" {
            let value = StringOrIntArrayRoot.Create(value = Choice1Of2 "hi")
            Expect.equal value (Choice1Of2 "hi") "StringOrIntArrayRoot.Create(Choice1Of2 \"hi\")"
        }

    let oneOfRootWithArrayBranchShouldBeCreatedFromArrayBranch =
        test "oneOf root with an array branch Create builds the value from the array branch" {
            let value = StringOrIntArrayRoot.Create(value = Choice2Of2 [ 1; 2; 3 ])
            Expect.equal value (Choice2Of2 [ 1; 2; 3 ]) "StringOrIntArrayRoot.Create(Choice2Of2 [1;2;3])"
        }

    let constrainedOneOfRootShouldAcceptInRangeIntBranch =
        test "constrained oneOf root Create accepts an in-range int branch value" {
            let value = Expect.wantOk (ConstrainedIntOrStringRoot.Create(value = Choice1Of2 7)) "Create should succeed"
            Expect.equal value (Choice1Of2 7) "ConstrainedIntOrStringRoot.Create(Choice1Of2 7)"
        }

    let constrainedOneOfRootShouldAcceptStringBranch =
        test "constrained oneOf root Create accepts the string branch" {
            let value =
                Expect.wantOk (ConstrainedIntOrStringRoot.Create(value = Choice2Of2 "hello")) "Create should succeed"
            Expect.equal value (Choice2Of2 "hello") "ConstrainedIntOrStringRoot.Create(Choice2Of2 \"hello\")"
        }

    let constrainedOneOfRootShouldRejectBelowMinimumIntBranch =
        test "constrained oneOf root Create rejects a below-minimum int branch value" {
            Expect.isError
                (ConstrainedIntOrStringRoot.Create(value = Choice1Of2 3))
                "3 satisfies neither the minimum:5 int branch nor the string branch"
        }

    let nestedOneOfRootShouldBeCreatedFromOuterStringBranch =
        test "nested oneOf root Create builds the value from the outer string branch" {
            let value = NestedOneOfRoot.Create(value = Choice1Of2 "s")
            Expect.equal value (Choice1Of2 "s") "NestedOneOfRoot.Create(Choice1Of2 \"s\")"
        }

    let nestedOneOfRootShouldBeCreatedFromInnerIntBranch =
        test "nested oneOf root Create builds the value from the inner int branch" {
            let value = NestedOneOfRoot.Create(value = Choice2Of2(Choice1Of2 3))
            Expect.equal value (Choice2Of2(Choice1Of2 3)) "NestedOneOfRoot.Create(Choice2Of2(Choice1Of2 3))"
        }

    let nestedOneOfRootShouldBeCreatedFromInnerBoolBranch =
        test "nested oneOf root Create builds the value from the inner bool branch" {
            let value = NestedOneOfRoot.Create(value = Choice2Of2(Choice2Of2 false))
            Expect.equal value (Choice2Of2(Choice2Of2 false)) "NestedOneOfRoot.Create(Choice2Of2(Choice2Of2 false))"
        }

    // List and oneOf roots get Parse the same way primitive roots do: validate the whole document
    // against the root schema, then evaluate to the same typed value Create takes.
    let listRootShouldBeParsed =
        test "array root Parse builds the value" {
            let result = Expect.wantOk (ListRoot.Parse("""["a", "b"]""")) "Parse should succeed"
            Expect.equal result [ "a"; "b" ] "ListRoot.Parse evaluates to the typed string list"
        }

    let listRootParseShouldRejectInvalidItem =
        test "array root Parse rejects an item of the wrong type" {
            Expect.isError
                (ListRoot.Parse("""["a", 1]"""))
                "Parse should reject a non-string item"
        }

    let placeListRootShouldBeParsed =
        test "array-of-objects root Parse builds the value" {
            let result = Expect.wantOk (PlaceListRoot.Parse("""[{"name": "Copenhagen", "lat": 55.6761, "lng": 12.5683}]""")) "Parse should succeed"
            Expect.equal result.[0].name "Copenhagen" "name roundtrips through the typed Item class"
        }

    let stringOrIntRootShouldBeParsed =
        test "oneOf root Parse builds the value" {
            let result = Expect.wantOk (StringOrIntRoot.Parse("42")) "Parse should succeed"
            Expect.equal result (Choice2Of2 42) "StringOrIntRoot.Parse(\"42\") picks the int branch"
        }

    let constrainedOneOfRootParseShouldRejectNoMatchingBranch =
        test "oneOf root Parse rejects a value matching no branch" {
            Expect.isError
                (ConstrainedIntOrStringRoot.Parse("3"))
                "Parse should reject a value matching no oneOf branch"
        }

    let malformedJsonParseShouldBeError =
        test "Parse of syntactically malformed JSON evaluates to Error instead of raising" {
            Expect.isError (ListRoot.Parse("""["a", """)) "malformed JSON should be an Error"
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
              emptyPlaceListRootShouldBeCreated
              nonEmptyPlaceListRootShouldBeCreated
              stringOrIntRootShouldBeCreatedFromStringBranch
              stringOrIntRootShouldBeCreatedFromIntBranch
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
              nestedOneOfRootShouldBeCreatedFromOuterStringBranch
              nestedOneOfRootShouldBeCreatedFromInnerIntBranch
              nestedOneOfRootShouldBeCreatedFromInnerBoolBranch
              listRootShouldBeParsed
              listRootParseShouldRejectInvalidItem
              placeListRootShouldBeParsed
              stringOrIntRootShouldBeParsed
              constrainedOneOfRootParseShouldRejectNoMatchingBranch
              malformedJsonParseShouldBeError ]
