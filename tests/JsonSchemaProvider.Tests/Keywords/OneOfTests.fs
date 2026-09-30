namespace JsonSchemaProvider.Tests

module OneOfTests =
    open Expecto
    open JsonSchemaProvider

    [<Literal>]
    let stringOrIntSchema =
        """{ "type": "object", "properties": { "value": { "oneOf": [{"type": "string"}, {"type": "integer"}] } }, "required": ["value"] }"""
    type StringOrInt = JsonSchemaProvider<schema = stringOrIntSchema>

    let parseStringBranchOfTwoWayOneOf =
        test "parse picks string branch of a string|int oneOf" {
            let v = Expect.wantOk (StringOrInt.Parse("""{"value": "hello"}""")) "Parse should succeed"
            Expect.equal v.value (Choice1Of2 "hello") "value = Choice1Of2 \"hello\""
        }

    let parseIntBranchOfTwoWayOneOf =
        test "parse picks int branch of a string|int oneOf" {
            let v = Expect.wantOk (StringOrInt.Parse("""{"value": 42}""")) "Parse should succeed"
            Expect.equal v.value (Choice2Of2 42) "value = Choice2Of2 42"
        }

    let createStringBranchRoundTrips =
        test "create with string branch round-trips through Parse" {
            let created = StringOrInt.Create(value = Choice1Of2 "abc")
            let reparsed = Expect.wantOk (StringOrInt.Parse(created.ToString())) "Parse should succeed"
            Expect.equal reparsed.value (Choice1Of2 "abc") "round-tripped value = Choice1Of2 \"abc\""
        }

    let createIntBranchRoundTrips =
        test "create with int branch round-trips through Parse" {
            let created = StringOrInt.Create(value = Choice2Of2 99)
            let reparsed = Expect.wantOk (StringOrInt.Parse(created.ToString())) "Parse should succeed"
            Expect.equal reparsed.value (Choice2Of2 99) "round-tripped value = Choice2Of2 99"
        }

    [<Literal>]
    let stringOrIntOrBoolSchema =
        """{ "type": "object", "properties": { "value": { "oneOf": [{"type": "string"}, {"type": "integer"}, {"type": "boolean"}] } }, "required": ["value"] }"""
    type StringOrIntOrBool = JsonSchemaProvider<schema = stringOrIntOrBoolSchema>

    let parseStringBranchOfThreeWayOneOf =
        test "parse picks string branch of a string|int|bool oneOf" {
            let v = Expect.wantOk (StringOrIntOrBool.Parse("""{"value": "x"}""")) "Parse should succeed"
            Expect.equal v.value (Choice1Of2 "x") "value = Choice1Of2 \"x\""
        }

    let parseIntBranchOfThreeWayOneOf =
        test "parse picks int branch of a string|int|bool oneOf" {
            let v = Expect.wantOk (StringOrIntOrBool.Parse("""{"value": 7}""")) "Parse should succeed"
            Expect.equal v.value (Choice2Of2(Choice1Of2 7)) "value = Choice2Of2(Choice1Of2 7)"
        }

    let parseBoolBranchOfThreeWayOneOf =
        test "parse picks bool branch of a string|int|bool oneOf" {
            let v = Expect.wantOk (StringOrIntOrBool.Parse("""{"value": true}""")) "Parse should succeed"
            Expect.equal v.value (Choice2Of2(Choice2Of2 true)) "value = Choice2Of2(Choice2Of2 true)"
        }

    [<Literal>]
    let stringOrIntArraySchema =
        """{ "type": "object", "properties": { "value": { "oneOf": [{"type": "string"}, {"type": "array", "items": {"type": "integer"}}] } }, "required": ["value"] }"""
    type StringOrIntArray = JsonSchemaProvider<schema = stringOrIntArraySchema>

    let parseStringBranchOfStringOrArrayOneOf =
        test "parse picks string branch of a string|array oneOf" {
            let v = Expect.wantOk (StringOrIntArray.Parse("""{"value": "hi"}""")) "Parse should succeed"
            Expect.equal v.value (Choice1Of2 "hi") "value = Choice1Of2 \"hi\""
        }

    let parseArrayBranchOfStringOrArrayOneOf =
        test "parse picks array branch of a string|array oneOf" {
            let v = Expect.wantOk (StringOrIntArray.Parse("""{"value": [1, 2, 3]}""")) "Parse should succeed"
            Expect.equal v.value (Choice2Of2 [ 1; 2; 3 ]) "value = Choice2Of2 [1;2;3]"
        }

    // string | (int | bool) instead of a flat 3-way oneOf - exercises a nested JsonOneOf.
    [<Literal>]
    let nestedOneOfSchema =
        """{ "type": "object", "properties": { "value": { "oneOf": [{"type": "string"}, {"oneOf": [{"type": "integer"}, {"type": "boolean"}]}] } }, "required": ["value"] }"""
    type NestedOneOf = JsonSchemaProvider<schema = nestedOneOfSchema>

    let nestedOneOfMatchesOuterAlternative =
        test "nested oneOf: outer string alternative matches" {
            let v = Expect.wantOk (NestedOneOf.Parse("""{"value": "s"}""")) "Parse should succeed"
            Expect.equal v.value (Choice1Of2 "s") "value = Choice1Of2 \"s\""
        }

    let nestedOneOfMatchesInnerFirstAlternative =
        test "nested oneOf: inner int alternative matches" {
            let v = Expect.wantOk (NestedOneOf.Parse("""{"value": 3}""")) "Parse should succeed"
            Expect.equal v.value (Choice2Of2(Choice1Of2 3)) "value = Choice2Of2(Choice1Of2 3)"
        }

    let nestedOneOfMatchesInnerSecondAlternative =
        test "nested oneOf: inner bool alternative matches" {
            let v = Expect.wantOk (NestedOneOf.Parse("""{"value": false}""")) "Parse should succeed"
            Expect.equal v.value (Choice2Of2(Choice2Of2 false)) "value = Choice2Of2(Choice2Of2 false)"
        }

    // Same JSON kind (number) on both branches - only per-branch validation, not shape alone, can
    // disambiguate these.
    [<Literal>]
    let numberRangeOneOfSchema =
        """{ "type": "object", "properties": { "value": { "oneOf": [{"type": "number", "maximum": 0}, {"type": "number", "minimum": 0.1}] } }, "required": ["value"] }"""
    type NumberRangeOneOf = JsonSchemaProvider<schema = numberRangeOneOfSchema>

    let numberRangeOneOfPicksNonPositiveBranch =
        test "number|number oneOf: value <= 0 picks the maximum-0 branch" {
            let v = Expect.wantOk (NumberRangeOneOf.Parse("""{"value": -5.5}""")) "Parse should succeed"
            Expect.equal v.value (Choice1Of2 -5.5) "value = Choice1Of2 -5.5"
        }

    let numberRangeOneOfPicksPositiveBranch =
        test "number|number oneOf: value > 0 picks the minimum-0.1 branch" {
            let v = Expect.wantOk (NumberRangeOneOf.Parse("""{"value": 3.2}""")) "Parse should succeed"
            Expect.equal v.value (Choice2Of2 3.2) "value = Choice2Of2 3.2"
        }

    let numberRangeOneOfBoundaryPicksInclusiveBranch =
        test "number|number oneOf: the maximum-0 boundary itself picks the inclusive branch" {
            let v = Expect.wantOk (NumberRangeOneOf.Parse("""{"value": 0}""")) "Parse should succeed"
            Expect.equal v.value (Choice1Of2 0.0) "0 satisfies maximum: 0 but not minimum: 0.1"
        }

    let numberRangeOneOfGapValueFailsWholeDocumentValidation =
        test "number|number oneOf: a value matching neither branch fails Parse validation" {
            Expect.isError (NumberRangeOneOf.Parse("""{"value": 0.05}""")) "0.05 satisfies neither branch"
        }

    [<Literal>]
    let stringPatternOneOfSchema =
        """{ "type": "object", "properties": { "value": { "oneOf": [{"type": "string", "pattern": "^[0-9]+$"}, {"type": "string", "pattern": "^[a-z]+$"}] } }, "required": ["value"] }"""
    type StringPatternOneOf = JsonSchemaProvider<schema = stringPatternOneOfSchema>

    let stringPatternOneOfPicksDigitsBranch =
        test "string|string oneOf: digits-only value picks the numeric-pattern branch" {
            let v = Expect.wantOk (StringPatternOneOf.Parse("""{"value": "123"}""")) "Parse should succeed"
            Expect.equal v.value (Choice1Of2 "123") "value = Choice1Of2 \"123\""
        }

    let stringPatternOneOfPicksLettersBranch =
        test "string|string oneOf: lowercase-only value picks the alphabetic-pattern branch" {
            let v = Expect.wantOk (StringPatternOneOf.Parse("""{"value": "abc"}""")) "Parse should succeed"
            Expect.equal v.value (Choice2Of2 "abc") "value = Choice2Of2 \"abc\""
        }

    // Both branches are arrays - only the item schema tells them apart.
    [<Literal>]
    let arrayItemTypeOneOfSchema =
        """{ "type": "object", "properties": { "value": { "oneOf": [{"type": "array", "items": {"type": "string"}}, {"type": "array", "items": {"type": "integer"}}] } }, "required": ["value"] }"""
    type ArrayItemTypeOneOf = JsonSchemaProvider<schema = arrayItemTypeOneOfSchema>

    let arrayItemTypeOneOfPicksStringBranch =
        test "array|array oneOf: string items pick the string-item branch" {
            let v = Expect.wantOk (ArrayItemTypeOneOf.Parse("""{"value": ["a", "b"]}""")) "Parse should succeed"
            Expect.equal v.value (Choice1Of2 [ "a"; "b" ]) "value = Choice1Of2 [\"a\";\"b\"]"
        }

    let arrayItemTypeOneOfPicksIntBranch =
        test "array|array oneOf: integer items pick the integer-item branch" {
            let v = Expect.wantOk (ArrayItemTypeOneOf.Parse("""{"value": [1, 2, 3]}""")) "Parse should succeed"
            Expect.equal v.value (Choice2Of2 [ 1; 2; 3 ]) "value = Choice2Of2 [1;2;3]"
        }

    let arrayItemTypeOneOfAmbiguousEmptyArrayFailsWholeDocumentValidation =
        test "array|array oneOf: an empty array matches both branches and fails Parse validation" {
            Expect.isError (ArrayItemTypeOneOf.Parse("""{"value": []}""")) "[] satisfies both item schemas"
        }

    // maxItems:2 and minItems:3 partition array length with no gap and no overlap.
    [<Literal>]
    let arrayLengthOneOfSchema =
        """{ "type": "object", "properties": { "value": { "oneOf": [{"type": "array", "items": {"type": "integer"}, "maxItems": 2}, {"type": "array", "items": {"type": "integer"}, "minItems": 3}] } }, "required": ["value"] }"""
    type ArrayLengthOneOf = JsonSchemaProvider<schema = arrayLengthOneOfSchema>

    let arrayLengthOneOfPicksShortBranch =
        test "array|array oneOf: a 2-element array picks the maxItems:2 branch" {
            let v = Expect.wantOk (ArrayLengthOneOf.Parse("""{"value": [1, 2]}""")) "Parse should succeed"
            Expect.equal v.value (Choice1Of2(Some(1, Some 2))) "value = Choice1Of2 (Some (1, Some 2))"
        }

    let arrayLengthOneOfPicksLongBranch =
        test "array|array oneOf: a 3-element array picks the minItems:3 branch" {
            let v = Expect.wantOk (ArrayLengthOneOf.Parse("""{"value": [1, 2, 3]}""")) "Parse should succeed"
            Expect.equal v.value (Choice2Of2(1, (2, (3, [])))) "value = Choice2Of2 [1;2;3]"
        }

    // -- ToList/ToTuple for arrays inside oneOf: branch i gets a "<name>Case<i>Array" helper type --

    let arrayLengthOneOfEachBranchHasOwnToList =
        test "array|array oneOf: valueCase1Array/valueCase2Array.ToList flatten their own branch" {
            let toList (v: ArrayLengthOneOf) =
                match v.value with
                | Choice1Of2 short -> ArrayLengthOneOf.valueCase1Array.ToList short
                | Choice2Of2 long -> ArrayLengthOneOf.valueCase2Array.ToList long
            let short = Expect.wantOk (ArrayLengthOneOf.Parse("""{"value": [1, 2]}""")) "Parse should succeed"
            let long = Expect.wantOk (ArrayLengthOneOf.Parse("""{"value": [1, 2, 3, 4]}""")) "Parse should succeed"
            Expect.equal (toList short, toList long) ([ 1; 2 ], [ 1; 2; 3; 4 ]) "both branches flatten"
        }

    [<Literal>]
    let rootStringOrExact2ArraySchema =
        """{ "oneOf": [{"type": "string"}, {"type": "array", "items": {"type": "integer"}, "minItems": 2, "maxItems": 2}] }"""
    type RootStringOrExact2Array = JsonSchemaProvider<schema = rootStringOrExact2ArraySchema>

    let rootOneOfArrayBranchHasHelpers =
        test "root string|array oneOf: Case2Array.ToTuple flattens the array branch" {
            Expect.equal (RootStringOrExact2Array.Case2Array.ToTuple((1, 2))) (1, 2) "ToTuple = (1, 2)"
        }

    [<Literal>]
    let arrayOfStringOrMin1ArraySchema =
        """{ "type": "object", "properties": { "values": { "type": "array", "items": { "oneOf": [{"type": "string"}, {"type": "array", "items": {"type": "integer"}, "minItems": 1}] } } }, "required": ["values"] }"""
    type ArrayOfStringOrMin1Array = JsonSchemaProvider<schema = arrayOfStringOrMin1ArraySchema>

    let arrayItemOneOfArrayBranchHasHelpers =
        test "array of string|array oneOf items: valuesArray.ItemCase2Array.ToList flattens each array item" {
            let v = Expect.wantOk (ArrayOfStringOrMin1Array.Parse("""{"values": ["a", [1, 2]]}""")) "Parse should succeed"
            let flattened =
                v.values
                |> List.choose (function
                    | Choice2Of2 arr -> Some(ArrayOfStringOrMin1Array.valuesArray.ItemCase2Array.ToList arr)
                    | Choice1Of2 _ -> None)
            Expect.equal flattened [ [ 1; 2 ] ] "only the array item, flattened"
        }

    // No const-discriminated schema: NJsonSchema doesn't enforce `const` at all. enum is the
    // keyword NJsonSchema does enforce for a value-only distinction between same-kind branches.
    [<Literal>]
    let enumOneOfSchema =
        """{ "type": "object", "properties": { "value": { "oneOf": [{"type": "string", "enum": ["red", "green", "blue"]}, {"type": "string", "enum": ["circle", "square"]}] } }, "required": ["value"] }"""
    type EnumOneOf = JsonSchemaProvider<schema = enumOneOfSchema>

    let enumOneOfPicksColorBranch =
        test "string|string oneOf: a color enum value picks the color branch" {
            let v = Expect.wantOk (EnumOneOf.Parse("""{"value": "red"}""")) "Parse should succeed"
            Expect.equal v.value (Choice1Of2 "red") "value = Choice1Of2 \"red\""
        }

    let enumOneOfPicksShapeBranch =
        test "string|string oneOf: a shape enum value picks the shape branch" {
            let v = Expect.wantOk (EnumOneOf.Parse("""{"value": "circle"}""")) "Parse should succeed"
            Expect.equal v.value (Choice2Of2 "circle") "value = Choice2Of2 \"circle\""
        }

    let enumOneOfRejectsValueInNeitherEnum =
        test "string|string oneOf: a value in neither enum fails Parse validation" {
            Expect.isError (EnumOneOf.Parse("""{"value": "banana"}""")) "\"banana\" is in neither enum"
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
              stringPatternOneOfPicksLettersBranch
              arrayItemTypeOneOfPicksStringBranch
              arrayItemTypeOneOfPicksIntBranch
              arrayItemTypeOneOfAmbiguousEmptyArrayFailsWholeDocumentValidation
              arrayLengthOneOfPicksShortBranch
              arrayLengthOneOfPicksLongBranch
              arrayLengthOneOfEachBranchHasOwnToList
              rootOneOfArrayBranchHasHelpers
              arrayItemOneOfArrayBranchHasHelpers
              enumOneOfPicksColorBranch
              enumOneOfPicksShapeBranch
              enumOneOfRejectsValueInNeitherEnum ]
