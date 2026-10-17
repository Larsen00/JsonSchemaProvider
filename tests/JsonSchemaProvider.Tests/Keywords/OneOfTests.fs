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
                "value": {"oneOf": [{"type": "string"}, {"type": "integer"}]}
            },
            "required": ["value"]
        }
        """
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
        }
        """
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
        }
        """
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
        """
        {
            "type": "object",
            "properties": {
                "value": {
                    "oneOf": [
                        {"type": "string"},
                        {"oneOf": [{"type": "integer"}, {"type": "boolean"}]}
                    ]
                }
            },
            "required": ["value"]
        }
        """
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
        }
        """
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
        }
        """
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
        """
        {
            "type": "object",
            "properties": {
                "value": {
                    "oneOf": [
                        {"type": "array", "items": {"type": "string"}},
                        {"type": "array", "items": {"type": "integer"}}
                    ]
                }
            },
            "required": ["value"]
        }
        """
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
        """
        {
            "type": "object",
            "properties": {
                "value": {
                    "oneOf": [
                        {
                            "type": "array",
                            "items": {"type": "integer"},
                            "maxItems": 2
                        },
                        {
                            "type": "array",
                            "items": {"type": "integer"},
                            "minItems": 3
                        }
                    ]
                }
            },
            "required": ["value"]
        }
        """
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
        """
        {
            "oneOf": [
                {"type": "string"},
                {
                    "type": "array",
                    "items": {"type": "integer"},
                    "minItems": 2,
                    "maxItems": 2
                }
            ]
        }
        """
    type RootStringOrExact2Array = JsonSchemaProvider<schema = rootStringOrExact2ArraySchema>

    let rootOneOfArrayBranchHasHelpers =
        test "root string|array oneOf: Case2Array.ToTuple flattens the array branch" {
            Expect.equal (RootStringOrExact2Array.Case2Array.ToTuple((1, 2))) (1, 2) "ToTuple = (1, 2)"
        }

    [<Literal>]
    let arrayOfStringOrMin1ArraySchema =
        """
        {
            "type": "object",
            "properties": {
                "values": {
                    "type": "array",
                    "items": {
                        "oneOf": [
                            {"type": "string"},
                            {
                                "type": "array",
                                "items": {"type": "integer"},
                                "minItems": 1
                            }
                        ]
                    }
                }
            },
            "required": ["values"]
        }
        """
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
        """
        {
            "type": "object",
            "properties": {
                "value": {
                    "oneOf": [
                        {"type": "string", "enum": ["red", "green", "blue"]},
                        {"type": "string", "enum": ["circle", "square"]}
                    ]
                }
            },
            "required": ["value"]
        }
        """
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

    [<Literal>]
    let ownerSchema =
        """
        {
            "type": "object",
            "properties": {
                "owner": {
                    "oneOf": [
                        {"type": "string"},
                        {"type": "integer"},
                        {
                            "type": "object",
                            "properties": {"name": {"type": "string"}, "email": {"type": "string"}}
                        }
                    ]
                }
            },
            "required": ["owner"]
        }
        """
    type Owner = JsonSchemaProvider<schema = ownerSchema>

    // #region oneof-describe
    let describe (owner: Choice<string, Choice<int, Owner.ownerCase3>>) =
        match owner with
        | Choice1Of2 name -> $"name {name}"
        | Choice2Of2 (Choice1Of2 id) -> $"id {id}"
        | Choice2Of2 (Choice2Of2 contact) -> $"contact {contact}"
    // #endregion

    let describeOwner json =
        let v = Expect.wantOk (Owner.Parse(json)) "Parse should succeed"
        describe v.owner

    let describeMatchesEachOwnerBranch =
        test "string|int|object oneOf: describe matches the branch Parse picked" {
            Expect.equal (describeOwner """{"owner": "Ada"}""") "name Ada" "string branch"
            Expect.equal (describeOwner """{"owner": 7}""") "id 7" "int branch"
            Expect.stringStarts (describeOwner """{"owner": {"name": "Ada", "email": "ada@example.com"}}""") "contact {" "object branch prints the JSON object"
        }

    // A single branch is not wrapped in a Choice: the property gets the branch type directly.
    [<Literal>]
    let singleBranchSchema =
        """
        {
            "type": "object",
            "properties": {
                "value": {"oneOf": [{"type": "integer"}]}
            },
            "required": ["value"]
        }
        """
    type SingleBranch = JsonSchemaProvider<schema = singleBranchSchema>

    let singleBranchParsesToPlainType =
        test "single-branch oneOf: Parse gives the branch type, not a Choice" {
            let v = Expect.wantOk (SingleBranch.Parse("""{"value": 5}""")) "Parse should succeed"
            let n: int = v.value
            Expect.equal n 5 "value = 5"
        }

    let singleBranchCreateRoundTrips =
        test "single-branch oneOf: Create round-trips through Parse" {
            let created = SingleBranch.Create(value = 9)
            let reparsed = Expect.wantOk (SingleBranch.Parse(created.ToString())) "Parse should succeed"
            Expect.equal reparsed.value 9 "round-tripped value = 9"
        }

    let singleBranchRejectsOtherKind =
        test "single-branch oneOf: a value outside the only branch fails Parse validation" {
            Expect.isError (SingleBranch.Parse("""{"value": "five"}""")) "a string does not match the integer branch"
        }

    // Eight branches of the same kind, told apart only by enum. Choice nests seven levels deep.
    [<Literal>]
    let eightEnumBranchesSchema =
        """
        {
            "type": "object",
            "properties": {
                "value": {
                    "oneOf": [
                        {"type": "string", "enum": ["a"]},
                        {"type": "string", "enum": ["b"]},
                        {"type": "string", "enum": ["c"]},
                        {"type": "string", "enum": ["d"]},
                        {"type": "string", "enum": ["e"]},
                        {"type": "string", "enum": ["f"]},
                        {"type": "string", "enum": ["g"]},
                        {"type": "string", "enum": ["h"]}
                    ]
                }
            },
            "required": ["value"]
        }
        """
    type EightEnumBranches = JsonSchemaProvider<schema = eightEnumBranchesSchema>

    let parseEightEnum json =
        (Expect.wantOk (EightEnumBranches.Parse(json)) "Parse should succeed").value

    let eightEnumBranchesPickFirst =
        test "8-way enum oneOf: \"a\" picks the first branch" {
            Expect.equal (parseEightEnum """{"value": "a"}""") (Choice1Of2 "a") "value = Choice1Of2 \"a\""
        }

    let eightEnumBranchesPickMiddle =
        test "8-way enum oneOf: \"d\" picks the fourth branch" {
            Expect.equal
                (parseEightEnum """{"value": "d"}""")
                (Choice2Of2(Choice2Of2(Choice2Of2(Choice1Of2 "d"))))
                "value is the fourth branch"
        }

    let eightEnumBranchesPickSeventh =
        test "8-way enum oneOf: \"g\" picks the seventh branch, the last Choice1Of2" {
            Expect.equal
                (parseEightEnum """{"value": "g"}""")
                (Choice2Of2(Choice2Of2(Choice2Of2(Choice2Of2(Choice2Of2(Choice2Of2(Choice1Of2 "g")))))))
                "value is the seventh branch"
        }

    let eightEnumBranchesPickLast =
        test "8-way enum oneOf: \"h\" picks the eighth branch, the innermost Choice2Of2" {
            Expect.equal
                (parseEightEnum """{"value": "h"}""")
                (Choice2Of2(Choice2Of2(Choice2Of2(Choice2Of2(Choice2Of2(Choice2Of2(Choice2Of2 "h")))))))
                "value is the eighth branch"
        }

    let eightEnumBranchesRejectValueInNoBranch =
        test "8-way enum oneOf: a value in no enum fails Parse validation" {
            Expect.isError (EightEnumBranches.Parse("""{"value": "z"}""")) "\"z\" is in no branch's enum"
        }

    // Eight branches of mixed kinds: every JSON kind except null, with integers split into ranges.
    [<Literal>]
    let eightMixedBranchesSchema =
        """
        {
            "type": "object",
            "properties": {
                "value": {
                    "oneOf": [
                        {"type": "string"},
                        {"type": "boolean"},
                        {"type": "integer", "maximum": 0},
                        {"type": "integer", "minimum": 1, "maximum": 9},
                        {"type": "integer", "minimum": 10},
                        {"type": "array", "items": {"type": "string"}},
                        {"type": "array", "items": {"type": "integer"}},
                        {
                            "type": "object",
                            "properties": {"x": {"type": "integer"}},
                            "required": ["x"]
                        }
                    ]
                }
            },
            "required": ["value"]
        }
        """
    type EightMixedBranches = JsonSchemaProvider<schema = eightMixedBranchesSchema>

    let describeMixed json =
        let v = Expect.wantOk (EightMixedBranches.Parse(json)) "Parse should succeed"
        match v.value with
        | Choice1Of2 s -> $"string {s}"
        | Choice2Of2 (Choice1Of2 b) -> $"boolean {b}"
        | Choice2Of2 (Choice2Of2 (Choice1Of2 n)) -> $"non-positive {n}"
        | Choice2Of2 (Choice2Of2 (Choice2Of2 (Choice1Of2 n))) -> $"digit {n}"
        | Choice2Of2 (Choice2Of2 (Choice2Of2 (Choice2Of2 (Choice1Of2 n)))) -> $"large {n}"
        | Choice2Of2 (Choice2Of2 (Choice2Of2 (Choice2Of2 (Choice2Of2 (Choice1Of2 xs))))) -> $"strings %A{xs}"
        | Choice2Of2 (Choice2Of2 (Choice2Of2 (Choice2Of2 (Choice2Of2 (Choice2Of2 (Choice1Of2 xs)))))) -> $"ints %A{xs}"
        | Choice2Of2 (Choice2Of2 (Choice2Of2 (Choice2Of2 (Choice2Of2 (Choice2Of2 (Choice2Of2 o)))))) -> $"object x={o.x}"

    let eightMixedBranchesPickEachBranch =
        test "8-way mixed oneOf: Parse picks the matching branch for each kind and range" {
            Expect.equal (describeMixed """{"value": "s"}""") "string s" "branch 1"
            Expect.equal (describeMixed """{"value": true}""") "boolean True" "branch 2"
            Expect.equal (describeMixed """{"value": -3}""") "non-positive -3" "branch 3"
            Expect.equal (describeMixed """{"value": 4}""") "digit 4" "branch 4"
            Expect.equal (describeMixed """{"value": 42}""") "large 42" "branch 5"
            Expect.equal (describeMixed """{"value": ["p", "q"]}""") "strings [\"p\"; \"q\"]" "branch 6"
            Expect.equal (describeMixed """{"value": [1, 2]}""") "ints [1; 2]" "branch 7"
            Expect.equal (describeMixed """{"value": {"x": 8}}""") "object x=8" "branch 8"
        }

    let eightMixedBranchesRejectAmbiguousEmptyArray =
        test "8-way mixed oneOf: an empty array matches both array branches and fails Parse validation" {
            Expect.isError (EightMixedBranches.Parse("""{"value": []}""")) "[] satisfies both array branches"
        }

    let eightMixedBranchesRejectValueInNoBranch =
        test "8-way mixed oneOf: an object missing its required property fails Parse validation" {
            Expect.isError (EightMixedBranches.Parse("""{"value": {"y": 1}}""")) "no branch accepts {\"y\": 1}"
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
              enumOneOfRejectsValueInNeitherEnum
              describeMatchesEachOwnerBranch
              singleBranchParsesToPlainType
              singleBranchCreateRoundTrips
              singleBranchRejectsOtherKind
              eightEnumBranchesPickFirst
              eightEnumBranchesPickMiddle
              eightEnumBranchesPickSeventh
              eightEnumBranchesPickLast
              eightEnumBranchesRejectValueInNoBranch
              eightMixedBranchesPickEachBranch
              eightMixedBranchesRejectAmbiguousEmptyArray
              eightMixedBranchesRejectValueInNoBranch ]
