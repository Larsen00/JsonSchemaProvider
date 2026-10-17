namespace JsonSchemaProvider.Tests

module OneOfTests =
    open Xunit
    open FsCheck
    open FsCheck.Xunit
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

    [<Fact>]
    let ``parse picks string branch of a string|int oneOf`` () =
        let v = expectOk (StringOrInt.Parse("""{"value": "hello"}"""))
        expectEqual v.value (Choice1Of2 "hello")

    [<Fact>]
    let ``parse picks int branch of a string|int oneOf`` () =
        let v = expectOk (StringOrInt.Parse("""{"value": 42}"""))
        expectEqual v.value (Choice2Of2 42)

    [<Fact>]
    let ``create with string branch round-trips through Parse`` () =
        let created = StringOrInt.Create(value = Choice1Of2 "abc")
        let reparsed = expectOk (StringOrInt.Parse(created.ToString()))
        expectEqual reparsed.value (Choice1Of2 "abc")

    [<Fact>]
    let ``create with int branch round-trips through Parse`` () =
        let created = StringOrInt.Create(value = Choice2Of2 99)
        let reparsed = expectOk (StringOrInt.Parse(created.ToString()))
        expectEqual reparsed.value (Choice2Of2 99)

    [<Property>]
    let ``create with any string round-trips through Parse into the string branch`` (NonNull s) =
        let created = StringOrInt.Create(value = Choice1Of2 s)
        let reparsed = expectOk (StringOrInt.Parse(created.ToString()))
        expectEqual reparsed.value (Choice1Of2 s)

    [<Property>]
    let ``create with any int round-trips through Parse into the int branch`` (n: int) =
        let created = StringOrInt.Create(value = Choice2Of2 n)
        let reparsed = expectOk (StringOrInt.Parse(created.ToString()))
        expectEqual reparsed.value (Choice2Of2 n)

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

    [<Fact>]
    let ``parse picks string branch of a string|int|bool oneOf`` () =
        let v = expectOk (StringOrIntOrBool.Parse("""{"value": "x"}"""))
        expectEqual v.value (Choice1Of2 "x")

    [<Fact>]
    let ``parse picks int branch of a string|int|bool oneOf`` () =
        let v = expectOk (StringOrIntOrBool.Parse("""{"value": 7}"""))
        expectEqual v.value (Choice2Of2(Choice1Of2 7))

    [<Fact>]
    let ``parse picks bool branch of a string|int|bool oneOf`` () =
        let v = expectOk (StringOrIntOrBool.Parse("""{"value": true}"""))
        expectEqual v.value (Choice2Of2(Choice2Of2 true))

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

    [<Fact>]
    let ``parse picks string branch of a string|array oneOf`` () =
        let v = expectOk (StringOrIntArray.Parse("""{"value": "hi"}"""))
        expectEqual v.value (Choice1Of2 "hi")

    [<Fact>]
    let ``parse picks array branch of a string|array oneOf`` () =
        let v = expectOk (StringOrIntArray.Parse("""{"value": [1, 2, 3]}"""))
        expectEqual v.value (Choice2Of2 [ 1; 2; 3 ])

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

    [<Fact>]
    let ``nested oneOf: outer string alternative matches`` () =
        let v = expectOk (NestedOneOf.Parse("""{"value": "s"}"""))
        expectEqual v.value (Choice1Of2 "s")

    [<Fact>]
    let ``nested oneOf: inner int alternative matches`` () =
        let v = expectOk (NestedOneOf.Parse("""{"value": 3}"""))
        expectEqual v.value (Choice2Of2(Choice1Of2 3))

    [<Fact>]
    let ``nested oneOf: inner bool alternative matches`` () =
        let v = expectOk (NestedOneOf.Parse("""{"value": false}"""))
        expectEqual v.value (Choice2Of2(Choice2Of2 false))

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

    [<Fact>]
    let ``number|number oneOf: value <= 0 picks the maximum-0 branch`` () =
        let v = expectOk (NumberRangeOneOf.Parse("""{"value": -5.5}"""))
        expectEqual v.value (Choice1Of2 -5.5)

    [<Fact>]
    let ``number|number oneOf: value > 0 picks the minimum-0.1 branch`` () =
        let v = expectOk (NumberRangeOneOf.Parse("""{"value": 3.2}"""))
        expectEqual v.value (Choice2Of2 3.2)

    [<Fact>]
    let ``number|number oneOf: the maximum-0 boundary itself picks the inclusive branch`` () =
        let v = expectOk (NumberRangeOneOf.Parse("""{"value": 0}"""))
        expectEqual v.value (Choice1Of2 0.0)

    [<Fact>]
    let ``number|number oneOf: a value matching neither branch fails Parse validation`` () =
        expectError (NumberRangeOneOf.Parse("""{"value": 0.05}""")) // 0.05 satisfies neither branch

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

    [<Fact>]
    let ``string|string oneOf: digits-only value picks the numeric-pattern branch`` () =
        let v = expectOk (StringPatternOneOf.Parse("""{"value": "123"}"""))
        expectEqual v.value (Choice1Of2 "123")

    [<Fact>]
    let ``string|string oneOf: lowercase-only value picks the alphabetic-pattern branch`` () =
        let v = expectOk (StringPatternOneOf.Parse("""{"value": "abc"}"""))
        expectEqual v.value (Choice2Of2 "abc")

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

    [<Fact>]
    let ``array|array oneOf: string items pick the string-item branch`` () =
        let v = expectOk (ArrayItemTypeOneOf.Parse("""{"value": ["a", "b"]}"""))
        expectEqual v.value (Choice1Of2 [ "a"; "b" ])

    [<Fact>]
    let ``array|array oneOf: integer items pick the integer-item branch`` () =
        let v = expectOk (ArrayItemTypeOneOf.Parse("""{"value": [1, 2, 3]}"""))
        expectEqual v.value (Choice2Of2 [ 1; 2; 3 ])

    [<Fact>]
    let ``array|array oneOf: an empty array matches both branches and fails Parse validation`` () =
        expectError (ArrayItemTypeOneOf.Parse("""{"value": []}""")) // [] satisfies both item schemas

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

    [<Fact>]
    let ``array|array oneOf: a 2-element array picks the maxItems:2 branch`` () =
        let v = expectOk (ArrayLengthOneOf.Parse("""{"value": [1, 2]}"""))
        expectEqual v.value (Choice1Of2(Some(1, Some 2)))

    [<Fact>]
    let ``array|array oneOf: a 3-element array picks the minItems:3 branch`` () =
        let v = expectOk (ArrayLengthOneOf.Parse("""{"value": [1, 2, 3]}"""))
        expectEqual v.value (Choice2Of2(1, (2, (3, []))))

    // -- ToList/ToTuple for arrays inside oneOf: branch i gets a "<name>Case<i>Array" helper type --

    [<Fact>]
    let ``array|array oneOf: valueCase1Array/valueCase2Array.ToList flatten their own branch`` () =
        let toList (v: ArrayLengthOneOf) =
            match v.value with
            | Choice1Of2 short -> ArrayLengthOneOf.valueCase1Array.ToList short
            | Choice2Of2 long -> ArrayLengthOneOf.valueCase2Array.ToList long
        let short = expectOk (ArrayLengthOneOf.Parse("""{"value": [1, 2]}"""))
        let long = expectOk (ArrayLengthOneOf.Parse("""{"value": [1, 2, 3, 4]}"""))
        expectEqual (toList short, toList long) ([ 1; 2 ], [ 1; 2; 3; 4 ])

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

    [<Fact>]
    let ``root string|array oneOf: Case2Array.ToTuple flattens the array branch`` () =
        expectEqual (RootStringOrExact2Array.Case2Array.ToTuple((1, 2))) (1, 2)

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

    [<Fact>]
    let ``array of string|array oneOf items: valuesArray.ItemCase2Array.ToList flattens each array item`` () =
        let v = expectOk (ArrayOfStringOrMin1Array.Parse("""{"values": ["a", [1, 2]]}"""))
        let flattened =
            v.values
            |> List.choose (function
                | Choice2Of2 arr -> Some(ArrayOfStringOrMin1Array.valuesArray.ItemCase2Array.ToList arr)
                | Choice1Of2 _ -> None)
        expectEqual flattened [ [ 1; 2 ] ]

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

    [<Fact>]
    let ``string|string oneOf: a color enum value picks the color branch`` () =
        let v = expectOk (EnumOneOf.Parse("""{"value": "red"}"""))
        expectEqual v.value (Choice1Of2 "red")

    [<Fact>]
    let ``string|string oneOf: a shape enum value picks the shape branch`` () =
        let v = expectOk (EnumOneOf.Parse("""{"value": "circle"}"""))
        expectEqual v.value (Choice2Of2 "circle")

    [<Fact>]
    let ``string|string oneOf: a value in neither enum fails Parse validation`` () =
        expectError (EnumOneOf.Parse("""{"value": "banana"}""")) // "banana" is in neither enum

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
        let v = expectOk (Owner.Parse(json))
        describe v.owner

    [<Fact>]
    let ``string|int|object oneOf: describe matches the branch Parse picked`` () =
        expectEqual (describeOwner """{"owner": "Ada"}""") "name Ada"
        expectEqual (describeOwner """{"owner": 7}""") "id 7"
        let contact = describeOwner """{"owner": {"name": "Ada", "email": "ada@example.com"}}"""
        Assert.StartsWith("contact {", contact)

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

    [<Fact>]
    let ``single-branch oneOf: Parse gives the branch type, not a Choice`` () =
        let v = expectOk (SingleBranch.Parse("""{"value": 5}"""))
        let n: int = v.value
        expectEqual n 5

    [<Fact>]
    let ``single-branch oneOf: Create round-trips through Parse`` () =
        let created = SingleBranch.Create(value = 9)
        let reparsed = expectOk (SingleBranch.Parse(created.ToString()))
        expectEqual reparsed.value 9

    [<Fact>]
    let ``single-branch oneOf: a value outside the only branch fails Parse validation`` () =
        expectError (SingleBranch.Parse("""{"value": "five"}""")) // a string does not match the integer branch

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
        (expectOk (EightEnumBranches.Parse(json))).value

    [<Fact>]
    let ``8-way enum oneOf: "a" picks the first branch`` () =
        expectEqual (parseEightEnum """{"value": "a"}""") (Choice1Of2 "a")

    [<Fact>]
    let ``8-way enum oneOf: "d" picks the fourth branch`` () =
        expectEqual (parseEightEnum """{"value": "d"}""") (Choice2Of2(Choice2Of2(Choice2Of2(Choice1Of2 "d"))))

    [<Fact>]
    let ``8-way enum oneOf: "g" picks the seventh branch, the last Choice1Of2`` () =
        expectEqual (parseEightEnum """{"value": "g"}""") (Choice2Of2(Choice2Of2(Choice2Of2(Choice2Of2(Choice2Of2(Choice2Of2(Choice1Of2 "g")))))))

    [<Fact>]
    let ``8-way enum oneOf: "h" picks the eighth branch, the innermost Choice2Of2`` () =
        expectEqual (parseEightEnum """{"value": "h"}""") (Choice2Of2(Choice2Of2(Choice2Of2(Choice2Of2(Choice2Of2(Choice2Of2(Choice2Of2 "h")))))))

    [<Fact>]
    let ``8-way enum oneOf: a value in no enum fails Parse validation`` () =
        expectError (EightEnumBranches.Parse("""{"value": "z"}""")) // "z" is in no branch's enum

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
        let v = expectOk (EightMixedBranches.Parse(json))
        match v.value with
        | Choice1Of2 s -> $"string {s}"
        | Choice2Of2 (Choice1Of2 b) -> $"boolean {b}"
        | Choice2Of2 (Choice2Of2 (Choice1Of2 n)) -> $"non-positive {n}"
        | Choice2Of2 (Choice2Of2 (Choice2Of2 (Choice1Of2 n))) -> $"digit {n}"
        | Choice2Of2 (Choice2Of2 (Choice2Of2 (Choice2Of2 (Choice1Of2 n)))) -> $"large {n}"
        | Choice2Of2 (Choice2Of2 (Choice2Of2 (Choice2Of2 (Choice2Of2 (Choice1Of2 xs))))) -> $"strings %A{xs}"
        | Choice2Of2 (Choice2Of2 (Choice2Of2 (Choice2Of2 (Choice2Of2 (Choice2Of2 (Choice1Of2 xs)))))) -> $"ints %A{xs}"
        | Choice2Of2 (Choice2Of2 (Choice2Of2 (Choice2Of2 (Choice2Of2 (Choice2Of2 (Choice2Of2 o)))))) -> $"object x={o.x}"

    [<Fact>]
    let ``8-way mixed oneOf: Parse picks the matching branch for each kind and range`` () =
        expectEqual (describeMixed """{"value": "s"}""") "string s"
        expectEqual (describeMixed """{"value": true}""") "boolean True"
        expectEqual (describeMixed """{"value": -3}""") "non-positive -3"
        expectEqual (describeMixed """{"value": 4}""") "digit 4"
        expectEqual (describeMixed """{"value": 42}""") "large 42"
        expectEqual (describeMixed """{"value": ["p", "q"]}""") "strings [\"p\"; \"q\"]"
        expectEqual (describeMixed """{"value": [1, 2]}""") "ints [1; 2]"
        expectEqual (describeMixed """{"value": {"x": 8}}""") "object x=8"

    [<Property>]
    let ``8-way mixed oneOf: any integer lands in the branch for its range`` (n: int) =
        let expected =
            if n <= 0 then $"non-positive {n}"
            elif n <= 9 then $"digit {n}"
            else $"large {n}"
        expectEqual (describeMixed $"""{{"value": {n}}}""") expected

    [<Fact>]
    let ``8-way mixed oneOf: an empty array matches both array branches and fails Parse validation`` () =
        expectError (EightMixedBranches.Parse("""{"value": []}""")) // [] satisfies both array branches

    [<Fact>]
    let ``8-way mixed oneOf: an object missing its required property fails Parse validation`` () =
        expectError (EightMixedBranches.Parse("""{"value": {"y": 1}}""")) // no branch accepts {"y": 1}
