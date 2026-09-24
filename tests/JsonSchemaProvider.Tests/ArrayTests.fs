namespace JsonSchemaProvider.Tests

module ArrayTests =
    open Expecto
    open JsonSchemaProvider

    [<Literal>]
    let stringArrayMin2Schema =
        """
        {
          "type": "object",
          "properties": {
            "tags": {
              "type": "array",
              "items": {"type": "string"},
              "minItems": 2
            }
          },
          "required": ["tags"]
        }"""

    [<Literal>]
    let intArrayMin1Schema =
        """
        {
          "type": "object",
          "properties": {
            "values": {
              "type": "array",
              "items": {"type": "integer"},
              "minItems": 1
            }
          },
          "required": ["values"]
        }"""

    // Both minItems and maxItems declared together - minItems' mandatory prefix (1 element) plus
    // maxItems' remaining bound (1 more, optional) recurse into each other in buildArrayConversion.
    [<Literal>]
    let intArrayMin1Max2Schema =
        """
        {
          "type": "object",
          "properties": {
            "values": {
              "type": "array",
              "items": {"type": "integer"},
              "minItems": 1,
              "maxItems": 2
            }
          },
          "required": ["values"]
        }"""

    // maxItems alone, no minItems - maxItems is unconditionally precise, no compileMinItems flag needed.
    [<Literal>]
    let intArrayMax2Schema =
        """
        {
          "type": "object",
          "properties": {
            "values": {
              "type": "array",
              "items": {"type": "integer"},
              "maxItems": 2
            }
          },
          "required": ["values"]
        }"""

    // maxItems = 1 specifically - buildArrayConversion's own dedicated option<inner> case, distinct
    // from the option<(inner * tail)> shape maxItems > 1 produces.
    [<Literal>]
    let intArrayMax1Schema =
        """
        {
          "type": "object",
          "properties": {
            "values": {
              "type": "array",
              "items": {"type": "integer"},
              "maxItems": 1
            }
          },
          "required": ["values"]
        }"""

    // minItems = maxItems: buildArrayConversion's exact-tuple case has no `when
    // compileFlags.CompileMinItems` guard - it's checked before that branch and fires purely off
    // n = n2, so this schema should compile to the same tuple with or without the flag.
    [<Literal>]
    let intArrayExact2Schema =
        """
        {
          "type": "object",
          "properties": {
            "values": {
              "type": "array",
              "items": {"type": "integer"},
              "minItems": 2,
              "maxItems": 2
            }
          },
          "required": ["values"]
        }"""

    // Same schema as stringArrayMin2Schema but without the flag — plain list, constraint ignored at type level.
    type PlainStringArray = JsonSchemaProvider<schema = stringArrayMin2Schema, ignoreSpecificKeywords = true>

    // Compile-time constrained: tags : string * string * string list
    type StringArrayMin2 = JsonSchemaProvider<schema = stringArrayMin2Schema>

    // Compile-time constrained: values : int * int list
    type IntArrayMin1 = JsonSchemaProvider<schema = intArrayMin1Schema>

    // Compile-time constrained: values : int * int option
    type IntArrayMin1Max2 = JsonSchemaProvider<schema = intArrayMin1Max2Schema>

    // Compile-time constrained: values : option<int * int option>
    type IntArrayMax2 = JsonSchemaProvider<schema = intArrayMax2Schema>

    // Compile-time constrained: values : int option
    type IntArrayMax1 = JsonSchemaProvider<schema = intArrayMax1Schema>

    // Compile-time constrained: values : int * int - no flag passed at all.
    type IntArrayExact2 = JsonSchemaProvider<schema = intArrayExact2Schema>

    // Same schema, with the flag - should produce the identical int * int shape.
    type IntArrayExact2WithFlag = JsonSchemaProvider<schema = intArrayExact2Schema>

    let withoutFlagMinItemsSchemaYieldsPlainList =
        test "minItems schema without compileMinItems flag yields plain list" {
            let v = Expect.wantOk (PlainStringArray.Parse("""{"tags": ["a", "b", "c"]}""")) "Parse should succeed"
            Expect.equal v.tags [ "a"; "b"; "c" ] "tags = [a; b; c]"
        }

    let min2CreateProducesCorrectTuple =
        test "minItems=2 Create produces correct tuple" {
            let v = StringArrayMin2.Create(tags = ("a", "b", [ "c"; "d" ]))
            let (h1, h2, rest) = v.tags
            Expect.equal h1 "a" "first element"
            Expect.equal h2 "b" "second element"
            Expect.equal rest [ "c"; "d" ] "rest"
        }

    let min2ParseProducesCorrectTuple =
        test "minItems=2 Parse produces correct tuple" {
            let v = Expect.wantOk (StringArrayMin2.Parse("""{"tags": ["x", "y", "z"]}""")) "Parse should succeed"
            let (h1, h2, rest) = v.tags
            Expect.equal h1 "x" "first element"
            Expect.equal h2 "y" "second element"
            Expect.equal rest [ "z" ] "rest"
        }

    let min2ExactlyMinimumGivesEmptyRest =
        test "minItems=2 with exactly 2 elements gives empty rest" {
            let v = Expect.wantOk (StringArrayMin2.Parse("""{"tags": ["a", "b"]}""")) "Parse should succeed"
            let (h1, h2, rest) = v.tags
            Expect.equal h1 "a" "first element"
            Expect.equal h2 "b" "second element"
            Expect.equal rest [] "rest is empty"
        }

    let min1CreateProducesCorrectPair =
        test "minItems=1 Create produces correct pair" {
            let v = IntArrayMin1.Create(values = (42, [ 1; 2; 3 ]))
            let (head, rest) = v.values
            Expect.equal head 42 "head"
            Expect.equal rest [ 1; 2; 3 ] "rest"
        }

    let min1ParseProducesCorrectPair =
        test "minItems=1 Parse produces correct pair" {
            let v = Expect.wantOk (IntArrayMin1.Parse("""{"values": [10, 20, 30]}""")) "Parse should succeed"
            let (head, rest) = v.values
            Expect.equal head 10 "head"
            Expect.equal rest [ 20; 30 ] "rest"
        }

    let min1ExactlyMinimumGivesEmptyRest =
        test "minItems=1 with exactly 1 element gives empty rest" {
            let v = Expect.wantOk (IntArrayMin1.Parse("""{"values": [99]}""")) "Parse should succeed"
            let (head, rest) = v.values
            Expect.equal head 99 "head"
            Expect.equal rest [] "rest is empty"
        }

    // ---- both minItems and maxItems declared together: values : int * int option ----

    let minMaxCreateAtMinimumGivesNone =
        test "minItems=1, maxItems=2: Create with exactly minItems elements gives None for the rest" {
            let v = IntArrayMin1Max2.Create(values = (42, None))
            let (head, rest) = v.values
            Expect.equal head 42 "head"
            Expect.equal rest None "no second element"
        }

    let minMaxCreateAtMaximumGivesSome =
        test "minItems=1, maxItems=2: Create with maxItems elements gives Some for the rest" {
            let v = IntArrayMin1Max2.Create(values = (42, Some 7))
            let (head, rest) = v.values
            Expect.equal head 42 "head"
            Expect.equal rest (Some 7) "second element"
        }

    let minMaxParseAtMaximumGivesSome =
        test "minItems=1, maxItems=2: Parse with maxItems elements gives Some for the rest" {
            let v = Expect.wantOk (IntArrayMin1Max2.Parse("""{"values": [1, 2]}""")) "Parse should succeed"
            Expect.equal v.values (1, Some 2) "parsed tuple matches the two elements"
        }

    // ---- maxItems alone, no minItems: values : option<int * int option> ----

    let maxOnlyEmptyArrayGivesNone =
        test "maxItems=2, no minItems: Create with no elements gives None" {
            let v = IntArrayMax2.Create(values = None)
            Expect.equal v.values None "empty array with no lower bound"
        }

    let maxOnlyOneElementGivesPartialSome =
        test "maxItems=2, no minItems: Create with one element gives Some(head, None)" {
            let v = IntArrayMax2.Create(values = Some(1, None))
            Expect.equal v.values (Some(1, None)) "one element, nothing after it"
        }

    let maxOnlyAtLimitGivesFullSome =
        test "maxItems=2, no minItems: Create at the limit gives Some(head, Some tail)" {
            let v = IntArrayMax2.Create(values = Some(1, Some 2))
            Expect.equal v.values (Some(1, Some 2)) "both elements present"
        }

    let maxOnlyParseAtLimit =
        test "maxItems=2, no minItems: Parse at the limit round-trips both elements" {
            let v = Expect.wantOk (IntArrayMax2.Parse("""{"values": [5, 6]}""")) "Parse should succeed"
            Expect.equal v.values (Some(5, Some 6)) "both elements present"
        }

    // ---- maxItems = 1: values : int option ----

    let maxOneEmptyArrayGivesNone =
        test "maxItems=1: Create with no elements gives None" {
            let v = IntArrayMax1.Create(values = None)
            Expect.equal v.values None "empty array"
        }

    let maxOneSingleElementGivesSome =
        test "maxItems=1: Create with one element gives Some" {
            let v = IntArrayMax1.Create(values = Some 42)
            Expect.equal v.values (Some 42) "the single element"
        }

    let maxOneParseSingleElement =
        test "maxItems=1: Parse with one element round-trips it" {
            let v = Expect.wantOk (IntArrayMax1.Parse("""{"values": [7]}""")) "Parse should succeed"
            Expect.equal v.values (Some 7) "the single element"
        }

    // ---- minItems = maxItems: values : int * int, with and without compileMinItems ----

    let exactWithoutFlagCreateProducesTuple =
        test "minItems=maxItems=2, no flag: Create produces the exact tuple" {
            let v = IntArrayExact2.Create(values = (1, 2))
            Expect.equal v.values (1, 2) "exact 2-tuple"
        }

    let exactWithoutFlagParseProducesTuple =
        test "minItems=maxItems=2, no flag: Parse produces the exact tuple" {
            let v = Expect.wantOk (IntArrayExact2.Parse("""{"values": [3, 4]}""")) "Parse should succeed"
            Expect.equal v.values (3, 4) "exact 2-tuple"
        }

    let exactWithFlagCreateProducesTuple =
        test "minItems=maxItems=2, with compileMinItems: Create produces the same exact tuple" {
            let v = IntArrayExact2WithFlag.Create(values = (5, 6))
            Expect.equal v.values (5, 6) "the flag doesn't change the exact-tuple shape"
        }

    let exactWithFlagParseProducesTuple =
        test "minItems=maxItems=2, with compileMinItems: Parse produces the same exact tuple" {
            let v = Expect.wantOk (IntArrayExact2WithFlag.Parse("""{"values": [7, 8]}""")) "Parse should succeed"
            Expect.equal v.values (7, 8) "the flag doesn't change the exact-tuple shape"
        }

    [<Tests>]
    let tests =
        testList
            "JsonSchemaProvider.Tests.ArrayTests"
            [ withoutFlagMinItemsSchemaYieldsPlainList
              min2CreateProducesCorrectTuple
              min2ParseProducesCorrectTuple
              min2ExactlyMinimumGivesEmptyRest
              min1CreateProducesCorrectPair
              min1ParseProducesCorrectPair
              min1ExactlyMinimumGivesEmptyRest
              minMaxCreateAtMinimumGivesNone
              minMaxCreateAtMaximumGivesSome
              minMaxParseAtMaximumGivesSome
              maxOnlyEmptyArrayGivesNone
              maxOnlyOneElementGivesPartialSome
              maxOnlyAtLimitGivesFullSome
              maxOnlyParseAtLimit
              maxOneEmptyArrayGivesNone
              maxOneSingleElementGivesSome
              maxOneParseSingleElement
              exactWithoutFlagCreateProducesTuple
              exactWithoutFlagParseProducesTuple
              exactWithFlagCreateProducesTuple
              exactWithFlagParseProducesTuple ]
