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

    // Same schema as stringArrayMin2Schema but without the flag — plain list, constraint ignored at type level.
    type PlainStringArray = JsonSchemaProvider<schema = stringArrayMin2Schema>

    // Compile-time constrained: tags : string * string * string list
    type StringArrayMin2 = JsonSchemaProvider<schema = stringArrayMin2Schema, compileMinItems = true>

    // Compile-time constrained: values : int * int list
    type IntArrayMin1 = JsonSchemaProvider<schema = intArrayMin1Schema, compileMinItems = true>

    let withoutFlagMinItemsSchemaYieldsPlainList =
        test "minItems schema without compileMinItems flag yields plain list" {
            let v = PlainStringArray.Parse("""{"tags": ["a", "b", "c"]}""")
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
            let v = StringArrayMin2.Parse("""{"tags": ["x", "y", "z"]}""")
            let (h1, h2, rest) = v.tags
            Expect.equal h1 "x" "first element"
            Expect.equal h2 "y" "second element"
            Expect.equal rest [ "z" ] "rest"
        }

    let min2ExactlyMinimumGivesEmptyRest =
        test "minItems=2 with exactly 2 elements gives empty rest" {
            let v = StringArrayMin2.Parse("""{"tags": ["a", "b"]}""")
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
            let v = IntArrayMin1.Parse("""{"values": [10, 20, 30]}""")
            let (head, rest) = v.values
            Expect.equal head 10 "head"
            Expect.equal rest [ 20; 30 ] "rest"
        }

    let min1ExactlyMinimumGivesEmptyRest =
        test "minItems=1 with exactly 1 element gives empty rest" {
            let v = IntArrayMin1.Parse("""{"values": [99]}""")
            let (head, rest) = v.values
            Expect.equal head 99 "head"
            Expect.equal rest [] "rest is empty"
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
              min1ExactlyMinimumGivesEmptyRest ]
