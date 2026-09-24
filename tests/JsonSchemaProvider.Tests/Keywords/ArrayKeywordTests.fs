namespace JsonSchemaProvider.Tests

module ArrayKeywordTests =
    open Expecto
    open JsonSchemaProvider

    // ---- compile-time tuple encoding of minItems/maxItems ----
    // SchemaConversion.fs's buildArrayConversion compiles minItems/maxItems into a precise tuple
    // shape (head elements as their own fields, only the open-ended remainder as a list/option) -
    // this section is about that compile-time *shape*, not runtime rejection (see the runtime
    // section below for minItems actually being enforced by Create).

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

    // ---- runtime enforcement: uniqueItems, maxItems, minItems (plain-list case), per-item constraints ----

    [<Literal>]
    let uniqueItemsSchema =
        """
        { "type": "object", "properties": { "values": { "type": "array", "items": { "type": "integer" }, "uniqueItems": true } }, "required": ["values"] }"""

    type UniqueItemsArray = JsonSchemaProvider<schema=uniqueItemsSchema>

    [<Literal>]
    let maxItemsSchema =
        """
        { "type": "object", "properties": { "values": { "type": "array", "items": { "type": "integer" }, "maxItems": 2 } }, "required": ["values"] }"""

    type MaxItemsArray = JsonSchemaProvider<schema=maxItemsSchema>

    // Plain list (no compileMinItems) - the compile-time-shape section above only covers the
    // tuple shape for minItems, never whether Create actually REJECTS too few items at runtime.
    [<Literal>]
    let minItemsRuntimeSchema =
        """
        { "type": "object", "properties": { "tags": { "type": "array", "items": { "type": "string" }, "minItems": 2 } }, "required": ["tags"] }"""

    type MinItemsRuntime = JsonSchemaProvider<schema=minItemsRuntimeSchema, ignoreSpecificKeywords=true>

    [<Literal>]
    let arrayItemConstraintSchema =
        """
        { "type": "object", "properties": { "values": { "type": "array", "items": { "type": "integer", "minimum": 0 } } }, "required": ["values"] }"""

    type ArrayItemConstraint = JsonSchemaProvider<schema=arrayItemConstraintSchema>

    let duplicateItemsAreRejected =
        test "uniqueItems rejects an array with a duplicate" {
            Expect.isError
                (UniqueItemsArray.Create(values = [ 1; 2; 2 ]))
                "[1;2;2] has a duplicate"
        }

    let allUniqueItemsAreAccepted =
        test "uniqueItems accepts an array with no duplicates" {
            let result = Expect.wantOk (UniqueItemsArray.Create(values = [ 1; 2; 3 ])) "Create should succeed"
            Expect.equal result.values [ 1; 2; 3 ] "[1;2;3] has no duplicates"
        }

    // Unlike minItems (see MinItemsRuntime above, which has no compileMinItems flag and so stays
    // a plain list, only checked by Create at runtime), maxItems always compiles to a precise
    // Option<int * Option<int>> type here - there's no plain-list escape hatch for it. A 3rd
    // element has nowhere to go in that type, so "too many items" is a compile error, not
    // something Expect.isError can observe at runtime - hence no rejection test for it.
    let atMaxItemsIsAccepted =
        test "maxItems accepts an array at the limit" {
            let result = MaxItemsArray.Create(values = Some(1, Some 2))
            Expect.equal result.values (Some(1, Some 2)) "2 elements is exactly maxItems=2"
        }

    let withinMaxItemsIsAccepted =
        test "maxItems accepts an array within the limit" {
            let result = MaxItemsArray.Create(values = Some(1, None))
            Expect.equal result.values (Some(1, None)) "1 element is within maxItems=2"
        }

    let tooFewItemsAreRejectedAtRuntime =
        test "minItems is enforced at runtime by Create, not just as a compile-time tuple shape" {
            Expect.isError
                (MinItemsRuntime.Create(tags = [ "a" ]))
                "1 element is below minItems=2"
        }

    let enoughItemsAreAcceptedAtRuntime =
        test "minItems is satisfied at runtime by Create when there are enough elements" {
            let result = Expect.wantOk (MinItemsRuntime.Create(tags = [ "a"; "b" ])) "Create should succeed"
            Expect.equal result.tags [ "a"; "b" ] "2 elements satisfies minItems=2"
        }

    let arrayWithAnOutOfRangeItemIsRejected =
        test "a constraint on the array's own item schema is enforced against every element" {
            Expect.isError
                (ArrayItemConstraint.Create(values = [ 1; -1; 3 ]))
                "-1 violates the item schema's own minimum, even though the array itself has no length constraint"
        }

    let arrayWithAllInRangeItemsIsAccepted =
        test "an array whose every element satisfies the item schema is accepted" {
            let result = Expect.wantOk (ArrayItemConstraint.Create(values = [ 1; 2; 3 ])) "Create should succeed"
            Expect.equal result.values [ 1; 2; 3 ] "every element satisfies the item schema's minimum"
        }

    // ---- general array-property access: numeric/nested/object items, indexing ----

    [<Literal>]
    let numberArray =
        """
        {
          "type": "object",
          "properties": {
            "values": {
              "type": "array",
              "items": {"type": "number"}
            }
          },
          "required": ["values"]
        }"""

    [<Literal>]
    let integerArray =
        """
        {
          "type": "object",
          "properties": {
            "values": {
              "type": "array",
              "items": {"type": "integer"}
            }
          },
          "required": ["values"]
        }"""

    [<Literal>]
    let nestedArray =
        """
        {
          "type": "object",
          "properties": {
            "values": {
              "type": "array",
              "items": {
                "type": "array",
                "items": {"type": "string"}
              }
            }
          },
          "required": ["values"]
        }"""

    [<Literal>]
    let nestedArrayWithObjectItems =
        """
        {
          "type": "object",
          "properties": {
            "values": {
              "type": "array",
              "items": {
                "type": "array",
                "items": {
                  "type": "object",
                  "properties": {
                      "propA": {"type": "integer"},
                      "propB": {"type": "string"}
                  }
                }
              }
            }
          }
        }"""

    type NumberArray = JsonSchemaProvider<schema=numberArray>
    type IntegerArray = JsonSchemaProvider<schema=integerArray>
    type NestedArray = JsonSchemaProvider<schema=nestedArray>
    type NestedArrayWithObjectItems = JsonSchemaProvider<schema=nestedArrayWithObjectItems>

    let selectFromNumberArrayShouldYieldInputValue =
        let numArray = NumberArray.Create([ 11.0; 12.0; 11.6; 12.1 ])

        test "select from number array should yield input value" {
            Expect.equal numArray.values[1] 12.0 "numArray.values[1] = 12.0"
        }

    let selectFromIntegerArrayShouldYieldInputValue =
        let numArray = IntegerArray.Create([ 11; 12; 10; 13 ])

        test "select from integer array should yield input value" {
            Expect.equal numArray.values[1] 12 "numArray.values[1] = 12"
        }

    let selectFromNestedArrayShouldYieldInputValue =
        let array = NestedArray.Create([ [ "a"; "b" ] ])

        test "select from nested array should yield input value" {
            Expect.equal (array.values[0][1]) "b" "nestedArray.values[0][1] = \"b\""
        }

    let selectFromNestedArrayWithObjectItemsShouldYieldInputValue =
        let item = NestedArrayWithObjectItems.valuesItem.Create(propA = 5)
        let array = NestedArrayWithObjectItems.Create([ [ item ] ])

        test "select from nested array with object items should yield input value" {
            Expect.equal
                (array.values.Value[0][0]).propA
                (Some(5))
                "nestedArrayWithObjectItems.values.Value[0][0]).propA = 5"
        }

    [<Tests>]
    let tests =
        testList
            "JsonSchemaProvider.Tests.ArrayKeywordTests"
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
              exactWithFlagParseProducesTuple
              duplicateItemsAreRejected
              allUniqueItemsAreAccepted
              atMaxItemsIsAccepted
              withinMaxItemsIsAccepted
              tooFewItemsAreRejectedAtRuntime
              enoughItemsAreAcceptedAtRuntime
              arrayWithAnOutOfRangeItemIsRejected
              arrayWithAllInRangeItemsIsAccepted
              selectFromNumberArrayShouldYieldInputValue
              selectFromIntegerArrayShouldYieldInputValue
              selectFromNestedArrayShouldYieldInputValue
              selectFromNestedArrayWithObjectItemsShouldYieldInputValue ]
