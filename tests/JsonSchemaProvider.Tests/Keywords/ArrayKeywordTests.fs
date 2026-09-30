namespace JsonSchemaProvider.Tests

// Array keywords at the schema root. Object-nested versions of the minItems/maxItems/uniqueItems
// cases live in ObjectKeywordTests.fs. The last section (general array-property access) isn't
// about a keyword - it stays object-wrapped since a property is just where the array lives.
module ArrayKeywordTests =
    open Expecto
    open JsonSchemaProvider

    // -- minItems: compile-time tuple shape --

    [<Literal>]
    let stringArrayMin2Schema = """{ "type": "array", "items": {"type": "string"}, "minItems": 2 }"""
    type PlainStringArray = JsonSchemaProvider<schema = stringArrayMin2Schema, ignoreSpecificKeywords = true>
    type StringArrayMin2 = JsonSchemaProvider<schema = stringArrayMin2Schema>

    let withoutFlagMinItemsSchemaYieldsPlainList =
        test "minItems schema without compileMinItems flag yields plain list" {
            let v = Expect.wantOk (PlainStringArray.Parse("""["a", "b", "c"]""")) "Parse should succeed"
            Expect.equal v [ "a"; "b"; "c" ] "value = [a; b; c]"
        }

    let min2CreateProducesCorrectTuple =
        test "minItems=2 Create produces correct tuple" {
            let (h1, (h2, rest)) = StringArrayMin2.Create(("a", ("b", [ "c"; "d" ])))
            Expect.equal h1 "a" "first element"
            Expect.equal h2 "b" "second element"
            Expect.equal rest [ "c"; "d" ] "rest"
        }

    let min2ParseProducesCorrectTuple =
        test "minItems=2 Parse produces correct tuple" {
            let v = Expect.wantOk (StringArrayMin2.Parse("""["x", "y", "z"]""")) "Parse should succeed"
            let (h1, (h2, rest)) = v
            Expect.equal h1 "x" "first element"
            Expect.equal h2 "y" "second element"
            Expect.equal rest [ "z" ] "rest"
        }

    let min2ExactlyMinimumGivesEmptyRest =
        test "minItems=2 with exactly 2 elements gives empty rest" {
            let v = Expect.wantOk (StringArrayMin2.Parse("""["a", "b"]""")) "Parse should succeed"
            let (_, (_, rest)) = v
            Expect.equal rest [] "rest is empty"
        }

    [<Literal>]
    let intArrayMin1Schema = """{ "type": "array", "items": {"type": "integer"}, "minItems": 1 }"""
    type IntArrayMin1 = JsonSchemaProvider<schema = intArrayMin1Schema>

    let min1CreateProducesCorrectPair =
        test "minItems=1 Create produces correct pair" {
            let (head, rest) = IntArrayMin1.Create((42, [ 1; 2; 3 ]))
            Expect.equal head 42 "head"
            Expect.equal rest [ 1; 2; 3 ] "rest"
        }

    let min1ParseProducesCorrectPair =
        test "minItems=1 Parse produces correct pair" {
            let v = Expect.wantOk (IntArrayMin1.Parse("""[10, 20, 30]""")) "Parse should succeed"
            let (head, rest) = v
            Expect.equal head 10 "head"
            Expect.equal rest [ 20; 30 ] "rest"
        }

    let min1ExactlyMinimumGivesEmptyRest =
        test "minItems=1 with exactly 1 element gives empty rest" {
            let v = Expect.wantOk (IntArrayMin1.Parse("""[99]""")) "Parse should succeed"
            let (head, rest) = v
            Expect.equal head 99 "head"
            Expect.equal rest [] "rest is empty"
        }

    // -- minItems and maxItems together: mandatory prefix, optional tail --

    [<Literal>]
    let intArrayMin1Max2Schema = """{ "type": "array", "items": {"type": "integer"}, "minItems": 1, "maxItems": 2 }"""
    type IntArrayMin1Max2 = JsonSchemaProvider<schema = intArrayMin1Max2Schema>

    let minMaxCreateAtMinimumGivesNone =
        test "minItems=1, maxItems=2: exactly minItems elements gives None for the rest" {
            let (head, rest) = IntArrayMin1Max2.Create((42, None))
            Expect.equal head 42 "head"
            Expect.equal rest None "no second element"
        }

    let minMaxCreateAtMaximumGivesSome =
        test "minItems=1, maxItems=2: maxItems elements gives Some for the rest" {
            let (head, rest) = IntArrayMin1Max2.Create((42, Some 7))
            Expect.equal head 42 "head"
            Expect.equal rest (Some 7) "second element"
        }

    let minMaxParseAtMaximumGivesSome =
        test "minItems=1, maxItems=2: Parse with maxItems elements gives Some for the rest" {
            let v = Expect.wantOk (IntArrayMin1Max2.Parse("""[1, 2]""")) "Parse should succeed"
            Expect.equal v (1, Some 2) "parsed tuple matches the two elements"
        }

    // -- maxItems alone: option<head * tail> --

    [<Literal>]
    let intArrayMax2Schema = """{ "type": "array", "items": {"type": "integer"}, "maxItems": 2 }"""
    type IntArrayMax2 = JsonSchemaProvider<schema = intArrayMax2Schema>

    let maxOnlyEmptyArrayGivesNone =
        test "maxItems=2, no minItems: no elements gives None" {
            Expect.equal (IntArrayMax2.Create(None)) None "empty array with no lower bound"
        }

    let maxOnlyOneElementGivesPartialSome =
        test "maxItems=2, no minItems: one element gives Some(head, None)" {
            Expect.equal (IntArrayMax2.Create(Some(1, None))) (Some(1, None)) "one element, nothing after it"
        }

    let maxOnlyAtLimitGivesFullSome =
        test "maxItems=2, no minItems: at the limit gives Some(head, Some tail)" {
            Expect.equal (IntArrayMax2.Create(Some(1, Some 2))) (Some(1, Some 2)) "both elements present"
        }

    let maxOnlyParseAtLimit =
        test "maxItems=2, no minItems: Parse at the limit round-trips both elements" {
            let v = Expect.wantOk (IntArrayMax2.Parse("""[5, 6]""")) "Parse should succeed"
            Expect.equal v (Some(5, Some 6)) "both elements present"
        }

    // -- maxItems=1: dedicated option<inner> case, not option<inner * tail> --

    [<Literal>]
    let intArrayMax1Schema = """{ "type": "array", "items": {"type": "integer"}, "maxItems": 1 }"""
    type IntArrayMax1 = JsonSchemaProvider<schema = intArrayMax1Schema>

    let maxOneEmptyArrayGivesNone =
        test "maxItems=1: no elements gives None" {
            Expect.equal (IntArrayMax1.Create(None)) None "empty array"
        }

    let maxOneSingleElementGivesSome =
        test "maxItems=1: one element gives Some" {
            Expect.equal (IntArrayMax1.Create(Some 42)) (Some 42) "the single element"
        }

    let maxOneParseSingleElement =
        test "maxItems=1: Parse with one element round-trips it" {
            let v = Expect.wantOk (IntArrayMax1.Parse("""[7]""")) "Parse should succeed"
            Expect.equal v (Some 7) "the single element"
        }

    // -- minItems = maxItems: exact tuple, with and without ignoreSpecificKeywords --

    // NB: IntArrayExact2WithFlag is declared identically to IntArrayExact2 below (neither passes
    // ignoreSpecificKeywords) - inherited as-is from before this restructure. Per ArrayShape.fs,
    // ignoreSpecificKeywords=true would actually force the UnsupportedKeywords/plain-list case
    // ahead of ExactLength, so if the flag were ever added here these two would stop agreeing.
    // Flagging rather than changing it, since fixing it is a test-design decision, not a rename.
    [<Literal>]
    let intArrayExact2Schema = """{ "type": "array", "items": {"type": "integer"}, "minItems": 2, "maxItems": 2 }"""
    type IntArrayExact2 = JsonSchemaProvider<schema = intArrayExact2Schema>
    type IntArrayExact2WithFlag = JsonSchemaProvider<schema = intArrayExact2Schema>

    let exactWithoutFlagCreateProducesTuple =
        test "minItems=maxItems=2, no flag: Create produces the exact tuple" {
            Expect.equal (IntArrayExact2.Create((1, 2))) (1, 2) "exact 2-tuple"
        }

    let exactWithoutFlagParseProducesTuple =
        test "minItems=maxItems=2, no flag: Parse produces the exact tuple" {
            let v = Expect.wantOk (IntArrayExact2.Parse("""[3, 4]""")) "Parse should succeed"
            Expect.equal v (3, 4) "exact 2-tuple"
        }

    let exactWithFlagCreateProducesTuple =
        test "minItems=maxItems=2, second declaration: Create produces the same exact tuple" {
            Expect.equal (IntArrayExact2WithFlag.Create((5, 6))) (5, 6) "exact 2-tuple"
        }

    let exactWithFlagParseProducesTuple =
        test "minItems=maxItems=2, second declaration: Parse produces the same exact tuple" {
            let v = Expect.wantOk (IntArrayExact2WithFlag.Parse("""[7, 8]""")) "Parse should succeed"
            Expect.equal v (7, 8) "exact 2-tuple"
        }

    // -- ToList/ToTuple helpers on root arrays --

    [<Literal>]
    let intArrayExact1Schema = """{ "type": "array", "items": {"type": "integer"}, "minItems": 1, "maxItems": 1 }"""
    type IntArrayExact1 = JsonSchemaProvider<schema = intArrayExact1Schema>

    let exact1ToListGivesSingletonList =
        test "minItems=maxItems=1: ToList gives a singleton list" {
            Expect.equal (IntArrayExact1.ToList(7)) [ 7 ] "ToList wraps the bare element"
        }

    let exact2ToListFlattensNestedPair =
        test "minItems=maxItems=2: ToList flattens (h, tail) down to a plain list" {
            Expect.equal (IntArrayExact2.ToList((1, 2))) [ 1; 2 ] "ToList = [1; 2]"
        }

    let exact2ToTupleRebuildsFlatTuple =
        test "minItems=maxItems=2: ToTuple rebuilds the classic flat tuple" {
            Expect.equal (IntArrayExact2.ToTuple((1, 2))) (1, 2) "ToTuple = (1, 2)"
        }

    // n >= 3 has more than one level of nesting to flatten.
    [<Literal>]
    let intArrayExact3Schema = """{ "type": "array", "items": {"type": "integer"}, "minItems": 3, "maxItems": 3 }"""
    type IntArrayExact3 = JsonSchemaProvider<schema = intArrayExact3Schema>

    [<Literal>]
    let intArrayExact4Schema = """{ "type": "array", "items": {"type": "integer"}, "minItems": 4, "maxItems": 4 }"""
    type IntArrayExact4 = JsonSchemaProvider<schema = intArrayExact4Schema>

    let exact3ToListFlattensNestedPairs =
        test "minItems=maxItems=3: ToList flattens (h, (h, t)) down to a plain list" {
            Expect.equal (IntArrayExact3.ToList((1, (2, 3)))) [ 1; 2; 3 ] "ToList = [1; 2; 3]"
        }

    let exact3ToTupleRebuildsFlatTuple =
        test "minItems=maxItems=3: ToTuple rebuilds the flat 3-tuple" {
            Expect.equal (IntArrayExact3.ToTuple((1, (2, 3)))) (1, 2, 3) "ToTuple = (1, 2, 3)"
        }

    let exact4ToTupleRebuildsFlatTuple =
        test "minItems=maxItems=4: ToTuple rebuilds the flat 4-tuple" {
            Expect.equal (IntArrayExact4.ToTuple((1, (2, (3, 4))))) (1, 2, 3, 4) "ToTuple = (1, 2, 3, 4)"
        }

    let exact3ParseThenToTupleRoundtrips =
        test "minItems=maxItems=3: Parse then ToTuple gives the elements in JSON order" {
            let v = Expect.wantOk (IntArrayExact3.Parse("""[7, 8, 9]""")) "Parse should succeed"
            Expect.equal (IntArrayExact3.ToTuple(v)) (7, 8, 9) "ToTuple = (7, 8, 9)"
        }

    let min1ToListFlattensHeadAndOpenTail =
        test "minItems=1, no maxItems: ToList flattens (h, tail list)" {
            Expect.equal (IntArrayMin1.ToList((1, [ 2; 3 ]))) [ 1; 2; 3 ] "ToList = [1; 2; 3]"
        }

    let min1Max2ToListOnNoneTailGivesSingletonList =
        test "minItems=1, maxItems=2: ToList on (h, None) gives a singleton list" {
            Expect.equal (IntArrayMin1Max2.ToList((1, None))) [ 1 ] "ToList = [1]"
        }

    let min1Max2ToListOnSomeTailGivesBothElements =
        test "minItems=1, maxItems=2: ToList on (h, Some t) gives both elements" {
            Expect.equal (IntArrayMin1Max2.ToList((1, Some 2))) [ 1; 2 ] "ToList = [1; 2]"
        }

    let min2ToListFlattensNestedPrefix =
        test "minItems=2, no maxItems: ToList flattens the mandatory prefix plus the open tail" {
            Expect.equal (StringArrayMin2.ToList(("a", ("b", [ "c"; "d" ])))) [ "a"; "b"; "c"; "d" ] "ToList = [a; b; c; d]"
        }

    let maxOneToListOnNoneGivesEmptyList =
        test "maxItems=1: ToList on None gives an empty list" {
            Expect.equal (IntArrayMax1.ToList(None)) [] "ToList None = []"
        }

    let maxOneToListOnSomeGivesSingletonList =
        test "maxItems=1: ToList on Some gives a singleton list" {
            Expect.equal (IntArrayMax1.ToList(Some 42)) [ 42 ] "ToList (Some 42) = [42]"
        }

    let maxTwoToListOnNoneGivesEmptyList =
        test "maxItems=2, no minItems: ToList on None gives an empty list" {
            Expect.equal (IntArrayMax2.ToList(None)) [] "ToList None = []"
        }

    let maxTwoToListOnPartialSomeGivesSingletonList =
        test "maxItems=2, no minItems: ToList on Some(head, None) gives a singleton list" {
            Expect.equal (IntArrayMax2.ToList(Some(1, None))) [ 1 ] "ToList (Some(1, None)) = [1]"
        }

    let maxTwoToListOnFullSomeGivesTwoElementList =
        test "maxItems=2, no minItems: ToList on Some(head, Some tail) gives both elements" {
            Expect.equal (IntArrayMax2.ToList(Some(1, Some 2))) [ 1; 2 ] "ToList (Some(1, Some 2)) = [1; 2]"
        }

    let plainListToListIsIdentity =
        test "no minItems/maxItems (or ignoreSpecificKeywords): ToList is the identity function" {
            Expect.equal (PlainStringArray.ToList([ "a"; "b"; "c" ])) [ "a"; "b"; "c" ] "already a plain list"
        }

    // -- ToList/ToTuple for arrays inside arrays: helpers on nested "ItemArray" types --

    [<Literal>]
    let arrayOfExact3Schema =
        """{ "type": "array", "items": { "type": "array", "items": {"type": "integer"}, "minItems": 3, "maxItems": 3 } }"""
    type ArrayOfExact3 = JsonSchemaProvider<schema = arrayOfExact3Schema>

    let itemArrayToTupleOnEachElement =
        test "array of exact-3 arrays: List.map ItemArray.ToTuple flattens each inner array" {
            let v = Expect.wantOk (ArrayOfExact3.Parse("""[[1, 2, 3], [4, 5, 6]]""")) "Parse should succeed"
            Expect.equal (v |> List.map ArrayOfExact3.ItemArray.ToTuple) [ (1, 2, 3); (4, 5, 6) ] "each inner array as a flat tuple"
        }

    let itemArrayToListOnEachElement =
        test "array of exact-3 arrays: List.map ItemArray.ToList flattens each inner array" {
            let v = Expect.wantOk (ArrayOfExact3.Parse("""[[1, 2, 3], [4, 5, 6]]""")) "Parse should succeed"
            Expect.equal (v |> List.map ArrayOfExact3.ItemArray.ToList) [ [ 1; 2; 3 ]; [ 4; 5; 6 ] ] "each inner array as a list"
        }

    [<Literal>]
    let arrayOfMax2OfExact2Schema =
        """{ "type": "array", "items": { "type": "array", "maxItems": 2, "items": { "type": "array", "items": {"type": "integer"}, "minItems": 2, "maxItems": 2 } } }"""
    type ArrayOfMax2OfExact2 = JsonSchemaProvider<schema = arrayOfMax2OfExact2Schema>

    let itemArrayItemArrayTwoLevelsDeep =
        test "three array levels: ItemArray.ToList and ItemArray.ItemArray.ToTuple each flatten their own level" {
            let middle = ArrayOfMax2OfExact2.ItemArray.ToList(Some((1, 2), Some(3, 4)))
            Expect.equal (middle |> List.map ArrayOfMax2OfExact2.ItemArray.ItemArray.ToTuple) [ (1, 2); (3, 4) ] "both levels flattened"
        }

    [<Literal>]
    let min1ObjectArraySchema =
        """{ "type": "array", "minItems": 1, "items": { "type": "object", "properties": { "a": {"type": "integer"} }, "required": ["a"] } }"""
    type Min1ObjectArray = JsonSchemaProvider<schema = min1ObjectArraySchema>

    let min1ObjectArrayToList =
        test "minItems=1 array of objects: ToList flattens (h, tail list) of provided-type items" {
            let item = Min1ObjectArray.Item.Create(a = 1)
            Expect.equal (Min1ObjectArray.ToList((item, [])) |> List.map (fun i -> i.a)) [ 1 ] "ToList = [item]"
        }

    [<Literal>]
    let arrayOfMin1ObjectArraysSchema =
        """{ "type": "array", "items": { "type": "array", "minItems": 1, "items": { "type": "object", "properties": { "a": {"type": "integer"} }, "required": ["a"] } } }"""
    type ArrayOfMin1ObjectArrays = JsonSchemaProvider<schema = arrayOfMin1ObjectArraysSchema>

    let itemClassAndItemArrayCoexist =
        test "array of arrays of objects: the Item class and the ItemArray helper don't clash" {
            let item = ArrayOfMin1ObjectArrays.Item.Create(a = 1)
            let flattened = ArrayOfMin1ObjectArrays.ItemArray.ToList((item, []))
            Expect.equal (flattened |> List.map (fun i -> i.a)) [ 1 ] "ToList = [item]"
        }

    // -- uniqueItems: runtime-only, no compile-time encoding --

    [<Literal>]
    let uniqueItemsSchema = """{ "type": "array", "items": { "type": "integer" }, "uniqueItems": true }"""
    type UniqueItemsArray = JsonSchemaProvider<schema=uniqueItemsSchema>

    let duplicateItemsAreRejected =
        test "uniqueItems rejects an array with a duplicate" {
            Expect.isError (UniqueItemsArray.Create([ 1; 2; 2 ])) "[1;2;2] has a duplicate"
        }

    let allUniqueItemsAreAccepted =
        test "uniqueItems accepts an array with no duplicates" {
            let result = Expect.wantOk (UniqueItemsArray.Create([ 1; 2; 3 ])) "Create should succeed"
            Expect.equal result [ 1; 2; 3 ] "no duplicates"
        }

    // -- maxItems: boundary is always structurally enforced, so no rejection test exists here --

    [<Literal>]
    let maxItemsSchema = """{ "type": "array", "items": { "type": "integer" }, "maxItems": 2 }"""
    type MaxItemsArray = JsonSchemaProvider<schema=maxItemsSchema>

    let atMaxItemsIsAccepted =
        test "maxItems accepts an array at the limit" {
            Expect.equal (MaxItemsArray.Create(Some(1, Some 2))) (Some(1, Some 2)) "2 elements is exactly maxItems=2"
        }

    let withinMaxItemsIsAccepted =
        test "maxItems accepts an array within the limit" {
            Expect.equal (MaxItemsArray.Create(Some(1, None))) (Some(1, None)) "1 element is within maxItems=2"
        }

    // -- minItems without ignoreSpecificKeywords still compiles to a tuple (see above); this is
    //    the plain-list case, where minItems is only checked by Create at runtime --

    [<Literal>]
    let minItemsRuntimeSchema = """{ "type": "array", "items": { "type": "string" }, "minItems": 2 }"""
    type MinItemsRuntime = JsonSchemaProvider<schema=minItemsRuntimeSchema, ignoreSpecificKeywords=true>

    let tooFewItemsAreRejectedAtRuntime =
        test "minItems is enforced at runtime by Create, not just as a compile-time tuple shape" {
            Expect.isError (MinItemsRuntime.Create([ "a" ])) "1 element is below minItems=2"
        }

    let enoughItemsAreAcceptedAtRuntime =
        test "minItems is satisfied at runtime when there are enough elements" {
            let result = Expect.wantOk (MinItemsRuntime.Create([ "a"; "b" ])) "Create should succeed"
            Expect.equal result [ "a"; "b" ] "2 elements satisfies minItems=2"
        }

    // -- a constraint on the item schema itself, independent of the array's own keywords --

    [<Literal>]
    let arrayItemConstraintSchema = """{ "type": "array", "items": { "type": "integer", "minimum": 0 } }"""
    type ArrayItemConstraint = JsonSchemaProvider<schema=arrayItemConstraintSchema>

    let arrayWithAnOutOfRangeItemIsRejected =
        test "an item schema's own constraint is enforced against every element" {
            Expect.isError (ArrayItemConstraint.Create([ 1; -1; 3 ])) "-1 violates the item schema's minimum"
        }

    let arrayWithAllInRangeItemsIsAccepted =
        test "an array whose every element satisfies the item schema is accepted" {
            let result = Expect.wantOk (ArrayItemConstraint.Create([ 1; 2; 3 ])) "Create should succeed"
            Expect.equal result [ 1; 2; 3 ] "every element satisfies the minimum"
        }

    // -- general array-property access: not a keyword test, just item-shape/indexing coverage --

    [<Literal>]
    let numberArray =
        """{ "type": "object", "properties": { "values": { "type": "array", "items": {"type": "number"} } }, "required": ["values"] }"""
    type NumberArray = JsonSchemaProvider<schema=numberArray>

    let selectFromNumberArrayShouldYieldInputValue =
        test "select from number array should yield input value" {
            let numArray = NumberArray.Create([ 11.0; 12.0; 11.6; 12.1 ])
            Expect.equal numArray.values[1] 12.0 "numArray.values[1] = 12.0"
        }

    [<Literal>]
    let integerArray =
        """{ "type": "object", "properties": { "values": { "type": "array", "items": {"type": "integer"} } }, "required": ["values"] }"""
    type IntegerArray = JsonSchemaProvider<schema=integerArray>

    let selectFromIntegerArrayShouldYieldInputValue =
        test "select from integer array should yield input value" {
            let numArray = IntegerArray.Create([ 11; 12; 10; 13 ])
            Expect.equal numArray.values[1] 12 "numArray.values[1] = 12"
        }

    [<Literal>]
    let nestedArray =
        """{ "type": "object", "properties": { "values": { "type": "array", "items": { "type": "array", "items": {"type": "string"} } } }, "required": ["values"] }"""
    type NestedArray = JsonSchemaProvider<schema=nestedArray>

    let selectFromNestedArrayShouldYieldInputValue =
        test "select from nested array should yield input value" {
            let array = NestedArray.Create([ [ "a"; "b" ] ])
            Expect.equal (array.values[0][1]) "b" "nestedArray.values[0][1] = \"b\""
        }

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
                  "properties": { "propA": {"type": "integer"}, "propB": {"type": "string"} }
                }
              }
            }
          }
        }"""
    type NestedArrayWithObjectItems = JsonSchemaProvider<schema=nestedArrayWithObjectItems>

    let selectFromNestedArrayWithObjectItemsShouldYieldInputValue =
        test "select from nested array with object items should yield input value" {
            let item = NestedArrayWithObjectItems.valuesItem.Create(propA = 5)
            let array = NestedArrayWithObjectItems.Create([ [ item ] ])
            Expect.equal (array.values.Value[0][0]).propA (Some(5)) "propA roundtrips"
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
              exact1ToListGivesSingletonList
              exact2ToListFlattensNestedPair
              exact2ToTupleRebuildsFlatTuple
              exact3ToListFlattensNestedPairs
              exact3ToTupleRebuildsFlatTuple
              exact4ToTupleRebuildsFlatTuple
              exact3ParseThenToTupleRoundtrips
              min1ToListFlattensHeadAndOpenTail
              min1Max2ToListOnNoneTailGivesSingletonList
              min1Max2ToListOnSomeTailGivesBothElements
              min2ToListFlattensNestedPrefix
              maxOneToListOnNoneGivesEmptyList
              maxOneToListOnSomeGivesSingletonList
              maxTwoToListOnNoneGivesEmptyList
              maxTwoToListOnPartialSomeGivesSingletonList
              maxTwoToListOnFullSomeGivesTwoElementList
              plainListToListIsIdentity
              itemArrayToTupleOnEachElement
              itemArrayToListOnEachElement
              itemArrayItemArrayTwoLevelsDeep
              min1ObjectArrayToList
              itemClassAndItemArrayCoexist
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
