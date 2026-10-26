namespace JsonSchemaProvider.Tests

// Array keywords at the schema root. Object-nested versions of the minItems/maxItems/uniqueItems
// cases live in ObjectKeywordTests.fs. The last section (general array-property access) isn't
// about a keyword - it stays object-wrapped since a property is just where the array lives.
module ArrayKeywordTests =
    open Xunit
    open JsonSchemaProvider

    // -- minItems: compile-time tuple shape --

    [<Literal>]
    let stringArrayMin2Schema = """{ "type": "array", "items": {"type": "string"}, "minItems": 2 }"""
    type PlainStringArray = JsonSchemaProvider<schema = stringArrayMin2Schema, ignoreSpecificKeywords = true>
    type StringArrayMin2 = JsonSchemaProvider<schema = stringArrayMin2Schema>

    [<Fact>]
    let ``minItems schema without compileMinItems flag yields plain list`` () =
        let v = expectOk (PlainStringArray.Parse("""["a", "b", "c"]"""))
        expectEqual v [ "a"; "b"; "c" ]

    [<Fact>]
    let ``minItems=2 Create produces correct tuple`` () =
        let (h1, (h2, rest)) = expectOk (StringArrayMin2.Create(("a", ("b", [ "c"; "d" ]))))
        expectEqual h1 "a"
        expectEqual h2 "b"
        expectEqual rest [ "c"; "d" ]

    [<Fact>]
    let ``minItems=2 Parse produces correct tuple`` () =
        let v = expectOk (StringArrayMin2.Parse("""["x", "y", "z"]"""))
        let (h1, (h2, rest)) = v
        expectEqual h1 "x"
        expectEqual h2 "y"
        expectEqual rest [ "z" ]

    [<Fact>]
    let ``minItems=2 with exactly 2 elements gives empty rest`` () =
        let v = expectOk (StringArrayMin2.Parse("""["a", "b"]"""))
        let (_, (_, rest)) = v
        expectEqual rest []

    [<Literal>]
    let intArrayMin1Schema = """{ "type": "array", "items": {"type": "integer"}, "minItems": 1 }"""
    type IntArrayMin1 = JsonSchemaProvider<schema = intArrayMin1Schema>

    [<Fact>]
    let ``minItems=1 Create produces correct pair`` () =
        let (head, rest) = IntArrayMin1.Create((42, [ 1; 2; 3 ]))
        expectEqual head 42
        expectEqual rest [ 1; 2; 3 ]

    [<Fact>]
    let ``minItems=1 Parse produces correct pair`` () =
        let v = expectOk (IntArrayMin1.Parse("""[10, 20, 30]"""))
        let (head, rest) = v
        expectEqual head 10
        expectEqual rest [ 20; 30 ]

    [<Fact>]
    let ``minItems=1 with exactly 1 element gives empty rest`` () =
        let v = expectOk (IntArrayMin1.Parse("""[99]"""))
        let (head, rest) = v
        expectEqual head 99
        expectEqual rest []

    // -- minItems and maxItems together: mandatory prefix, optional tail --

    [<Literal>]
    let intArrayMin1Max2Schema = """{ "type": "array", "items": {"type": "integer"}, "minItems": 1, "maxItems": 2 }"""
    type IntArrayMin1Max2 = JsonSchemaProvider<schema = intArrayMin1Max2Schema>

    [<Fact>]
    let ``minItems=1, maxItems=2: exactly minItems elements gives None for the rest`` () =
        let (head, rest) = IntArrayMin1Max2.Create((42, None))
        expectEqual head 42
        expectEqual rest None

    [<Fact>]
    let ``minItems=1, maxItems=2: maxItems elements gives Some for the rest`` () =
        let (head, rest) = IntArrayMin1Max2.Create((42, Some 7))
        expectEqual head 42
        expectEqual rest (Some 7)

    [<Fact>]
    let ``minItems=1, maxItems=2: Parse with maxItems elements gives Some for the rest`` () =
        let v = expectOk (IntArrayMin1Max2.Parse("""[1, 2]"""))
        expectEqual v (1, Some 2)

    // -- maxItems alone: option<head * tail> --

    [<Literal>]
    let intArrayMax2Schema = """{ "type": "array", "items": {"type": "integer"}, "maxItems": 2 }"""
    type IntArrayMax2 = JsonSchemaProvider<schema = intArrayMax2Schema>

    [<Fact>]
    let ``maxItems=2, no minItems: no elements gives None`` () =
        expectEqual (IntArrayMax2.Create(None)) None

    [<Fact>]
    let ``maxItems=2, no minItems: one element gives Some(head, None)`` () =
        expectEqual (IntArrayMax2.Create(Some(1, None))) (Some(1, None))

    [<Fact>]
    let ``maxItems=2, no minItems: at the limit gives Some(head, Some tail)`` () =
        expectEqual (IntArrayMax2.Create(Some(1, Some 2))) (Some(1, Some 2))

    [<Fact>]
    let ``maxItems=2, no minItems: Parse at the limit round-trips both elements`` () =
        let v = expectOk (IntArrayMax2.Parse("""[5, 6]"""))
        expectEqual v (Some(5, Some 6))

    // -- maxItems=1: dedicated option<inner> case, not option<inner * tail> --

    [<Literal>]
    let intArrayMax1Schema = """{ "type": "array", "items": {"type": "integer"}, "maxItems": 1 }"""
    type IntArrayMax1 = JsonSchemaProvider<schema = intArrayMax1Schema>

    [<Fact>]
    let ``maxItems=1: no elements gives None`` () =
        expectEqual (IntArrayMax1.Create(None)) None

    [<Fact>]
    let ``maxItems=1: one element gives Some`` () =
        expectEqual (IntArrayMax1.Create(Some 42)) (Some 42)

    [<Fact>]
    let ``maxItems=1: Parse with one element round-trips it`` () =
        let v = expectOk (IntArrayMax1.Parse("""[7]"""))
        expectEqual v (Some 7)

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

    [<Fact>]
    let ``minItems=maxItems=2, no flag: Create produces the exact tuple`` () =
        expectEqual (IntArrayExact2.Create((1, 2))) (1, 2)

    [<Fact>]
    let ``minItems=maxItems=2, no flag: Parse produces the exact tuple`` () =
        let v = expectOk (IntArrayExact2.Parse("""[3, 4]"""))
        expectEqual v (3, 4)

    [<Fact>]
    let ``minItems=maxItems=2, second declaration: Create produces the same exact tuple`` () =
        expectEqual (IntArrayExact2WithFlag.Create((5, 6))) (5, 6)

    [<Fact>]
    let ``minItems=maxItems=2, second declaration: Parse produces the same exact tuple`` () =
        let v = expectOk (IntArrayExact2WithFlag.Parse("""[7, 8]"""))
        expectEqual v (7, 8)

    // -- ToList/ToTuple helpers on root arrays --

    [<Literal>]
    let intArrayExact1Schema = """{ "type": "array", "items": {"type": "integer"}, "minItems": 1, "maxItems": 1 }"""
    type IntArrayExact1 = JsonSchemaProvider<schema = intArrayExact1Schema>

    [<Fact>]
    let ``minItems=maxItems=1: ToList gives a singleton list`` () =
        expectEqual (IntArrayExact1.ToList(7)) [ 7 ]

    [<Fact>]
    let ``minItems=maxItems=2: ToList flattens (h, tail) down to a plain list`` () =
        expectEqual (IntArrayExact2.ToList((1, 2))) [ 1; 2 ]

    [<Fact>]
    let ``minItems=maxItems=2: ToTuple rebuilds the classic flat tuple`` () =
        expectEqual (IntArrayExact2.ToTuple((1, 2))) (1, 2)

    // n >= 3 has more than one level of nesting to flatten.
    [<Literal>]
    let intArrayExact3Schema = """{ "type": "array", "items": {"type": "integer"}, "minItems": 3, "maxItems": 3 }"""
    type IntArrayExact3 = JsonSchemaProvider<schema = intArrayExact3Schema>

    [<Literal>]
    let intArrayExact4Schema = """{ "type": "array", "items": {"type": "integer"}, "minItems": 4, "maxItems": 4 }"""
    type IntArrayExact4 = JsonSchemaProvider<schema = intArrayExact4Schema>

    [<Fact>]
    let ``minItems=maxItems=3: ToList flattens (h, (h, t)) down to a plain list`` () =
        expectEqual (IntArrayExact3.ToList((1, (2, 3)))) [ 1; 2; 3 ]

    [<Fact>]
    let ``minItems=maxItems=3: ToTuple rebuilds the flat 3-tuple`` () =
        expectEqual (IntArrayExact3.ToTuple((1, (2, 3)))) (1, 2, 3)

    [<Fact>]
    let ``minItems=maxItems=4: ToTuple rebuilds the flat 4-tuple`` () =
        expectEqual (IntArrayExact4.ToTuple((1, (2, (3, 4))))) (1, 2, 3, 4)

    [<Fact>]
    let ``minItems=maxItems=3: Parse then ToTuple gives the elements in JSON order`` () =
        let v = expectOk (IntArrayExact3.Parse("""[7, 8, 9]"""))
        expectEqual (IntArrayExact3.ToTuple(v)) (7, 8, 9)

    [<Fact>]
    let ``minItems=1, no maxItems: ToList flattens (h, tail list)`` () =
        expectEqual (IntArrayMin1.ToList((1, [ 2; 3 ]))) [ 1; 2; 3 ]

    [<Fact>]
    let ``minItems=1, maxItems=2: ToList on (h, None) gives a singleton list`` () =
        expectEqual (IntArrayMin1Max2.ToList((1, None))) [ 1 ]

    [<Fact>]
    let ``minItems=1, maxItems=2: ToList on (h, Some t) gives both elements`` () =
        expectEqual (IntArrayMin1Max2.ToList((1, Some 2))) [ 1; 2 ]

    [<Fact>]
    let ``minItems=2, no maxItems: ToList flattens the mandatory prefix plus the open tail`` () =
        expectEqual (StringArrayMin2.ToList(("a", ("b", [ "c"; "d" ])))) [ "a"; "b"; "c"; "d" ]

    [<Fact>]
    let ``maxItems=1: ToList on None gives an empty list`` () =
        expectEqual (IntArrayMax1.ToList(None)) []

    [<Fact>]
    let ``maxItems=1: ToList on Some gives a singleton list`` () =
        expectEqual (IntArrayMax1.ToList(Some 42)) [ 42 ]

    [<Fact>]
    let ``maxItems=2, no minItems: ToList on None gives an empty list`` () =
        expectEqual (IntArrayMax2.ToList(None)) []

    [<Fact>]
    let ``maxItems=2, no minItems: ToList on Some(head, None) gives a singleton list`` () =
        expectEqual (IntArrayMax2.ToList(Some(1, None))) [ 1 ]

    [<Fact>]
    let ``maxItems=2, no minItems: ToList on Some(head, Some tail) gives both elements`` () =
        expectEqual (IntArrayMax2.ToList(Some(1, Some 2))) [ 1; 2 ]

    [<Fact>]
    let ``no minItems/maxItems (or ignoreSpecificKeywords): ToList is the identity function`` () =
        expectEqual (PlainStringArray.ToList([ "a"; "b"; "c" ])) [ "a"; "b"; "c" ]

    // -- ToList/ToTuple for arrays inside arrays: helpers on nested "ItemArray" types --

    [<Literal>]
    let arrayOfExact3Schema =
        """{ "type": "array", "items": { "type": "array", "items": {"type": "integer"}, "minItems": 3, "maxItems": 3 } }"""
    type ArrayOfExact3 = JsonSchemaProvider<schema = arrayOfExact3Schema>

    [<Fact>]
    let ``array of exact-3 arrays: List.map ItemArray.ToTuple flattens each inner array`` () =
        let v = expectOk (ArrayOfExact3.Parse("""[[1, 2, 3], [4, 5, 6]]"""))
        expectEqual (v |> List.map ArrayOfExact3.ItemArray.ToTuple) [ (1, 2, 3); (4, 5, 6) ]

    [<Fact>]
    let ``array of exact-3 arrays: List.map ItemArray.ToList flattens each inner array`` () =
        let v = expectOk (ArrayOfExact3.Parse("""[[1, 2, 3], [4, 5, 6]]"""))
        expectEqual (v |> List.map ArrayOfExact3.ItemArray.ToList) [ [ 1; 2; 3 ]; [ 4; 5; 6 ] ]

    [<Literal>]
    let arrayOfMax2OfExact2Schema =
        """{ "type": "array", "items": { "type": "array", "maxItems": 2, "items": { "type": "array", "items": {"type": "integer"}, "minItems": 2, "maxItems": 2 } } }"""
    type ArrayOfMax2OfExact2 = JsonSchemaProvider<schema = arrayOfMax2OfExact2Schema>

    [<Fact>]
    let ``three array levels: ItemArray.ToList and ItemArray.ItemArray.ToTuple each flatten their own level`` () =
        let middle = ArrayOfMax2OfExact2.ItemArray.ToList(Some((1, 2), Some(3, 4)))
        expectEqual (middle |> List.map ArrayOfMax2OfExact2.ItemArray.ItemArray.ToTuple) [ (1, 2); (3, 4) ]

    [<Literal>]
    let min1ObjectArraySchema =
        """{ "type": "array", "minItems": 1, "items": { "type": "object", "properties": { "a": {"type": "integer"} }, "required": ["a"] } }"""
    type Min1ObjectArray = JsonSchemaProvider<schema = min1ObjectArraySchema>

    [<Fact>]
    let ``minItems=1 array of objects: ToList flattens (h, tail list) of provided-type items`` () =
        let item = Min1ObjectArray.Item.Create(a = 1)
        expectEqual (Min1ObjectArray.ToList((item, [])) |> List.map (fun i -> i.a)) [ 1 ]

    [<Literal>]
    let arrayOfMin1ObjectArraysSchema =
        """{ "type": "array", "items": { "type": "array", "minItems": 1, "items": { "type": "object", "properties": { "a": {"type": "integer"} }, "required": ["a"] } } }"""
    type ArrayOfMin1ObjectArrays = JsonSchemaProvider<schema = arrayOfMin1ObjectArraysSchema>

    [<Fact>]
    let ``array of arrays of objects: the Item class and the ItemArray helper don't clash`` () =
        let item = ArrayOfMin1ObjectArrays.Item.Create(a = 1)
        let flattened = ArrayOfMin1ObjectArrays.ItemArray.ToList((item, []))
        expectEqual (flattened |> List.map (fun i -> i.a)) [ 1 ]

    // -- uniqueItems: plain list, ToJson drops duplicates --

    [<Literal>]
    let uniqueItemsSchema = """{ "type": "array", "items": { "type": "integer" }, "uniqueItems": true }"""
    type UniqueItemsArray = JsonSchemaProvider<schema=uniqueItemsSchema>

    [<Fact>]
    let ``uniqueItems Create takes a plain list and needs no Result`` () =
        let value: int list = UniqueItemsArray.Create([ 3; 1; 2 ])
        expectEqual value [ 3; 1; 2 ]

    [<Fact>]
    let ``uniqueItems Create drops duplicates, keeping each first occurrence`` () =
        expectEqual (UniqueItemsArray.Create([ 3; 1; 3; 2; 1 ])) [ 3; 1; 2 ]

    [<Literal>]
    let uniqueItemsInTupleSchema =
        """{ "type": "array", "items": { "type": "array", "items": { "type": "integer" }, "uniqueItems": true }, "minItems": 2, "maxItems": 2 }"""
    type UniqueItemsInTuple = JsonSchemaProvider<schema=uniqueItemsInTupleSchema>

    [<Fact>]
    let ``uniqueItems arrays inside a size-bounded tuple drop their duplicates`` () =
        expectEqual (UniqueItemsInTuple.Create(([ 1; 1; 2 ], [ 3; 3 ]))) ([ 1; 2 ], [ 3 ])
        expectEqual (expectOk (UniqueItemsInTuple.Parse("""[[2, 1], [3]]"""))) ([ 2; 1 ], [ 3 ])
        expectError (UniqueItemsInTuple.Parse("""[[1, 1], [3]]"""))

    [<Literal>]
    let uniqueItemsOneOfBranchSchema =
        """{ "oneOf": [ { "type": "array", "items": { "type": "integer" }, "uniqueItems": true }, { "type": "string" } ] }"""
    type UniqueItemsOneOfBranch = JsonSchemaProvider<schema=uniqueItemsOneOfBranchSchema>

    [<Fact>]
    let ``uniqueItems array as a oneOf branch drops duplicates before validation`` () =
        expectEqual (expectOk (UniqueItemsOneOfBranch.Create(Choice1Of2 [ 2; 1; 2 ]))) (Choice1Of2 [ 2; 1 ])
        expectEqual (expectOk (UniqueItemsOneOfBranch.Parse("""[2, 1]"""))) (Choice1Of2 [ 2; 1 ])
        expectError (UniqueItemsOneOfBranch.Parse("""[1, 1]"""))

    [<Fact>]
    let ``uniqueItems Parse keeps the order`` () =
        expectEqual (expectOk (UniqueItemsArray.Parse("""[3, 1, 2]"""))) [ 3; 1; 2 ]

    [<Fact>]
    let ``uniqueItems Parse rejects JSON with a duplicate`` () =
        expectError (UniqueItemsArray.Parse("""[1, 2, 2]"""))

    [<Literal>]
    let uniqueStringsSchema = """{ "type": "array", "items": { "type": "string" }, "uniqueItems": true }"""
    type UniqueStringsArray = JsonSchemaProvider<schema=uniqueStringsSchema>

    [<Fact>]
    let ``uniqueItems on strings: Create validates, and the duplicate is dropped before that`` () =
        expectEqual (expectOk (UniqueStringsArray.Create([ "b"; "a"; "b" ]))) [ "b"; "a" ]

    [<Literal>]
    let nestedUniqueItemsSchema =
        """{ "type": "array", "items": { "type": "array", "items": { "type": "integer" }, "uniqueItems": true }, "uniqueItems": true }"""
    type NestedUniqueItemsArray = JsonSchemaProvider<schema=nestedUniqueItemsSchema>

    [<Fact>]
    let ``nested uniqueItems: inner lists in a different order are different elements`` () =
        expectEqual (expectOk (NestedUniqueItemsArray.Parse("""[[1, 2], [2, 1]]"""))) [ [ 1; 2 ]; [ 2; 1 ] ]
        expectError (NestedUniqueItemsArray.Parse("""[[1, 2], [1, 2]]"""))

    [<Literal>]
    let uniqueNumbersSchema = """{ "type": "array", "items": { "type": "number" }, "uniqueItems": true }"""
    type UniqueNumbersArray = JsonSchemaProvider<schema=uniqueNumbersSchema>

    [<Fact>]
    let ``uniqueItems on numbers: 1 and 1.0 are the same JSON number`` () =
        expectError (UniqueNumbersArray.Parse("""[1, 1.0]"""))

    [<Literal>]
    let uniqueMin2Schema = """{ "type": "array", "items": { "type": "integer" }, "minItems": 2, "uniqueItems": true }"""
    type UniqueMin2Array = JsonSchemaProvider<schema=uniqueMin2Schema>

    [<Fact>]
    let ``uniqueItems with minItems is a plain list checked at runtime`` () =
        expectEqual (expectOk (UniqueMin2Array.Create([ 2; 1 ]))) [ 2; 1 ] // order is kept
        expectError (UniqueMin2Array.Create([ 1 ]))
        expectError (UniqueMin2Array.Create([ 1; 1 ]))
        expectError (UniqueMin2Array.Parse("""[1, 2, 2]"""))

    [<Literal>]
    let uniqueMax2Schema = """{ "type": "array", "items": { "type": "integer" }, "maxItems": 2, "uniqueItems": true }"""
    type UniqueMax2Array = JsonSchemaProvider<schema=uniqueMax2Schema>

    [<Fact>]
    let ``uniqueItems with maxItems keeps the option chain and Create needs no Result`` () =
        expectEqual (UniqueMax2Array.Create(Some(1, Some 2))) (Some(1, Some 2))
        expectEqual (UniqueMax2Array.Create(None)) None
        expectEqual (expectOk (UniqueMax2Array.Parse("""[2, 1]"""))) (Some(2, Some 1))
        expectError (UniqueMax2Array.Parse("""[1, 1]"""))

    [<Literal>]
    let uniqueObjectItemsSchema =
        """{ "type": "array", "items": { "type": "object", "properties": { "a": { "type": "integer" } }, "required": ["a"] }, "uniqueItems": true }"""
    type UniqueObjectItemsArray = JsonSchemaProvider<schema=uniqueObjectItemsSchema>

    [<Fact>]
    let ``uniqueItems on object items stays a list checked at runtime`` () =
        let item a = UniqueObjectItemsArray.Item.Create(a = a)
        let ok = expectOk (UniqueObjectItemsArray.Create([ item 1; item 2 ]))
        expectEqual (ok |> List.map (fun i -> i.a)) [ 1; 2 ]
        expectError (UniqueObjectItemsArray.Create([ item 1; item 1 ]))

    // -- maxItems: boundary is always structurally enforced, so no rejection test exists here --

    [<Literal>]
    let maxItemsSchema = """{ "type": "array", "items": { "type": "integer" }, "maxItems": 2 }"""
    type MaxItemsArray = JsonSchemaProvider<schema=maxItemsSchema>

    [<Fact>]
    let ``maxItems accepts an array at the limit`` () =
        expectEqual (MaxItemsArray.Create(Some(1, Some 2))) (Some(1, Some 2))

    [<Fact>]
    let ``maxItems accepts an array within the limit`` () =
        expectEqual (MaxItemsArray.Create(Some(1, None))) (Some(1, None))

    // -- minItems without ignoreSpecificKeywords still compiles to a tuple (see above); this is
    //    the plain-list case, where minItems is only checked by Create at runtime --

    [<Literal>]
    let minItemsRuntimeSchema = """{ "type": "array", "items": { "type": "string" }, "minItems": 2 }"""
    type MinItemsRuntime = JsonSchemaProvider<schema=minItemsRuntimeSchema, ignoreSpecificKeywords=true>

    [<Fact>]
    let ``minItems is enforced at runtime by Create, not just as a compile-time tuple shape`` () =
        expectError (MinItemsRuntime.Create([ "a" ])) // 1 element is below minItems=2

    [<Fact>]
    let ``minItems is satisfied at runtime when there are enough elements`` () =
        let result = expectOk (MinItemsRuntime.Create([ "a"; "b" ]))
        expectEqual result [ "a"; "b" ]

    // -- a constraint on the item schema itself, independent of the array's own keywords --

    [<Literal>]
    let arrayItemConstraintSchema = """{ "type": "array", "items": { "type": "integer", "minimum": 0 } }"""
    type ArrayItemConstraint = JsonSchemaProvider<schema=arrayItemConstraintSchema>

    [<Fact>]
    let ``an item schema's own constraint is enforced against every element`` () =
        expectError (ArrayItemConstraint.Create([ 1; -1; 3 ])) // -1 violates the item schema's minimum

    [<Fact>]
    let ``an array whose every element satisfies the item schema is accepted`` () =
        let result = expectOk (ArrayItemConstraint.Create([ 1; 2; 3 ]))
        expectEqual result [ 1; 2; 3 ]

    // -- general array-property access: not a keyword test, just item-shape/indexing coverage --

    [<Literal>]
    let numberArray =
        """{ "type": "object", "properties": { "values": { "type": "array", "items": {"type": "number"} } }, "required": ["values"] }"""
    type NumberArray = JsonSchemaProvider<schema=numberArray>

    [<Fact>]
    let ``select from number array should yield input value`` () =
        let numArray = NumberArray.Create([ 11.0; 12.0; 11.6; 12.1 ])
        expectEqual numArray.values[1] 12.0

    [<Literal>]
    let integerArray =
        """{ "type": "object", "properties": { "values": { "type": "array", "items": {"type": "integer"} } }, "required": ["values"] }"""
    type IntegerArray = JsonSchemaProvider<schema=integerArray>

    [<Fact>]
    let ``select from integer array should yield input value`` () =
        let numArray = IntegerArray.Create([ 11; 12; 10; 13 ])
        expectEqual numArray.values[1] 12

    [<Literal>]
    let nestedArray =
        """{ "type": "object", "properties": { "values": { "type": "array", "items": { "type": "array", "items": {"type": "string"} } } }, "required": ["values"] }"""
    type NestedArray = JsonSchemaProvider<schema=nestedArray>

    [<Fact>]
    let ``select from nested array should yield input value`` () =
        let array = expectOk (NestedArray.Create([ [ "a"; "b" ] ]))
        expectEqual (array.values[0][1]) "b"

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

    [<Fact>]
    let ``select from nested array with object items should yield input value`` () =
        let item = expectOk (NestedArrayWithObjectItems.valuesItem.Create(propA = 5))
        let array = expectOk (NestedArrayWithObjectItems.Create([ [ item ] ]))
        expectEqual (array.values.Value[0][0]).propA (Some(5))
