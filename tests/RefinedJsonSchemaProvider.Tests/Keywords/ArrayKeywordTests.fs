namespace RefinedJsonSchemaProvider.Tests

// Array keywords at the schema root. Object-nested versions of the minItems/maxItems/uniqueItems
// cases live in ObjectKeywordTests.fs. The last section (general array-property access) isn't
// about a keyword - it stays object-wrapped since a property is just where the array lives.
module ArrayKeywordTests =
    open Xunit
    open FsCheck
    open FsCheck.FSharp
    open FsCheck.Xunit
    open RefinedJsonSchemaProvider

    // -- minItems: compile-time tuple shape --

    [<Literal>]
    let stringArrayMin2Schema =
        """
        {
            "type": "array",
            "items": { "type": "string" },
            "minItems": 2
        }
        """

    type StringArrayMin2 = RefinedJsonSchemaProvider<schema = stringArrayMin2Schema>
    type PlainStringArray = RefinedJsonSchemaProvider<schema = stringArrayMin2Schema, refinedTypes = false>

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

    [<Fact>]
    let ``minItems=2, no maxItems: ToList flattens the mandatory prefix plus the open tail`` () =
        expectEqual (StringArrayMin2.ToList(("a", ("b", [ "c"; "d" ])))) [ "a"; "b"; "c"; "d" ]

    [<Fact>]
    let ``no minItems/maxItems (or refinedTypes = false): ToList is the identity function`` () =
        expectEqual (PlainStringArray.ToList([ "a"; "b"; "c" ])) [ "a"; "b"; "c" ]

    [<Property>]
    let ``Parse accepts exactly the arrays with at least two items`` (items: list<string>) =
        let json = System.Text.Json.JsonSerializer.Serialize items
        let n = List.length items
        (match StringArrayMin2.Parse json with
        | Ok (v1, (v2, rest) as res) ->
            let v1', (v2', rest') as res' = StringArrayMin2.Create res |> expectOk

            let check v1 v2 rest res =
                n >= 2 &&
                StringArrayMin2.ToList res = items &&
                List.skip 2 items = rest &&
                List.head items = v1 &&
                List.head (List.tail items) = v2

            check v1 v2 rest res && check v1' v2' rest' res'

        | Error _ -> n < 2)
        |> Prop.classify (n < 2) "fewer than 2 items"
        |> Prop.classify (n >= 2) "at least 2 items"

    [<Property>]
    let ``refinedTypes = false, minItems=2: Parse accepts exactly the arrays with at least two items`` (items: list<string>) =
        let json = System.Text.Json.JsonSerializer.Serialize items
        let n = List.length items
        (match PlainStringArray.Parse json with
         | Ok res ->
            let check res = n >= 2 && PlainStringArray.ToList res = items && res = items
            check res && check (PlainStringArray.Create res |> expectOk)
         | Error _ -> n < 2)
        |> Prop.classify (n >= 2) "at least 2 items"
        |> Prop.classify (n < 2) "fewer than 2 items"

    // #region array-tests
    [<Literal>]
    let intArrayMin1Schema =
        """
        {
            "type": "array",
            "items": { "type": "integer" },
            "minItems": 1
        }
        """

    type IntArrayMin1 = RefinedJsonSchemaProvider<schema = intArrayMin1Schema>

    [<Fact>]
    let ``minItems=1 Create produces correct pair`` () =
        let head, rest = IntArrayMin1.Create((42, [ 1; 2; 3 ]))
        expectEqual head 42
        expectEqual rest [ 1; 2; 3 ]

    [<Fact>]
    let ``minItems=1 Parse produces correct pair`` () =
        let head, rest = expectOk (IntArrayMin1.Parse"[10, 20, 30]")
        expectEqual head 10
        expectEqual rest [ 20; 30 ]

    // #elide-start array-tests
    [<Fact>]
    let ``minItems=1 with exactly 1 element gives empty rest`` () =
        let head, rest = expectOk (IntArrayMin1.Parse"[99]")
        expectEqual head 99
        expectEqual rest []

    [<Fact>]
    let ``minItems=1, no maxItems: ToList flattens (h, tail list)`` () =
        expectEqual (IntArrayMin1.ToList((1, [ 2; 3 ]))) [ 1; 2; 3 ]
    // #elide-end array-tests

    [<Property>]
    let ``Parse accepts exactly the arrays with at least one item`` (items: list<int>) =
        let json = System.Text.Json.JsonSerializer.Serialize items
        (match IntArrayMin1.Parse json with
         | Ok (v1, rest as res) ->
            let v1', rest' as res' = IntArrayMin1.Create res

            let check v1 rest res =
                List.length items >= 1 &&
                IntArrayMin1.ToList res = items &&
                List.skip 1 items = rest &&
                List.head items = v1

            check v1 rest res && check v1' rest' res'

         | Error _ -> List.length items < 1)
        |> Prop.classify (List.length items < 1) "no items"
        |> Prop.classify (List.length items >= 1) "at least 1 item"
    // #endregion array-tests

    // -- minItems and maxItems together: mandatory prefix, optional tail --

    [<Literal>]
    let intArrayMin1Max2Schema =
        """
        {
            "type": "array",
            "items": {
                "type": "integer",
                "minimum": -2147483648,
                "maximum": 2147483647
            },
            "minItems": 1,
            "maxItems": 2
        }
        """
    type IntArrayMin1Max2 = RefinedJsonSchemaProvider<schema = intArrayMin1Max2Schema>

    [<Fact>]
    let ``Create with minItems elements gives None for the tail`` () =
        let (head, tail) = IntArrayMin1Max2.Create((42, None))
        expectEqual head 42
        expectEqual tail None

    [<Fact>]
    let ``Create with maxItems elements gives Some for the tail`` () =
        let (head, tail) = IntArrayMin1Max2.Create((42, Some 7))
        expectEqual head 42
        expectEqual tail (Some 7)

    [<Fact>]
    let ``Parse with maxItems elements gives Some for the tail`` () =
        let v = expectOk (IntArrayMin1Max2.Parse("[1, 2]"))
        expectEqual v (1, Some 2)

    [<Fact>]
    let ``minItems=1, maxItems=2: ToList on (h, None) gives a singleton list`` () =
        expectEqual (IntArrayMin1Max2.ToList((1, None))) [ 1 ]

    [<Fact>]
    let ``minItems=1, maxItems=2: ToList on (h, Some t) gives both elements`` () =
        expectEqual (IntArrayMin1Max2.ToList((1, Some 2))) [ 1; 2 ]

    [<Property>]
    let ``minItems=1, maxItems=2: Parse accepts exactly the arrays with one or two items`` (items: list<int>) =
        let json = System.Text.Json.JsonSerializer.Serialize items
        let n = List.length items
        (match IntArrayMin1Max2.Parse json with
         | Ok res ->
            let check res =
                1 <= n && n <= 2 &&
                IntArrayMin1Max2.ToList res = items &&
                (match res with
                 | v1, None -> items = [ v1 ]
                 | v1, Some v2 -> items = [ v1; v2 ])
            check res && check (IntArrayMin1Max2.Create res)
         | Error _ -> n < 1 || n > 2)
        |> Prop.classify (1 <= n && n <= 2) "1 or 2 items"
        |> Prop.classify (n < 1 || n > 2) "fewer than 1 or more than 2 items"

    // -- maxItems alone: option<head * tail> --

    [<Literal>]
    let intArrayMax2Schema = """{ "type": "array", "items": {"type": "integer"}, "maxItems": 2 }"""
    type IntArrayMax2 = RefinedJsonSchemaProvider<schema = intArrayMax2Schema>

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

    [<Fact>]
    let ``maxItems=2, no minItems: ToList on None gives an empty list`` () =
        expectEqual (IntArrayMax2.ToList(None)) []

    [<Fact>]
    let ``maxItems=2, no minItems: ToList on Some(head, None) gives a singleton list`` () =
        expectEqual (IntArrayMax2.ToList(Some(1, None))) [ 1 ]

    [<Fact>]
    let ``maxItems=2, no minItems: ToList on Some(head, Some tail) gives both elements`` () =
        expectEqual (IntArrayMax2.ToList(Some(1, Some 2))) [ 1; 2 ]

    [<Property>]
    let ``maxItems=2: Parse accepts exactly the arrays with at most two items`` (items: list<int>) =
        let json = System.Text.Json.JsonSerializer.Serialize items
        let n = List.length items
        (match IntArrayMax2.Parse json with
         | Ok res ->
            let check res =
                n <= 2 &&
                IntArrayMax2.ToList res = items &&
                (match res with
                 | None -> items = []
                 | Some (v1, None) -> items = [ v1 ]
                 | Some (v1, Some v2) -> items = [ v1; v2 ])
            check res && check (IntArrayMax2.Create res)
         | Error _ -> n > 2)
        |> Prop.classify (n <= 2) "at most 2 items"
        |> Prop.classify (n > 2) "more than 2 items"

    // -- maxItems=1: dedicated option<inner> case, not option<inner * tail> --

    [<Literal>]
    let intArrayMax1Schema = """{ "type": "array", "items": {"type": "integer"}, "maxItems": 1 }"""
    type IntArrayMax1 = RefinedJsonSchemaProvider<schema = intArrayMax1Schema>

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

    [<Fact>]
    let ``maxItems=1: ToList on None gives an empty list`` () =
        expectEqual (IntArrayMax1.ToList(None)) []

    [<Fact>]
    let ``maxItems=1: ToList on Some gives a singleton list`` () =
        expectEqual (IntArrayMax1.ToList(Some 42)) [ 42 ]

    [<Property>]
    let ``maxItems=1: Parse accepts exactly the arrays with at most one item`` (items: list<int>) =
        let json = System.Text.Json.JsonSerializer.Serialize items
        let n = List.length items
        (match IntArrayMax1.Parse json with
         | Ok res ->
            let check res =
                n <= 1 &&
                IntArrayMax1.ToList res = items &&
                (match res with
                 | None -> items = []
                 | Some v1 -> items = [ v1 ])
            check res && check (IntArrayMax1.Create res)
         | Error _ -> n > 1)
        |> Prop.classify (n <= 1) "at most 1 item"
        |> Prop.classify (n > 1) "more than 1 item"

    // -- minItems = maxItems: exact tuple, with refinedTypes on and off 

    [<Literal>]
    let intArrayExact2Schema = """{ "type": "array", "items": {"type": "integer"}, "minItems": 2, "maxItems": 2 }"""
    type IntArrayExact2 = RefinedJsonSchemaProvider<schema = intArrayExact2Schema>
    type IntArrayExact2WithFlag = RefinedJsonSchemaProvider<schema = intArrayExact2Schema>

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

    [<Fact>]
    let ``minItems=maxItems=2: ToList flattens (h, tail) down to a plain list`` () =
        expectEqual (IntArrayExact2.ToList((1, 2))) [ 1; 2 ]

    [<Fact>]
    let ``minItems=maxItems=2: ToTuple rebuilds the classic flat tuple`` () =
        expectEqual (IntArrayExact2.ToTuple((1, 2))) (1, 2)

    [<Property>]
    let ``minItems=maxItems=2: Parse accepts exactly the arrays with two items`` (items: list<int>) =
        let json = System.Text.Json.JsonSerializer.Serialize items
        let n = List.length items
        (match IntArrayExact2.Parse json with
         | Ok res ->
            let check ((v1, v2) as res) =
                n = 2 && IntArrayExact2.ToList res = items && items = [ v1; v2 ]
            check res && check (IntArrayExact2.Create res)
         | Error _ -> n <> 2)
        |> Prop.classify (n = 2) "exactly 2 items"
        |> Prop.classify (n <> 2) "not 2 items"

    [<Literal>]
    let intArrayExact1Schema = """{ "type": "array", "items": {"type": "integer"}, "minItems": 1, "maxItems": 1 }"""
    type IntArrayExact1 = RefinedJsonSchemaProvider<schema = intArrayExact1Schema>

    [<Fact>]
    let ``minItems=maxItems=1: ToList gives a singleton list`` () =
        expectEqual (IntArrayExact1.ToList(7)) [ 7 ]

    [<Property>]
    let ``minItems=maxItems=1: Parse accepts exactly the arrays with one item`` (items: list<int>) =
        let json = System.Text.Json.JsonSerializer.Serialize items
        let n = List.length items
        (match IntArrayExact1.Parse json with
         | Ok res ->
            let check res = n = 1 && IntArrayExact1.ToList res = items && items = [ res ]
            check res && check (IntArrayExact1.Create res)
         | Error _ -> n <> 1)
        |> Prop.classify (n = 1) "exactly 1 item"
        |> Prop.classify (n <> 1) "not 1 item"

    // n >= 3 has more than one level of nesting to flatten.

    [<Literal>]
    let intArrayExact3Schema = """{ "type": "array", "items": {"type": "integer"}, "minItems": 3, "maxItems": 3 }"""
    type IntArrayExact3 = RefinedJsonSchemaProvider<schema = intArrayExact3Schema>

    [<Fact>]
    let ``minItems=maxItems=3: ToList flattens (h, (h, t)) down to a plain list`` () =
        expectEqual (IntArrayExact3.ToList((1, (2, 3)))) [ 1; 2; 3 ]

    [<Fact>]
    let ``minItems=maxItems=3: ToTuple rebuilds the flat 3-tuple`` () =
        expectEqual (IntArrayExact3.ToTuple((1, (2, 3)))) (1, 2, 3)

    [<Fact>]
    let ``minItems=maxItems=3: Parse then ToTuple gives the elements in JSON order`` () =
        let v = expectOk (IntArrayExact3.Parse("""[7, 8, 9]"""))
        expectEqual (IntArrayExact3.ToTuple(v)) (7, 8, 9)

    [<Property>]
    let ``minItems=maxItems=3: Parse accepts exactly the arrays with three items`` (items: list<int>) =
        let json = System.Text.Json.JsonSerializer.Serialize items
        let n = List.length items
        (match IntArrayExact3.Parse json with
         | Ok res ->
            let check ((v1, (v2, v3)) as res) =
                n = 3 && IntArrayExact3.ToList res = items && items = [ v1; v2; v3 ]
            check res && check (IntArrayExact3.Create res)
         | Error _ -> n <> 3)
        |> Prop.classify (n = 3) "exactly 3 items"
        |> Prop.classify (n <> 3) "not 3 items"

    [<Literal>]
    let intArrayExact4Schema = """{ "type": "array", "items": {"type": "integer"}, "minItems": 4, "maxItems": 4 }"""
    type IntArrayExact4 = RefinedJsonSchemaProvider<schema = intArrayExact4Schema>

    [<Fact>]
    let ``minItems=maxItems=4: ToTuple rebuilds the flat 4-tuple`` () =
        expectEqual (IntArrayExact4.ToTuple((1, (2, (3, 4))))) (1, 2, 3, 4)

    [<Property>]
    let ``minItems=maxItems=4: Parse accepts exactly the arrays with four items`` (items: list<int>) =
        let json = System.Text.Json.JsonSerializer.Serialize items
        let n = List.length items
        (match IntArrayExact4.Parse json with
         | Ok res ->
            let check ((v1, (v2, (v3, v4))) as res) =
                n = 4 && IntArrayExact4.ToList res = items && items = [ v1; v2; v3; v4 ]
            check res && check (IntArrayExact4.Create res)
         | Error _ -> n <> 4)
        |> Prop.classify (n = 4) "exactly 4 items"
        |> Prop.classify (n <> 4) "not 4 items"

    [<Literal>]
    let arrayOfExact3Schema =
        """{ "type": "array", "items": { "type": "array", "items": {"type": "integer"}, "minItems": 3, "maxItems": 3 } }"""
    type ArrayOfExact3 = RefinedJsonSchemaProvider<schema = arrayOfExact3Schema>

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
    type ArrayOfMax2OfExact2 = RefinedJsonSchemaProvider<schema = arrayOfMax2OfExact2Schema>

    [<Fact>]
    let ``three array levels: ItemArray.ToList and ItemArray.ItemArray.ToTuple each flatten their own level`` () =
        let middle = ArrayOfMax2OfExact2.ItemArray.ToList(Some((1, 2), Some(3, 4)))
        expectEqual (middle |> List.map ArrayOfMax2OfExact2.ItemArray.ItemArray.ToTuple) [ (1, 2); (3, 4) ]

    [<Literal>]
    let min1ObjectArraySchema =
        """{ "type": "array", "minItems": 1, "items": { "type": "object", "properties": { "a": {"type": "integer"} }, "required": ["a"] } }"""
    type Min1ObjectArray = RefinedJsonSchemaProvider<schema = min1ObjectArraySchema>

    [<Fact>]
    let ``minItems=1 array of objects: ToList flattens (h, tail list) of provided-type items`` () =
        let item = Min1ObjectArray.Item.Create(a = 1)
        expectEqual (Min1ObjectArray.ToList((item, [])) |> List.map (fun i -> i.a)) [ 1 ]

    [<Literal>]
    let arrayOfMin1ObjectArraysSchema =
        """{ "type": "array", "items": { "type": "array", "minItems": 1, "items": { "type": "object", "properties": { "a": {"type": "integer"} }, "required": ["a"] } } }"""
    type ArrayOfMin1ObjectArrays = RefinedJsonSchemaProvider<schema = arrayOfMin1ObjectArraysSchema>

    [<Fact>]
    let ``array of arrays of objects: the Item class and the ItemArray helper don't clash`` () =
        let item = ArrayOfMin1ObjectArrays.Item.Create(a = 1)
        let flattened = ArrayOfMin1ObjectArrays.ItemArray.ToList((item, []))
        expectEqual (flattened |> List.map (fun i -> i.a)) [ 1 ]

    // -- uniqueItems: plain list, ToJson drops duplicates --

    [<Literal>]
    let uniqueItemsSchema = """{ "type": "array", "items": { "type": "integer" }, "uniqueItems": true }"""
    type UniqueItemsArray = RefinedJsonSchemaProvider<schema=uniqueItemsSchema>

    [<Fact>]
    let ``uniqueItems Create takes a plain list and needs no Result`` () =
        let value: int list = UniqueItemsArray.Create([ 3; 1; 2 ])
        expectEqual value [ 3; 1; 2 ]

    [<Fact>]
    let ``uniqueItems Create drops duplicates, keeping each first occurrence`` () =
        expectEqual (UniqueItemsArray.Create([ 3; 1; 3; 2; 1 ])) [ 3; 1; 2 ]

    [<Fact>]
    let ``uniqueItems Parse keeps the order`` () =
        expectEqual (expectOk (UniqueItemsArray.Parse("""[3, 1, 2]"""))) [ 3; 1; 2 ]

    [<Fact>]
    let ``uniqueItems Parse rejects JSON with a duplicate`` () =
        expectError (UniqueItemsArray.Parse("""[1, 2, 2]"""))

    [<Property>]
    let ``uniqueItems: Parse accepts exactly the arrays without duplicates`` (items: list<int>) =
        let json = System.Text.Json.JsonSerializer.Serialize items
        let distinct = List.distinct items = items
        let holds =
            (match UniqueItemsArray.Parse json with
             | Ok res -> distinct && res = items
             | Error _ -> not distinct)
            && UniqueItemsArray.Create items = List.distinct items
        holds
        |> Prop.classify distinct "no duplicates"
        |> Prop.classify (not distinct) "duplicates"

    [<Literal>]
    let uniqueItemsInTupleSchema =
        """{ "type": "array", "items": { "type": "array", "items": { "type": "integer" }, "uniqueItems": true }, "minItems": 2, "maxItems": 2 }"""
    type UniqueItemsInTuple = RefinedJsonSchemaProvider<schema=uniqueItemsInTupleSchema>

    [<Fact>]
    let ``uniqueItems arrays inside a size-bounded tuple drop their duplicates`` () =
        expectEqual (UniqueItemsInTuple.Create(([ 1; 1; 2 ], [ 3; 3 ]))) ([ 1; 2 ], [ 3 ])
        expectEqual (expectOk (UniqueItemsInTuple.Parse("""[[2, 1], [3]]"""))) ([ 2; 1 ], [ 3 ])
        expectError (UniqueItemsInTuple.Parse("""[[1, 1], [3]]"""))

    [<Literal>]
    let uniqueItemsOneOfBranchSchema =
        """{ "oneOf": [ { "type": "array", "items": { "type": "integer" }, "uniqueItems": true }, { "type": "string" } ] }"""
    type UniqueItemsOneOfBranch = RefinedJsonSchemaProvider<schema=uniqueItemsOneOfBranchSchema>

    [<Fact>]
    let ``uniqueItems array as a oneOf branch drops duplicates before validation`` () =
        expectEqual (expectOk (UniqueItemsOneOfBranch.Create(Choice1Of2 [ 2; 1; 2 ]))) (Choice1Of2 [ 2; 1 ])
        expectEqual (expectOk (UniqueItemsOneOfBranch.Parse("""[2, 1]"""))) (Choice1Of2 [ 2; 1 ])
        expectError (UniqueItemsOneOfBranch.Parse("""[1, 1]"""))

    [<Literal>]
    let uniqueStringsSchema = """{ "type": "array", "items": { "type": "string" }, "uniqueItems": true }"""
    type UniqueStringsArray = RefinedJsonSchemaProvider<schema=uniqueStringsSchema>

    [<Fact>]
    let ``uniqueItems on strings: Create validates, and the duplicate is dropped before that`` () =
        expectEqual (expectOk (UniqueStringsArray.Create([ "b"; "a"; "b" ]))) [ "b"; "a" ]

    [<Property>]
    let ``uniqueItems on strings: Parse accepts exactly the arrays without duplicates`` (items: list<string>) =
        let json = System.Text.Json.JsonSerializer.Serialize items
        let distinct = List.distinct items = items
        let holds =
            (match UniqueStringsArray.Parse json with
             | Ok res -> distinct && res = items
             | Error _ -> not distinct)
            && (UniqueStringsArray.Create items |> expectOk) = List.distinct items
        holds
        |> Prop.classify distinct "no duplicates"
        |> Prop.classify (not distinct) "duplicates"
        |> Prop.classify (List.contains null items) "contains null"

    [<Literal>]
    let nestedUniqueItemsSchema =
        """{ "type": "array", "items": { "type": "array", "items": { "type": "integer" }, "uniqueItems": true }, "uniqueItems": true }"""
    type NestedUniqueItemsArray = RefinedJsonSchemaProvider<schema=nestedUniqueItemsSchema>

    [<Fact>]
    let ``nested uniqueItems: inner lists in a different order are different elements`` () =
        expectEqual (expectOk (NestedUniqueItemsArray.Parse("""[[1, 2], [2, 1]]"""))) [ [ 1; 2 ]; [ 2; 1 ] ]
        expectError (NestedUniqueItemsArray.Parse("""[[1, 2], [1, 2]]"""))

    [<Literal>]
    let uniqueNumbersSchema = """{ "type": "array", "items": { "type": "number" }, "uniqueItems": true }"""
    type UniqueNumbersArray = RefinedJsonSchemaProvider<schema=uniqueNumbersSchema>

    [<Fact>]
    let ``uniqueItems on numbers: 1 and 1.0 are the same JSON number`` () =
        expectError (UniqueNumbersArray.Parse("""[1, 1.0]"""))

    [<Literal>]
    let uniqueMin2Schema = """{ "type": "array", "items": { "type": "integer" }, "minItems": 2, "uniqueItems": true }"""
    type UniqueMin2Array = RefinedJsonSchemaProvider<schema=uniqueMin2Schema>

    [<Fact>]
    let ``uniqueItems with minItems is a plain list checked at runtime`` () =
        expectEqual (expectOk (UniqueMin2Array.Create([ 2; 1 ]))) [ 2; 1 ] // order is kept
        expectError (UniqueMin2Array.Create([ 1 ]))
        expectError (UniqueMin2Array.Create([ 1; 1 ]))
        expectError (UniqueMin2Array.Parse("""[1, 2, 2]"""))

    [<Property>]
    let ``uniqueItems, minItems=2: Parse accepts exactly the arrays of at least two distinct items`` (items: list<int>) =
        let json = System.Text.Json.JsonSerializer.Serialize items
        let distinct = List.distinct items = items
        let valid = distinct && List.length items >= 2
        // with minItems the duplicates are not dropped (that could make the array too short),
        // so Create validates like Parse does
        let holds =
            (match UniqueMin2Array.Parse json with
             | Ok res -> valid && res = items
             | Error _ -> not valid)
            && (match UniqueMin2Array.Create items with
                | Ok res -> valid && res = items
                | Error _ -> not valid)
        holds
        |> Prop.classify valid "at least 2 distinct items"
        |> Prop.classify (not valid) "duplicates or fewer than 2 items"

    [<Literal>]
    let uniqueMax2Schema = """{ "type": "array", "items": { "type": "integer" }, "maxItems": 2, "uniqueItems": true }"""
    type UniqueMax2Array = RefinedJsonSchemaProvider<schema=uniqueMax2Schema>

    [<Fact>]
    let ``uniqueItems with maxItems keeps the option chain and Create needs no Result`` () =
        expectEqual (UniqueMax2Array.Create(Some(1, Some 2))) (Some(1, Some 2))
        expectEqual (UniqueMax2Array.Create(None)) None
        expectEqual (expectOk (UniqueMax2Array.Parse("""[2, 1]"""))) (Some(2, Some 1))
        expectError (UniqueMax2Array.Parse("""[1, 1]"""))

    [<Property>]
    let ``uniqueItems, maxItems=2: Parse accepts exactly the arrays of at most two distinct items`` (items: list<int>) =
        let json = System.Text.Json.JsonSerializer.Serialize items
        let valid = List.distinct items = items && List.length items <= 2
        (match UniqueMax2Array.Parse json with
         | Ok res ->
            let check res = valid && UniqueMax2Array.ToList res = items
            check res && check (UniqueMax2Array.Create res)
         | Error _ -> not valid)
        |> Prop.classify valid "at most 2 distinct items"
        |> Prop.classify (not valid) "duplicates or more than 2 items"

    [<Literal>]
    let uniqueObjectItemsSchema =
        """{ "type": "array", "items": { "type": "object", "properties": { "a": { "type": "integer" } }, "required": ["a"] }, "uniqueItems": true }"""
    type UniqueObjectItemsArray = RefinedJsonSchemaProvider<schema=uniqueObjectItemsSchema>

    [<Fact>]
    let ``uniqueItems on object items: Create drops duplicates, Parse rejects them`` () =
        let item a = UniqueObjectItemsArray.Item.Create(a = a)
        let ok = expectOk (UniqueObjectItemsArray.Create([ item 1; item 2; item 1 ]))
        expectEqual (ok |> List.map (fun i -> i.a)) [ 1; 2 ]
        expectError (UniqueObjectItemsArray.Parse("""[{"a": 1}, {"a": 1}]"""))

    [<Literal>]
    let uniqueTwoPropertyItemsSchema =
        """{ "type": "array", "items": { "type": "object", "properties": { "a": { "type": "integer" }, "b": { "type": "integer" } }, "required": ["a", "b"] }, "uniqueItems": true }"""
    type UniqueTwoPropertyItemsArray = RefinedJsonSchemaProvider<schema=uniqueTwoPropertyItemsSchema>

    [<Fact>]
    let ``uniqueItems on object items ignores property order`` () =
        let parsed = expectOk (UniqueTwoPropertyItemsArray.Parse("""[{"b": 2, "a": 1}]"""))
        let built = UniqueTwoPropertyItemsArray.Item.Create(a = 1, b = 2)
        let ok = expectOk (UniqueTwoPropertyItemsArray.Create(parsed @ [ built ]))
        expectEqual (ok |> List.map (fun i -> i.a, i.b)) [ 1, 2 ]

    // -- maxItems: boundary is always structurally enforced, so no rejection test exists here --

    [<Literal>]
    let maxItemsSchema = """{ "type": "array", "items": { "type": "integer" }, "maxItems": 2 }"""
    type MaxItemsArray = RefinedJsonSchemaProvider<schema=maxItemsSchema>

    [<Fact>]
    let ``maxItems accepts an array at the limit`` () =
        expectEqual (MaxItemsArray.Create(Some(1, Some 2))) (Some(1, Some 2))

    [<Fact>]
    let ``maxItems accepts an array within the limit`` () =
        expectEqual (MaxItemsArray.Create(Some(1, None))) (Some(1, None))

    // -- minItems with refinedTypes on still compiles to a tuple (see above); this is

    //    the plain-list case, where minItems is only checked by Create at runtime --

    [<Literal>]
    let minItemsRuntimeSchema = """{ "type": "array", "items": { "type": "string" }, "minItems": 2 }"""
    type MinItemsRuntime = RefinedJsonSchemaProvider<schema=minItemsRuntimeSchema, refinedTypes = false>

    [<Fact>]
    let ``minItems is enforced at runtime by Create, not just as a compile-time tuple shape`` () =
        expectError (MinItemsRuntime.Create([ "a" ])) // 1 element is below minItems=2

    [<Fact>]
    let ``minItems is satisfied at runtime when there are enough elements`` () =
        let result = expectOk (MinItemsRuntime.Create([ "a"; "b" ]))
        expectEqual result [ "a"; "b" ]

    // -- a constraint on the item schema itself, independent of the array's own keywords --

    [<Literal>]
    let arrayItemConstraintSchema = """{ "type": "array", "items": { "type": "integer", "minimum": 1, "exclusiveMinimum": -2147483649 } }"""
    type ArrayItemConstraint = RefinedJsonSchemaProvider<schema=arrayItemConstraintSchema>

    [<Fact>]
    let ``an item schema's own constraint is enforced against every element`` () =
        expectError (ArrayItemConstraint.Create([ 1; 0; 3 ])) // 0 violates the item schema's minimum

    [<Fact>]
    let ``an array whose every element satisfies the item schema is accepted`` () =
        let result = expectOk (ArrayItemConstraint.Create([ 1; 2; 3 ]))
        expectEqual result [ 1; 2; 3 ]

    [<Property>]
    let ``item minimum 1: Parse accepts exactly the arrays where every item is at least 1`` (items: list<int>) =
        let json = System.Text.Json.JsonSerializer.Serialize items
        let valid = List.forall (fun x -> x >= 1) items
        (match ArrayItemConstraint.Parse json with
         | Ok res -> valid && res = items && (ArrayItemConstraint.Create res |> expectOk) = items
         | Error _ -> not valid)
        |> Prop.classify valid "every item at least 1"
        |> Prop.classify (not valid) "some item below 1"

    // -- general array-property access: not a keyword test, just item-shape/indexing coverage --

    [<Literal>]
    let numberArray =
        """{ "type": "object", "properties": { "values": { "type": "array", "items": {"type": "number"} } }, "required": ["values"] }"""
    type NumberArray = RefinedJsonSchemaProvider<schema=numberArray>
    type UncheckedNumberArray = RefinedJsonSchemaProvider<schema=numberArray, allowNonFiniteNumbers=true>

    [<Fact>]
    let ``select from number array should yield input value`` () =
        let numArray = UncheckedNumberArray.Create([ 11.0; 12.0; 11.6; 12.1 ])
        expectEqual numArray.values[1] 12.0

    [<Fact>]
    let ``select from number array should yield input value when Create gives Ok by default`` () =
        let numArray = expectOk (NumberArray.Create([ 11.0; 12.0; 11.6; 12.1 ]))
        expectEqual numArray.values[1] 12.0

    [<Literal>]
    let integerArray =
        """{ "type": "object", "properties": { "values": { "type": "array", "items": {"type": "integer"} } }, "required": ["values"] }"""
    type IntegerArray = RefinedJsonSchemaProvider<schema=integerArray>

    [<Fact>]
    let ``select from integer array should yield input value`` () =
        let numArray = IntegerArray.Create([ 11; 12; 10; 13 ])
        expectEqual numArray.values[1] 12

    [<Literal>]
    let nestedArray =
        """{ "type": "object", "properties": { "values": { "type": "array", "items": { "type": "array", "items": {"type": "string"} } } }, "required": ["values"] }"""
    type NestedArray = RefinedJsonSchemaProvider<schema=nestedArray>

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
    type NestedArrayWithObjectItems = RefinedJsonSchemaProvider<schema=nestedArrayWithObjectItems>

    [<Fact>]
    let ``select from nested array with object items should yield input value`` () =
        let item = expectOk (NestedArrayWithObjectItems.valuesItem.Create(propA = 5))
        let array = expectOk (NestedArrayWithObjectItems.Create([ [ item ] ]))
        expectEqual (array.values.Value[0][0]).propA (Some(5))
