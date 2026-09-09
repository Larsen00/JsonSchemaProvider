namespace JsonSchemaProvider.Tests

// Covers the "any type as root" work in notes/any-type-as-root-refactor.md: a schema whose
// root is a primitive, not just an object. There's no getter property for a primitive root yet
// (see that note's open item #2), so these tests read the constructed value back out via the
// raw NullableJsonValue.JsonVal instead of a nice typed accessor.
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

    // FSharpString carries no SpecificKeywords at all (unlike FSharpInt) - this schema exists to
    // confirm primitive-root validation genuinely runs the whole sub-schema through
    // NJsonSchema's own Validate, not something limited to the keywords SchemaConversion.fs
    // happens to model at the FSharpType level.
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
    type ListRootMinItems2Compiled = JsonSchemaProvider<schema=listRootMinItems2Schema, compileMinItems=true>

    type PlaceListRoot = JsonSchemaProvider<schema=placeListRootSchema>

    let boolRootShouldBeCreated =
        test "boolean root Create builds the value" {
            let result = BoolRoot.Create(true)
            Expect.equal (result) true "BoolRoot.Create(true).JsonVal = true"
        }

    let boolRootShouldBeParsed =
        test "boolean root Parse builds the value" {
            let result = BoolRoot.Parse("true")
            Expect.equal (result.JsonVal.AsBoolean()) true "BoolRoot.Parse(\"true\").JsonVal = true"
        }

    let intRootShouldBeCreated =
        test "integer root Create builds the value" {
            let result = IntRoot.Create(42)
            Expect.equal result 42 "IntRoot.Create(42).JsonVal = 42"
        }

    let intRootShouldBeParsed =
        test "integer root Parse builds the value" {
            let result = IntRoot.Parse("42")
            Expect.equal (result.JsonVal.AsInteger()) 42 "IntRoot.Parse(\"42\").JsonVal = 42"
        }

    let numberRootShouldBeCreated =
        test "number root Create builds the value" {
            let result = NumberRoot.Create(3.14)
            Expect.equal result 3.14 "NumberRoot.Create(3.14).JsonVal = 3.14"
        }

    let numberRootShouldBeParsed =
        test "number root Parse builds the value" {
            let result = NumberRoot.Parse("3.14")
            Expect.equal (result.JsonVal.AsFloat()) 3.14 "NumberRoot.Parse(\"3.14\").JsonVal = 3.14"
        }

    let stringRootShouldBeCreated =
        test "string root Create builds the value" {
            let result = StringRoot.Create("hello")
            Expect.equal result "hello" "StringRoot.Create(\"hello\").JsonVal = \"hello\""
        }

    let stringRootShouldBeParsed =
        test "string root Parse builds the value" {
            let result = StringRoot.Parse("\"hello\"")
            Expect.equal (result.JsonVal.AsString()) "hello" "StringRoot.Parse of a JSON string literal = \"hello\""
        }

    let inRangeConstrainedIntRootShouldBeAccepted =
        test "in-range integer root value is accepted by Create" {
            let result = ConstrainedIntRoot.Create(7)
            Expect.equal result 7 "ConstrainedIntRoot.Create(7).JsonVal = 7"
        }

    let belowMinimumConstrainedIntRootShouldBeRejectedByCreate =
        test "below-minimum integer root value is rejected by Create" {
            Expect.throws
                (fun () -> ConstrainedIntRoot.Create(3) |> ignore)
                "Create should reject a root value below minimum"
        }

    let aboveMaximumConstrainedIntRootShouldBeRejectedByCreate =
        test "above-maximum integer root value is rejected by Create" {
            Expect.throws
                (fun () -> ConstrainedIntRoot.Create(20) |> ignore)
                "Create should reject a root value above maximum"
        }

    let belowMinimumConstrainedIntRootShouldBeRejectedByParse =
        test "below-minimum integer root value is rejected by Parse" {
            Expect.throws
                (fun () -> ConstrainedIntRoot.Parse("3") |> ignore)
                "Parse should reject a root value below minimum"
        }

    let matchingPatternStringRootShouldBeAccepted =
        test "string root value matching pattern is accepted by Create" {
            let result = PatternConstrainedStringRoot.Create("hello")
            Expect.equal result "hello" "PatternConstrainedStringRoot.Create(\"hello\") should be accepted"
        }

    let nonMatchingPatternStringRootShouldBeRejected =
        test "string root value not matching pattern is rejected by Create" {
            Expect.throws
                (fun () -> PatternConstrainedStringRoot.Create("HELLO") |> ignore)
                "Create should reject a root value that doesn't match the pattern"
        }

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
    let nonEmptyPlaceListRootShouldBeCreated =
        test "array-of-objects root Create builds a non-empty list" {
            let place = PlaceListRoot.Item.Create(name = "Copenhagen", lat = 55.6761, lng = 12.5683)
            let result = PlaceListRoot.Create([ place ])
            Expect.equal (List.length result) 1 "one element"
            Expect.equal result.[0].name "Copenhagen" "name roundtrips"
            Expect.equal result.[0].lat 55.6761 "lat roundtrips"
            Expect.equal result.[0].lng 12.5683 "lng roundtrips"
        }

    // FSharpOneOf as the schema root isn't wired up yet (run still `failwith`s for it - see
    // notes/any-type-as-root-refactor.md open item #3). Deliberately no
    // `type X = JsonSchemaProvider<schema=...>` for that here: a design-time failwith aborts
    // compiling this whole file, not just one test, so there's no way to assert that gap from
    // inside Expecto today. Add a case here once oneOf root is implemented.

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
              nonEmptyPlaceListRootShouldBeCreated ]
