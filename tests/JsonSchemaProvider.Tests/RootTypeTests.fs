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

    type BoolRoot = JsonSchemaProvider<schema=boolRootSchema>
    type IntRoot = JsonSchemaProvider<schema=intRootSchema>
    type NumberRoot = JsonSchemaProvider<schema=numberRootSchema>
    type StringRoot = JsonSchemaProvider<schema=stringRootSchema>
    type ConstrainedIntRoot = JsonSchemaProvider<schema=constrainedIntRootSchema>

    let boolRootShouldBeCreated =
        test "boolean root Create builds the value" {
            let result = BoolRoot.Create(true)
            Expect.equal (result.JsonVal.AsBoolean()) true "BoolRoot.Create(true).JsonVal = true"
        }

    let boolRootShouldBeParsed =
        test "boolean root Parse builds the value" {
            let result = BoolRoot.Parse("true")
            Expect.equal (result.JsonVal.AsBoolean()) true "BoolRoot.Parse(\"true\").JsonVal = true"
        }

    let intRootShouldBeCreated =
        test "integer root Create builds the value" {
            let result = IntRoot.Create(42)
            Expect.equal (result.JsonVal.AsInteger()) 42 "IntRoot.Create(42).JsonVal = 42"
        }

    let intRootShouldBeParsed =
        test "integer root Parse builds the value" {
            let result = IntRoot.Parse("42")
            Expect.equal (result.JsonVal.AsInteger()) 42 "IntRoot.Parse(\"42\").JsonVal = 42"
        }

    let numberRootShouldBeCreated =
        test "number root Create builds the value" {
            let result = NumberRoot.Create(3.14)
            Expect.equal (result.JsonVal.AsFloat()) 3.14 "NumberRoot.Create(3.14).JsonVal = 3.14"
        }

    let numberRootShouldBeParsed =
        test "number root Parse builds the value" {
            let result = NumberRoot.Parse("3.14")
            Expect.equal (result.JsonVal.AsFloat()) 3.14 "NumberRoot.Parse(\"3.14\").JsonVal = 3.14"
        }

    let stringRootShouldBeCreated =
        test "string root Create builds the value" {
            let result = StringRoot.Create("hello")
            Expect.equal (result.JsonVal.AsString()) "hello" "StringRoot.Create(\"hello\").JsonVal = \"hello\""
        }

    let stringRootShouldBeParsed =
        test "string root Parse builds the value" {
            let result = StringRoot.Parse("\"hello\"")
            Expect.equal (result.JsonVal.AsString()) "hello" "StringRoot.Parse of a JSON string literal = \"hello\""
        }

    let inRangeConstrainedIntRootShouldBeAccepted =
        test "in-range integer root value is accepted by Create" {
            let result = ConstrainedIntRoot.Create(7)
            Expect.equal (result.JsonVal.AsInteger()) 7 "ConstrainedIntRoot.Create(7).JsonVal = 7"
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

    // FSharpList/FSharpOneOf as the schema root aren't wired up yet (run still `failwith`s for
    // them - see notes/any-type-as-root-refactor.md open item #3). Deliberately no
    // `type X = JsonSchemaProvider<schema=...>` for those here: a design-time failwith aborts
    // compiling this whole file, not just one test, so there's no way to assert that gap from
    // inside Expecto today. Add cases here once list/oneOf root is implemented.

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
              belowMinimumConstrainedIntRootShouldBeRejectedByParse ]
