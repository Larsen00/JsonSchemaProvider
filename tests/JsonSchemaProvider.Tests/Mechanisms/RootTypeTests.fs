namespace JsonSchemaProvider.Tests

// A schema whose root is a primitive, list, or oneOf, not just an object. Create and Parse both
// work with the plain typed value (bool, int, string list, Choice<...>); Parse always wraps it
// in Result.
module RootTypeTests =
    open Xunit
    open JsonSchemaProvider

    [<Literal>]
    let boolRootSchema = """{ "type": "boolean" }"""
    type BoolRoot = JsonSchemaProvider<schema=boolRootSchema>

    [<Fact>]
    let ``boolean root Create builds the value`` () =
        expectEqual (BoolRoot.Create true) true

    [<Fact>]
    let ``boolean root Parse builds the value`` () =
        let result = expectOk (BoolRoot.Parse("true"))
        expectEqual result true

    [<Literal>]
    let intRootSchema = """{ "type": "integer" }"""
    type IntRoot = JsonSchemaProvider<schema=intRootSchema>

    [<Fact>]
    let ``integer root Create builds the value`` () =
        expectEqual (IntRoot.Create 42) 42

    [<Fact>]
    let ``integer root Parse builds the value`` () =
        let result = expectOk (IntRoot.Parse("42"))
        expectEqual result 42

    [<Literal>]
    let numberRootSchema = """{ "type": "number" }"""
    type NumberRoot = JsonSchemaProvider<schema=numberRootSchema>

    [<Fact>]
    let ``number root Create builds the value`` () =
        expectEqual (NumberRoot.Create 3.14) 3.14

    [<Fact>]
    let ``number root Parse builds the value`` () =
        let result = expectOk (NumberRoot.Parse("3.14"))
        expectEqual result 3.14

    [<Literal>]
    let stringRootSchema = """{ "type": "string" }"""
    type StringRoot = JsonSchemaProvider<schema=stringRootSchema>

    [<Fact>]
    let ``string root Create builds the value`` () =
        expectEqual (StringRoot.Create "hello") "hello"

    [<Fact>]
    let ``string root Parse builds the value`` () =
        let result = expectOk (StringRoot.Parse("\"hello\""))
        expectEqual result "hello"

    // minimum/maximum don't compile into the type (int stays int), so this root is never
    // FullyCompilable and Create returns Result, unlike the unconstrained roots above.
    [<Literal>]
    let constrainedIntRootSchema = """{ "type": "integer", "minimum": 5, "maximum": 10 }"""
    type ConstrainedIntRoot = JsonSchemaProvider<schema=constrainedIntRootSchema>

    [<Fact>]
    let ``in-range integer root value is accepted by Create`` () =
        let value = expectOk (ConstrainedIntRoot.Create(7))
        expectEqual value 7

    [<Fact>]
    let ``below-minimum integer root value is rejected by Create`` () =
        expectError (ConstrainedIntRoot.Create(3)) // below minimum

    [<Fact>]
    let ``above-maximum integer root value is rejected by Create`` () =
        expectError (ConstrainedIntRoot.Create(20)) // above maximum

    [<Fact>]
    let ``below-minimum integer root value is rejected by Parse`` () =
        expectError (ConstrainedIntRoot.Parse("3")) // below minimum

    // Confirms primitive-root validation runs the whole sub-schema through NJsonSchema's own
    // Validate, not just whatever SchemaConversion.fs's JsonSchemaType model happens to act on.
    [<Literal>]
    let patternConstrainedStringRootSchema = """{ "type": "string", "pattern": "^[a-z]+$" }"""
    type PatternConstrainedStringRoot = JsonSchemaProvider<schema=patternConstrainedStringRootSchema>

    [<Fact>]
    let ``string root value matching pattern is accepted by Create`` () =
        let value = expectOk (PatternConstrainedStringRoot.Create("hello"))
        expectEqual value "hello"

    [<Fact>]
    let ``string root value not matching pattern is rejected by Create`` () =
        expectError (PatternConstrainedStringRoot.Create("HELLO")) // doesn't match the pattern

    [<Literal>]
    let listRootSchema = """{ "type": "array", "items": { "type": "string" } }"""
    type ListRoot = JsonSchemaProvider<schema=listRootSchema>

    // A minItems/maxItems-constrained list root is covered in Keywords/ArrayKeywordTests.fs, not
    // here - this file is about root support existing at all.
    [<Fact>]
    let ``array root Parse builds the value`` () =
        let result = expectOk (ListRoot.Parse("""["a", "b"]"""))
        expectEqual result [ "a"; "b" ]

    [<Fact>]
    let ``array root Parse rejects an item of the wrong type`` () =
        expectError (ListRoot.Parse("""["a", 1]""")) // non-string item

    [<Fact>]
    let ``Parse of syntactically malformed JSON evaluates to Error instead of raising`` () =
        expectError (ListRoot.Parse("""["a", """)) // malformed JSON should be an Error

    // A list of objects, not just of primitives - the item class is exposed as PlaceListRoot.Item
    // ("Item" suffix, vs "Obj" for a nested object property; no property name to prefix at the root).
    [<Literal>]
    let placeListRootSchema =
        """
        {
          "type": "array",
          "items": {
            "type": "object",
            "properties": { "name": { "type": "string" }, "lat": { "type": "number" }, "lng": { "type": "number" } },
            "required": ["name", "lat", "lng"]
          }
        }"""
    type PlaceListRoot = JsonSchemaProvider<schema=placeListRootSchema>

    [<Fact>]
    let ``array-of-objects root Create builds an empty list`` () =
        expectEqual (List.length (PlaceListRoot.Create([]))) 0

    [<Fact>]
    let ``array-of-objects root Create builds a non-empty list`` () =
        let place = PlaceListRoot.Item.Create(name = "Copenhagen", lat = 55.6761, lng = 12.5683)
        let result = PlaceListRoot.Create([ place ])
        expectEqual (List.length result) 1
        expectEqual result.[0].name "Copenhagen"
        expectEqual result.[0].lat 55.6761
        expectEqual result.[0].lng 12.5683

    [<Fact>]
    let ``array-of-objects root Parse builds the value`` () =
        let result = expectOk (PlaceListRoot.Parse("""[{"name": "Copenhagen", "lat": 55.6761, "lng": 12.5683}]"""))
        expectEqual result.[0].name "Copenhagen"

    // Both unconstrained, so this oneOf root is FullyCompilable - Create returns the bare
    // Choice<string, int> directly, not Result.
    [<Literal>]
    let stringOrIntRootSchema = """{ "oneOf": [{ "type": "string" }, { "type": "integer" }] }"""
    type StringOrIntRoot = JsonSchemaProvider<schema=stringOrIntRootSchema>

    [<Fact>]
    let ``oneOf root Create builds the value from the string branch`` () =
        expectEqual (StringOrIntRoot.Create(value = Choice1Of2 "hello")) (Choice1Of2 "hello")

    [<Fact>]
    let ``oneOf root Create builds the value from the int branch`` () =
        expectEqual (StringOrIntRoot.Create(value = Choice2Of2 42)) (Choice2Of2 42)

    [<Fact>]
    let ``oneOf root Parse builds the value`` () =
        let result = expectOk (StringOrIntRoot.Parse("42"))
        expectEqual result (Choice2Of2 42)

    [<Literal>]
    let stringOrIntOrBoolRootSchema = """{ "oneOf": [{ "type": "string" }, { "type": "integer" }, { "type": "boolean" }] }"""
    type StringOrIntOrBoolRoot = JsonSchemaProvider<schema=stringOrIntOrBoolRootSchema>

    [<Fact>]
    let ``3-way oneOf root Create builds the value from the string branch`` () =
        expectEqual (StringOrIntOrBoolRoot.Create(value = Choice1Of2 "x")) (Choice1Of2 "x")

    [<Fact>]
    let ``3-way oneOf root Create builds the value from the int branch`` () =
        expectEqual (StringOrIntOrBoolRoot.Create(value = Choice2Of2(Choice1Of2 7))) (Choice2Of2(Choice1Of2 7))

    [<Fact>]
    let ``3-way oneOf root Create builds the value from the bool branch`` () =
        expectEqual (StringOrIntOrBoolRoot.Create(value = Choice2Of2(Choice2Of2 true))) (Choice2Of2(Choice2Of2 true))

    // One branch is an object - the generated class is named "Case1" (empty root name + its
    // branch's "Case<i>" suffix) and, being an unconstrained string property, is FullyCompilable.
    [<Literal>]
    let placeOrIdRootSchema =
        """{ "oneOf": [{ "type": "object", "properties": { "name": { "type": "string" } }, "required": ["name"] }, { "type": "integer" }] }"""
    type PlaceOrIdRoot = JsonSchemaProvider<schema=placeOrIdRootSchema>

    [<Fact>]
    let ``oneOf root with an object branch Create builds the value from the object branch`` () =
        let place = PlaceOrIdRoot.Case1.Create(name = "Copenhagen")
        match PlaceOrIdRoot.Create(value = Choice1Of2 place) with
        | Choice1Of2 case -> expectEqual case.name "Copenhagen"
        | Choice2Of2 _ -> failwith "expected the object branch (Choice1Of2)"

    [<Fact>]
    let ``oneOf root with an object branch Create builds the value from the int branch`` () =
        expectEqual (PlaceOrIdRoot.Create(value = Choice2Of2 5)) (Choice2Of2 5)

    [<Literal>]
    let stringOrIntArrayRootSchema = """{ "oneOf": [{ "type": "string" }, { "type": "array", "items": { "type": "integer" } }] }"""
    type StringOrIntArrayRoot = JsonSchemaProvider<schema=stringOrIntArrayRootSchema>

    [<Fact>]
    let ``oneOf root with an array branch Create builds the value from the string branch`` () =
        expectEqual (StringOrIntArrayRoot.Create(value = Choice1Of2 "hi")) (Choice1Of2 "hi")

    [<Fact>]
    let ``oneOf root with an array branch Create builds the value from the array branch`` () =
        expectEqual (StringOrIntArrayRoot.Create(value = Choice2Of2 [ 1; 2; 3 ])) (Choice2Of2 [ 1; 2; 3 ])

    // minimum:5 makes the int branch (and so the oneOf node itself) not FullyCompilable - Create
    // returns Result here, unlike the schemas above.
    [<Literal>]
    let constrainedIntOrStringRootSchema = """{ "oneOf": [{ "type": "integer", "minimum": 5 }, { "type": "string" }] }"""
    type ConstrainedIntOrStringRoot = JsonSchemaProvider<schema=constrainedIntOrStringRootSchema>

    [<Fact>]
    let ``constrained oneOf root Create accepts an in-range int branch value`` () =
        let value = expectOk (ConstrainedIntOrStringRoot.Create(value = Choice1Of2 7))
        expectEqual value (Choice1Of2 7)

    [<Fact>]
    let ``constrained oneOf root Create accepts the string branch`` () =
        let value = expectOk (ConstrainedIntOrStringRoot.Create(value = Choice2Of2 "hello"))
        expectEqual value (Choice2Of2 "hello")

    [<Fact>]
    let ``constrained oneOf root Create rejects a below-minimum int branch value`` () =
        expectError (ConstrainedIntOrStringRoot.Create(value = Choice1Of2 3)) // 3 satisfies neither branch

    [<Fact>]
    let ``oneOf root Parse rejects a value matching no branch`` () =
        expectError (ConstrainedIntOrStringRoot.Parse("3")) // matches no oneOf branch

    // string | (int | bool) - same shape as OneOfTests.fs's nestedOneOfSchema but at the root.
    [<Literal>]
    let nestedOneOfRootSchema = """{ "oneOf": [{ "type": "string" }, { "oneOf": [{ "type": "integer" }, { "type": "boolean" }] }] }"""
    type NestedOneOfRoot = JsonSchemaProvider<schema=nestedOneOfRootSchema>

    [<Fact>]
    let ``nested oneOf root Create builds the value from the outer string branch`` () =
        expectEqual (NestedOneOfRoot.Create(value = Choice1Of2 "s")) (Choice1Of2 "s")

    [<Fact>]
    let ``nested oneOf root Create builds the value from the inner int branch`` () =
        expectEqual (NestedOneOfRoot.Create(value = Choice2Of2(Choice1Of2 3))) (Choice2Of2(Choice1Of2 3))

    [<Fact>]
    let ``nested oneOf root Create builds the value from the inner bool branch`` () =
        expectEqual (NestedOneOfRoot.Create(value = Choice2Of2(Choice2Of2 false))) (Choice2Of2(Choice2Of2 false))
