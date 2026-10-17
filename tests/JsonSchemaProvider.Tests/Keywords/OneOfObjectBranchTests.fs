namespace JsonSchemaProvider.Tests

// Two object branches in the same oneOf: branch selection, and each branch getting its own
// indexed provided type ("valueCase1", "valueCase2") whose members are reachable by name.
module OneOfObjectBranchTests =
    open Xunit
    open JsonSchemaProvider

    [<Literal>]
    let requiredPropertyDiscriminatorSchema =
        """
        {
          "type": "object",
          "properties": {
            "value": {
              "oneOf": [
                { "type": "object", "properties": { "a": { "type": "string" } }, "required": ["a"], "additionalProperties": false },
                { "type": "object", "properties": { "b": { "type": "string" } }, "required": ["b"], "additionalProperties": false }
              ]
            }
          },
          "required": ["value"]
        }"""
    type RequiredPropertyDiscriminator = JsonSchemaProvider<schema = requiredPropertyDiscriminatorSchema>

    [<Fact>]
    let ``object|object oneOf: {a} alone picks the a-branch`` () =
        let v = expectOk (RequiredPropertyDiscriminator.Parse("""{"value": {"a": "x"}}"""))
        match v.value with
        | Choice1Of2 case -> expectEqual case.a "x"
        | Choice2Of2 _ -> failwith "expected the a-branch (Choice1Of2)"

    [<Fact>]
    let ``object|object oneOf: {b} alone picks the b-branch`` () =
        let v = expectOk (RequiredPropertyDiscriminator.Parse("""{"value": {"b": "y"}}"""))
        match v.value with
        | Choice2Of2 case -> expectEqual case.b "y"
        | Choice1Of2 _ -> failwith "expected the b-branch (Choice2Of2)"

    // Before branches were indexed, both were named "valueCase" and only the first resolved by name.
    [<Fact>]
    let ``object|object oneOf: each branch's indexed type has its own Create`` () =
        let a = expectOk (RequiredPropertyDiscriminator.valueCase1.Create(a = "x"))
        let b = expectOk (RequiredPropertyDiscriminator.valueCase2.Create(b = "y"))
        expectEqual a.a "x"
        expectEqual b.b "y"

    [<Fact>]
    let ``object|object oneOf: the second branch's Parse validates against its own sub-schema`` () =
        let b = expectOk (RequiredPropertyDiscriminator.valueCase2.Parse("""{"b": "y"}"""))
        expectEqual b.b "y"
        expectError (RequiredPropertyDiscriminator.valueCase2.Parse("""{"a": "x"}""")) // the a-branch's JSON fails the b-branch

    // additionalProperties:false makes {a, b} invalid against *both* branches, not just ambiguous.
    [<Fact>]
    let ``object|object oneOf: {a, b} together satisfies neither branch and fails Parse validation`` () =
        expectError (RequiredPropertyDiscriminator.Parse("""{"value": {"a": "x", "b": "y"}}""")) // additionalProperties:false rejects the other branch's property on both sides

    // "kind"'s const is never enforced by NJsonSchema - these tests actually pass because
    // "radius" vs "side" already differs between the branches' required sets.
    [<Literal>]
    let constDiscriminatorSchema =
        """
        {
          "type": "object",
          "properties": {
            "value": {
              "oneOf": [
                { "type": "object", "properties": { "kind": { "type": "string", "const": "circle" }, "radius": { "type": "number" } }, "required": ["kind", "radius"] },
                { "type": "object", "properties": { "kind": { "type": "string", "const": "square" }, "side": { "type": "number" } }, "required": ["kind", "side"] }
              ]
            }
          },
          "required": ["value"]
        }"""
    type ConstDiscriminator = JsonSchemaProvider<schema = constDiscriminatorSchema>

    [<Fact>]
    let ``object|object oneOf: kind="circle" picks the circle branch`` () =
        let v = expectOk (ConstDiscriminator.Parse("""{"value": {"kind": "circle", "radius": 2.5}}"""))
        match v.value with
        | Choice1Of2 case ->
            expectEqual case.kind "circle"
            expectEqual case.radius 2.5
        | Choice2Of2 _ -> failwith "expected the circle branch (Choice1Of2)"

    [<Fact>]
    let ``object|object oneOf: kind="square" picks the square branch`` () =
        let v = expectOk (ConstDiscriminator.Parse("""{"value": {"kind": "square", "side": 4.0}}"""))
        match v.value with
        | Choice2Of2 case ->
            expectEqual case.kind "square"
            expectEqual case.side 4.0
        | Choice1Of2 _ -> failwith "expected the square branch (Choice2Of2)"

    // No test for {"kind":"circle","side":4.0}: rejecting it needs const enforcement, which
    // NJsonSchema doesn't do - "side" alone already satisfies the square branch's required set.
