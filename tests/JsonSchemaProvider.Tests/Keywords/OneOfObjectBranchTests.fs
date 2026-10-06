namespace JsonSchemaProvider.Tests

// Two object branches in the same oneOf: branch selection, and each branch getting its own
// indexed provided type ("valueCase1", "valueCase2") whose members are reachable by name.
module OneOfObjectBranchTests =
    open Expecto
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

    let requiredPropertyDiscriminatorPicksABranch =
        test "object|object oneOf: {a} alone picks the a-branch" {
            let v = Expect.wantOk (RequiredPropertyDiscriminator.Parse("""{"value": {"a": "x"}}""")) "Parse should succeed"
            match v.value with
            | Choice1Of2 case -> Expect.equal case.a "x" "a = \"x\""
            | Choice2Of2 _ -> failtest "expected the a-branch (Choice1Of2)"
        }

    let requiredPropertyDiscriminatorPicksBBranch =
        test "object|object oneOf: {b} alone picks the b-branch" {
            let v = Expect.wantOk (RequiredPropertyDiscriminator.Parse("""{"value": {"b": "y"}}""")) "Parse should succeed"
            match v.value with
            | Choice2Of2 case -> Expect.equal case.b "y" "b = \"y\""
            | Choice1Of2 _ -> failtest "expected the b-branch (Choice2Of2)"
        }

    // Before branches were indexed, both were named "valueCase" and only the first resolved by name.
    let eachBranchHasItsOwnCreate =
        test "object|object oneOf: each branch's indexed type has its own Create" {
            let a = Expect.wantOk (RequiredPropertyDiscriminator.valueCase1.Create(a = "x")) "first branch Create should succeed"
            let b = Expect.wantOk (RequiredPropertyDiscriminator.valueCase2.Create(b = "y")) "second branch Create should succeed"
            Expect.equal a.a "x" "first branch's Create builds an a-record"
            Expect.equal b.b "y" "second branch's Create builds a b-record"
        }

    let secondBranchParseValidatesItsOwnSubschema =
        test "object|object oneOf: the second branch's Parse validates against its own sub-schema" {
            let b = Expect.wantOk (RequiredPropertyDiscriminator.valueCase2.Parse("""{"b": "y"}""")) "Parse should succeed"
            Expect.equal b.b "y" "b = \"y\""
            Expect.isError (RequiredPropertyDiscriminator.valueCase2.Parse("""{"a": "x"}""")) "the a-branch's JSON fails the b-branch"
        }

    // additionalProperties:false makes {a, b} invalid against *both* branches, not just ambiguous.
    let requiredPropertyDiscriminatorRejectsBothPropertiesPresent =
        test "object|object oneOf: {a, b} together satisfies neither branch and fails Parse validation" {
            Expect.isError
                (RequiredPropertyDiscriminator.Parse("""{"value": {"a": "x", "b": "y"}}"""))
                "additionalProperties:false rejects the other branch's property on both sides"
        }

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

    let constDiscriminatorPicksCircleBranch =
        test "object|object oneOf: kind=\"circle\" picks the circle branch" {
            let v = Expect.wantOk (ConstDiscriminator.Parse("""{"value": {"kind": "circle", "radius": 2.5}}""")) "Parse should succeed"
            match v.value with
            | Choice1Of2 case ->
                Expect.equal case.kind "circle" "kind = \"circle\""
                Expect.equal case.radius 2.5 "radius = 2.5"
            | Choice2Of2 _ -> failtest "expected the circle branch (Choice1Of2)"
        }

    let constDiscriminatorPicksSquareBranch =
        test "object|object oneOf: kind=\"square\" picks the square branch" {
            let v = Expect.wantOk (ConstDiscriminator.Parse("""{"value": {"kind": "square", "side": 4.0}}""")) "Parse should succeed"
            match v.value with
            | Choice2Of2 case ->
                Expect.equal case.kind "square" "kind = \"square\""
                Expect.equal case.side 4.0 "side = 4.0"
            | Choice1Of2 _ -> failtest "expected the square branch (Choice2Of2)"
        }

    // No test for {"kind":"circle","side":4.0}: rejecting it needs const enforcement, which
    // NJsonSchema doesn't do - "side" alone already satisfies the square branch's required set.

    [<Tests>]
    let tests =
        testList
            "JsonSchemaProvider.Tests.OneOfObjectBranchTests"
            [ requiredPropertyDiscriminatorPicksABranch
              requiredPropertyDiscriminatorPicksBBranch
              eachBranchHasItsOwnCreate
              secondBranchParseValidatesItsOwnSubschema
              requiredPropertyDiscriminatorRejectsBothPropertiesPresent
              constDiscriminatorPicksCircleBranch
              constDiscriminatorPicksSquareBranch ]
