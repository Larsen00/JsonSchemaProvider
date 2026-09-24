namespace JsonSchemaProvider.Tests

// Two object branches in the same oneOf both get named "valueCase" (buildClassMapHelper's naming
// rule), so the compiler resolves a member access against only one of the two identically-named
// types. The two b-branch/square-branch tests below work around it via ToString() instead of the
// typed member - a generated-type naming gap, not a validation gap (branch selection still works).
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
            | Choice2Of2 case ->
                Expect.stringContains (case.ToString()) "\"b\"" "the record has a \"b\" property"
                Expect.stringContains (case.ToString()) "\"y\"" "b's value is \"y\""
            | Choice1Of2 _ -> failtest "expected the b-branch (Choice2Of2)"
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
                Expect.stringContains (case.ToString()) "\"side\"" "the record has a \"side\" property"
                Expect.stringContains (case.ToString()) "4" "side's value is 4.0"
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
              requiredPropertyDiscriminatorRejectsBothPropertiesPresent
              constDiscriminatorPicksCircleBranch
              constDiscriminatorPicksSquareBranch ]
