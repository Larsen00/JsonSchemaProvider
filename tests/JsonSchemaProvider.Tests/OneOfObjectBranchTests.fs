namespace JsonSchemaProvider.Tests

// This file puts two *object* alternatives directly in the same oneOf. buildClassMapHelper's
// FSharpOneOf case (TypeProvider.fs:152) names every branch's generated class
// "<outer property name>Case", so both object branches under "value" both request the class name
// "valueCase". Each branch still gets its own ProvidedTypeDefinition internally, but the F#
// compiler resolves member access for either Choice1Of2/Choice2Of2 arm against only one of the two
// identically-named types - so a member unique to the other branch's shape ("b", "side") doesn't
// typecheck even inside its own matched arm, while a member both branches share by name ("kind")
// resolves fine either way. Runtime branch *selection* is unaffected (NJsonSchema validation still
// picks the right branch) - this is a generated-type naming gap, not a validation gap. The two
// affected tests below work around it by reading `case.ToString()` (inherited from the common
// NullableJsonValue base, so unaffected by which "valueCase" identity won) instead of the typed
// member.
module OneOfObjectBranchTests =
    open Expecto
    open JsonSchemaProvider

    // Branches share no properties, and additionalProperties:false on both sides means the
    // "wrong" property isn't just ignored - it makes that branch actively invalid.
    [<Literal>]
    let requiredPropertyDiscriminatorSchema =
        """
        {
          "type": "object",
          "properties": {
            "value": {
              "oneOf": [
                {
                  "type": "object",
                  "properties": { "a": { "type": "string" } },
                  "required": ["a"],
                  "additionalProperties": false
                },
                {
                  "type": "object",
                  "properties": { "b": { "type": "string" } },
                  "required": ["b"],
                  "additionalProperties": false
                }
              ]
            }
          },
          "required": ["value"]
        }"""

    // The two branches are structurally identical (same property count, same required set) once
    // you ignore "kind"'s const - the discriminator is a const string one level inside an object
    // branch, not anything visible at the branch's own JSON kind or shape.
    [<Literal>]
    let constDiscriminatorSchema =
        """
        {
          "type": "object",
          "properties": {
            "value": {
              "oneOf": [
                {
                  "type": "object",
                  "properties": {
                    "kind": { "type": "string", "const": "circle" },
                    "radius": { "type": "number" }
                  },
                  "required": ["kind", "radius"]
                },
                {
                  "type": "object",
                  "properties": {
                    "kind": { "type": "string", "const": "square" },
                    "side": { "type": "number" }
                  },
                  "required": ["kind", "side"]
                }
              ]
            }
          },
          "required": ["value"]
        }"""

    type RequiredPropertyDiscriminator = JsonSchemaProvider<schema = requiredPropertyDiscriminatorSchema>
    type ConstDiscriminator = JsonSchemaProvider<schema = constDiscriminatorSchema>

    let requiredPropertyDiscriminatorPicksABranch =
        test "object|object oneOf: {a} alone picks the a-branch" {
            let v = RequiredPropertyDiscriminator.Parse("""{"value": {"a": "x"}}""")

            match v.value with
            | Choice1Of2 case -> Expect.equal case.a "x" "a = \"x\""
            | Choice2Of2 _ -> failtest "expected the a-branch (Choice1Of2)"
        }

    let requiredPropertyDiscriminatorPicksBBranch =
        test "object|object oneOf: {b} alone picks the b-branch" {
            let v = RequiredPropertyDiscriminator.Parse("""{"value": {"b": "y"}}""")

            match v.value with
            | Choice2Of2 case ->
                // Can't say `case.b` here - see the file header. Read the raw JSON instead.
                Expect.stringContains (case.ToString()) "\"b\"" "the record has a \"b\" property"
                Expect.stringContains (case.ToString()) "\"y\"" "b's value is \"y\""
            | Choice1Of2 _ -> failtest "expected the b-branch (Choice2Of2)"
        }

    // Without additionalProperties:false this would be ambiguous instead (it'd satisfy the
    // a-branch's own required-property check AND the b-branch's) - additionalProperties:false is
    // what makes {a, b} together invalid against *both* branches, so this also confirms
    // additionalProperties is enforced per-branch, not just "required".
    let requiredPropertyDiscriminatorRejectsBothPropertiesPresent =
        test "object|object oneOf: {a, b} together satisfies neither branch and fails Parse validation" {
            Expect.throws
                (fun () -> RequiredPropertyDiscriminator.Parse("""{"value": {"a": "x", "b": "y"}}""") |> ignore)
                "additionalProperties:false rejects the other branch's property on both sides"
        }

    let constDiscriminatorPicksCircleBranch =
        test "object|object oneOf: kind=\"circle\" picks the circle branch" {
            let v = ConstDiscriminator.Parse("""{"value": {"kind": "circle", "radius": 2.5}}""")

            match v.value with
            | Choice1Of2 case ->
                Expect.equal case.kind "circle" "kind = \"circle\""
                Expect.equal case.radius 2.5 "radius = 2.5"
            | Choice2Of2 _ -> failtest "expected the circle branch (Choice1Of2)"
        }

    let constDiscriminatorPicksSquareBranch =
        test "object|object oneOf: kind=\"square\" picks the square branch" {
            let v = ConstDiscriminator.Parse("""{"value": {"kind": "square", "side": 4.0}}""")

            match v.value with
            | Choice2Of2 case ->
                Expect.equal case.kind "square" "kind = \"square\""
                // Can't say `case.side` here - see the file header. Read the raw JSON instead.
                Expect.stringContains (case.ToString()) "\"side\"" "the record has a \"side\" property"
                Expect.stringContains (case.ToString()) "4" "side's value is 4.0"
            | Choice1Of2 _ -> failtest "expected the square branch (Choice2Of2)"
        }

    let constDiscriminatorRejectsMismatchedKindAndShape =
        test "object|object oneOf: kind=\"circle\" with square's shape (side, no radius) fails Parse validation" {
            Expect.throws
                (fun () -> ConstDiscriminator.Parse("""{"value": {"kind": "circle", "side": 4.0}}""") |> ignore)
                "kind=\"circle\" requires \"radius\", not \"side\" - matches neither branch"
        }

    [<Tests>]
    let tests =
        testList
            "JsonSchemaProvider.Tests.OneOfObjectBranchTests"
            [ requiredPropertyDiscriminatorPicksABranch
              requiredPropertyDiscriminatorPicksBBranch
              requiredPropertyDiscriminatorRejectsBothPropertiesPresent
              constDiscriminatorPicksCircleBranch
              constDiscriminatorPicksSquareBranch
              constDiscriminatorRejectsMismatchedKindAndShape ]
