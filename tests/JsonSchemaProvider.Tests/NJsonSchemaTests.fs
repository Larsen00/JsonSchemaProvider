namespace JsonSchemaProvider.Tests

module NJsonSchemaTests =
    open NJsonSchema
    open JsonSchemaProvider
    open Expecto

    let minItemsDefaultsToZeroWhenAbsent =
        test "Omitting minItems in a JSON schema should result in a default value of 0" {
            let schema =
                """
                {
                  "type": "array",
                  "items": {
                    "type": "string"
                  }
                }"""
                |> SchemaCache.parseSchema
            Expect.equal schema.MinItems 0 "Expected default value of 0 for minItems"
        }

    // ActualProperties reflects only the last allOf branch, not a union of all branches -
    // contrary to what SPEC_GAPS.md previously assumed.
    let actualPropertiesDoesNotMergePlainAllOfBranches =
        test "ActualProperties does not merge properties across plain (non-$ref) allOf branches" {
            let schema =
                """
                {
                  "allOf": [
                    { "type": "object", "properties": { "X": {"type": "string"} }, "required": ["X"] },
                    { "type": "object", "properties": { "Y": {"type": "integer"} } }
                  ]
                }"""
                |> SchemaCache.parseSchema

            Expect.equal schema.Type JsonObjectType.None "A pure allOf schema has no own 'type'"
            Expect.isEmpty (schema.Properties.Keys |> List.ofSeq) "Properties only reflects the schema's own (absent) properties"
            Expect.equal (schema.ActualProperties.Keys |> List.ofSeq) [ "Y" ] "Only the last branch survives"
            Expect.isEmpty (schema.RequiredProperties |> List.ofSeq) "X's required status from the first branch is lost"

            Expect.equal schema.AllOf.Count 2 "Both branches are still available individually via AllOf"
            Expect.equal ((Seq.item 0 schema.AllOf).Properties.Keys |> List.ofSeq) [ "X" ] "First branch keeps its own properties"
            Expect.equal ((Seq.item 1 schema.AllOf).Properties.Keys |> List.ofSeq) [ "Y" ] "Second branch keeps its own properties"
        }

    // Same loss occurs for the "$ref base + inline extension" shape, the pattern NJsonSchema's
    // inheritance support is normally documented around.
    let actualPropertiesDoesNotMergeRefBasedAllOfBranches =
        test "ActualProperties does not merge properties across $ref-based allOf branches" {
            let schema =
                """
                {
                  "definitions": {
                    "Base": {
                      "type": "object",
                      "properties": { "X": {"type": "string"} },
                      "required": ["X"]
                    }
                  },
                  "allOf": [
                    { "$ref": "#/definitions/Base" },
                    { "type": "object", "properties": { "Y": {"type": "integer"} } }
                  ]
                }"""
                |> SchemaCache.parseSchema

            Expect.equal
                (schema.ActualProperties.Keys |> List.ofSeq)
                [ "Y" ]
                "ActualProperties still reflects only the last (non-$ref) allOf branch"
        }

    [<Tests>]
    let tests =
        testList
            "JsonSchemaProvider.Tests.NJsonSchemaTests"
            [ minItemsDefaultsToZeroWhenAbsent
              actualPropertiesDoesNotMergePlainAllOfBranches
              actualPropertiesDoesNotMergeRefBasedAllOfBranches ]
