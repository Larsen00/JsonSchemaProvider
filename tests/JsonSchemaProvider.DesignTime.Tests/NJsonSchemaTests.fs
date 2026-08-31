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

    // A numeric-looking JSON string is never a valid "number" instance, no matter what its text
    // says - JSON Schema's "type" checks the underlying JSON kind, not whether the text could be
    // coerced. This is what guarantees a oneOf like [number, string] is never actually ambiguous
    // at the schema level for a value like the JSON string "42.5" - only the string branch is
    // ever truly satisfied, regardless of what a lenient runtime conversion might accept.
    let numericLookingStringFailsNumberType =
        test "A JSON string with numeric-looking text does not validate against type: number" {
            let schema = """{ "type": "number" }""" |> SchemaCache.parseSchema
            let errors = schema.Validate("\"42.5\"")
            Expect.isFalse (Seq.isEmpty errors) "A quoted JSON string must not satisfy type: number"
        }

    let numericLookingStringPassesStringType =
        test "A JSON string with numeric-looking text validates against type: string" {
            let schema = """{ "type": "string" }""" |> SchemaCache.parseSchema
            let errors = schema.Validate("\"42.5\"")
            Expect.isTrue (Seq.isEmpty errors) "A quoted JSON string must satisfy type: string"
        }

    let numericLookingStringUniquelyMatchesStringBranchOfOneOf =
        test "oneOf [number, string] accepts a numeric-looking JSON string only via the string branch" {
            let schema =
                """{ "oneOf": [ { "type": "number" }, { "type": "string" } ] }"""
                |> SchemaCache.parseSchema
            let errors = schema.Validate("\"42.5\"")
            Expect.isTrue (Seq.isEmpty errors) "The document is valid overall - satisfied uniquely by the string branch"
        }

    // Probing test for notes/any-type-as-root-refactor.md open item #1 (nested-class validation)
    // and #6 (primitive-root Create validation): does a nested property's own JsonSchema object
    // validate a JSON fragment correctly on its own, with no dependency on the parent document?
    // If so, item #1 doesn't need a full-document re-validate at all - just a reference to the
    // right nested JsonSchema node.
    let nestedPropertySchemaValidatesItsOwnFragmentIndependently =
        test "a property's own JsonSchema validates its own JSON value independently of the parent" {
            let schema =
                """
                {
                  "type": "object",
                  "properties": {
                    "age": { "type": "integer", "minimum": 5, "maximum": 10 }
                  }
                }"""
                |> SchemaCache.parseSchema

            let ageSchema = schema.Properties.["age"]

            let inRangeErrors = ageSchema.Validate("6")
            let belowMinimumErrors = ageSchema.Validate("3")

            Expect.isTrue (Seq.isEmpty inRangeErrors) "age=6 satisfies the nested schema validated on its own"
            Expect.isFalse (Seq.isEmpty belowMinimumErrors) "age=3 violates minimum when validated via the nested schema alone, with no parent object around it"
        }

    // Probing test for the "how do we relocate a nested schema fragment after a runtime
    // re-parse" problem (see notes/any-type-as-root-refactor.md open item #6 and
    // [[baseline-validation-behavior]]): a design-time JsonSchema object can't be embedded in a
    // quotation, only simple values (strings/ints/Guids) can. JsonPathUtilities.GetJsonPath gives
    // a path string identifying a node's position in the tree - if that path is stable across
    // independent parses of the same schema text, it's a candidate for "the id to carry in
    // FSharpType", without needing to inject anything into the schema JSON itself.
    let jsonPathForANestedPropertyIsStableAcrossIndependentParses =
        test "JsonPathUtilities.GetJsonPath returns the same path for the same property across two independent parses" {
            let schemaText =
                """
                {
                  "type": "object",
                  "properties": {
                    "age": { "type": "integer", "minimum": 5, "maximum": 10 }
                  }
                }"""

            let schema1 = SchemaCache.parseSchema schemaText
            let schema2 = SchemaCache.parseSchema schemaText

            let path1 = JsonPathUtilities.GetJsonPath(schema1, schema1.Properties.["age"])
            let path2 = JsonPathUtilities.GetJsonPath(schema2, schema2.Properties.["age"])

            Expect.equal path1 path2 (sprintf "Expected a stable path across independent parses, got '%s' vs '%s'" path1 path2)
            Expect.equal path1 "#/properties/age" "Expected a standard JSON Pointer identifying the property"
        }

    [<Tests>]
    let tests =
        testList
            "JsonSchemaProvider.Tests.NJsonSchemaTests"
            [ minItemsDefaultsToZeroWhenAbsent
              actualPropertiesDoesNotMergePlainAllOfBranches
              actualPropertiesDoesNotMergeRefBasedAllOfBranches
              numericLookingStringFailsNumberType
              numericLookingStringPassesStringType
              numericLookingStringUniquelyMatchesStringBranchOfOneOf
              nestedPropertySchemaValidatesItsOwnFragmentIndependently
              jsonPathForANestedPropertyIsStableAcrossIndependentParses ]
