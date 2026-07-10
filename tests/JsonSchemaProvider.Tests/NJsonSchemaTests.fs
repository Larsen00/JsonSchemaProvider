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

    [<Tests>]
    let tests =
        testList "JsonSchemaProvider.Tests.NJsonSchemaTests" [ minItemsDefaultsToZeroWhenAbsent ]
