namespace RefinedJsonSchemaProvider.Tests

// 2019-09/2020-12 keywords the provider has no encoding for. Pins two things per keyword:
// NJsonSchema 11 keeps it through ToJson, and the node is not compilable. What NJsonSchema's
// Validate (the runtime fallback) does with it is in KnownGaps/KnownGapTests.fs.
module Draft2020KeywordTests =
    open Expecto
    open RefinedJsonSchemaProvider
    open RefinedJsonSchemaProvider.DesignTime.SchemaConversion
    open RefinedJsonSchemaProvider.DesignTime.ArrayShape
    open TestHelpers

    // keyword, schema using it, instance the spec rejects, instance the spec accepts
    let arrayCases =
        [ "prefixItems", """{ "type": "array", "items": { "type": "integer" }, "prefixItems": [ { "const": 1 } ], "minItems": 2 }""", "[2, 3]", "[1, 3]"
          "contains", """{ "type": "array", "items": { "type": "integer" }, "contains": { "const": 1 }, "minItems": 2 }""", "[2, 3]", "[2, 1]"
          "minContains", """{ "type": "array", "items": { "type": "integer" }, "contains": { "const": 1 }, "minContains": 2, "maxItems": 3 }""", "[1, 2]", "[1, 1]"
          "maxContains", """{ "type": "array", "items": { "type": "integer" }, "contains": { "const": 1 }, "maxContains": 1, "maxItems": 3 }""", "[1, 1]", "[1, 2]"
          "unevaluatedItems", """{ "type": "array", "items": { "type": "integer" }, "prefixItems": [ { "type": "integer" } ], "unevaluatedItems": false, "maxItems": 2 }""", "[1, 2]", "[1]" ]

    let objectCases =
        [ "dependentRequired", """{ "type": "object", "properties": { "a": { "type": "string" }, "b": { "type": "string" } }, "dependentRequired": { "a": ["b"] } }""", """{ "a": "x" }""", """{ "a": "x", "b": "y" }"""
          "dependentSchemas", """{ "type": "object", "properties": { "a": { "type": "string" }, "b": { "type": "string" } }, "dependentSchemas": { "a": { "required": ["b"] } } }""", """{ "a": "x" }""", """{ "a": "x", "b": "y" }"""
          "propertyNames", """{ "type": "object", "properties": { "abc": { "type": "string" } }, "propertyNames": { "maxLength": 2 } }""", """{ "abc": "x" }""", "{}"
          "unevaluatedProperties", """{ "type": "object", "properties": { "a": { "type": "string" } }, "unevaluatedProperties": false }""", """{ "a": "x", "z": 1 }""", """{ "a": "x" }""" ]

    // Draft 4-7 "dependencies", which 2020-12 split into dependentRequired and dependentSchemas.
    let dependenciesCase =
        "dependencies", """{ "type": "object", "properties": { "a": { "type": "string" }, "b": { "type": "string" } }, "dependencies": { "a": ["b"] } }""", """{ "a": "x" }""", """{ "a": "x", "b": "y" }"""

    let allCases = arrayCases @ objectCases @ [ dependenciesCase ]

    // Next to items, unevaluatedItems never applies (items evaluates every element), and the
    // provider rejects an array without items, so its Validate case uses a schema the provider can't take.
    let validateCases =
        allCases
        |> List.map (fun ((keyword, _, _, _) as case) ->
            if keyword = "unevaluatedItems" then
                keyword, """{ "type": "array", "prefixItems": [ { "type": "integer" } ], "unevaluatedItems": false }""", "[1, 2]", "[1]"
            else
                case)

    let private canBeCompiled schemaType =
        match schemaType with
        | JsonArray(_, k) -> k.common.CanBeCompiled
        | JsonObject(k, _) -> k.common.CanBeCompiled
        | other -> failwithf "Expected an array or object, got %A" other

    let keptByToJson =
        testList
            "NJsonSchema keeps the keyword through ToJson"
            [ for keyword, schemaText, _, _ in allCases ->
                  test keyword {
                      let json = (SchemaCache.parseSchema schemaText).ToJson()
                      Expect.stringContains json $"\"{keyword}\"" "canBeCompiled diffs ToJson keys, so a dropped keyword would look compilable"
                  } ]

    let notCompilable =
        testList
            "the node is not compilable"
            [ for keyword, schemaText, _, _ in allCases ->
                  test keyword { Expect.isFalse (canBeCompiled (parseJsonSchema schemaText)) schemaText } ]

    let arraysFallBack =
        testList
            "an array using the keyword is UnsupportedKeywords and not fully compilable"
            [ for keyword, schemaText, _, _ in arrayCases ->
                  test keyword {
                      match parseJsonSchema schemaText with
                      | JsonArray(inner, keywords) ->
                          match classifyArrayShape defaultFlags keywords with
                          | UnsupportedKeywords -> ()
                          | shape -> failtestf "Expected UnsupportedKeywords, got %A" shape
                      | other -> failtestf "Expected an array, got %A" other

                      Expect.isFalse (convertRoot (fixture schemaText)).FullyCompilable "Create must not skip validation"
                  } ]

    // 11.0.2 rewrote it to draft-04; Validate behaves the same either way. That Validate ignores
    // the keywords above is pinned in KnownGaps/KnownGapTests.fs.
    let schemaKept =
        test "a 2020-12 $schema is kept by ToJson" {
            let schema = SchemaCache.parseSchema """{ "$schema": "https://json-schema.org/draft/2020-12/schema", "type": "string" }"""
            Expect.stringContains (schema.ToJson()) "draft/2020-12" ""
        }

    [<Tests>]
    let tests =
        testList
            "Draft2020KeywordTests"
            [ keptByToJson; notCompilable; arraysFallBack; schemaKept ]
