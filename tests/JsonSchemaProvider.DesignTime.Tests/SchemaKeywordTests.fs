namespace JsonSchemaProvider.Tests

// Keyword capture, CanBeCompiled and Path, checked through parseJsonSchema on real schema text.
module SchemaKeywordTests =
    open Expecto
    open JsonSchemaProvider
    open JsonSchemaProvider.DesignTime.SchemaConversion
    open JsonSchemaProvider.DesignTime.NodeConversions

    let private canBeCompiled (schemaType: JsonSchemaType) =
        match schemaType with
        | JsonBoolean k -> k.common.CanBeCompiled
        | JsonInteger k
        | JsonNumber k -> k.common.CanBeCompiled
        | JsonString k -> k.common.CanBeCompiled
        | JsonObject(k, _) -> k.common.CanBeCompiled
        | JsonArray(_, k) -> k.common.CanBeCompiled
        | JsonOneOf(k, _, _) -> k.CanBeCompiled

    let private expectCompilable (schemaText: string) (expected: bool) =
        Expect.equal (canBeCompiled (parseJsonSchema schemaText)) expected schemaText

    let private stringKeywords schemaText =
        match parseJsonSchema schemaText with
        | JsonString k -> k
        | other -> failwithf "Expected a string, got %A" other

    let private numberKeywords schemaText =
        match parseJsonSchema schemaText with
        | JsonInteger k
        | JsonNumber k -> k
        | other -> failwithf "Expected a number, got %A" other

    let private objectOf schemaText =
        match parseJsonSchema schemaText with
        | JsonObject(k, properties) -> k, properties
        | other -> failwithf "Expected an object, got %A" other

    let private arrayKeywords schemaText =
        match parseJsonSchema schemaText with
        | JsonArray(_, k) -> k
        | other -> failwithf "Expected an array, got %A" other

    let canBeCompiledTests =
        testList
            "CanBeCompiled"
            [ test "plain primitives are compilable" {
                  for t in [ "string"; "integer"; "number"; "boolean" ] do
                      expectCompilable $"""{{ "type": "{t}" }}""" true
              }

              test "annotation keywords do not disqualify a node" {
                  expectCompilable
                      """{ "type": "string", "title": "T", "description": "D", "default": "x", "examples": ["y"], "$comment": "c", "deprecated": true }"""
                      true
              }

              test "a string validation keyword disqualifies the node" {
                  expectCompilable """{ "type": "string", "minLength": 1 }""" false
                  expectCompilable """{ "type": "string", "pattern": "^a" }""" false
                  expectCompilable """{ "type": "string", "format": "email" }""" false
              }

              test "a numeric validation keyword disqualifies the node" {
                  expectCompilable """{ "type": "integer", "minimum": 0 }""" false
                  expectCompilable """{ "type": "number", "multipleOf": 0.5 }""" false
              }

              test "enum and const disqualify the node" {
                  expectCompilable """{ "type": "string", "enum": ["a", "b"] }""" false
                  expectCompilable """{ "type": "integer", "const": 3 }""" false
              }

              test "object keywords beyond properties/required disqualify the object" {
                  expectCompilable """{ "type": "object", "properties": { "a": { "type": "string" } }, "required": ["a"] }""" true
                  expectCompilable """{ "type": "object", "minProperties": 1 }""" false
                  expectCompilable """{ "type": "object", "additionalProperties": false }""" false
                  expectCompilable """{ "type": "object", "patternProperties": { "^x": { "type": "string" } } }""" false
              }

              test "array size keywords keep the array node itself compilable" {
                  expectCompilable """{ "type": "array", "items": { "type": "integer" }, "minItems": 1, "maxItems": 3, "uniqueItems": true }""" true
              }

              test "a property's keyword only disqualifies that property, not its parent" {
                  let schemaText = """{ "type": "object", "properties": { "a": { "type": "integer", "minimum": 0 } } }"""
                  let keywords, properties = objectOf schemaText
                  Expect.isTrue keywords.common.CanBeCompiled "parent object"
                  Expect.isFalse (canBeCompiled (snd properties.Head)) "property a"
              }

              test "a oneOf node with a sibling validation keyword is not compilable" {
                  expectCompilable """{ "oneOf": [ { "type": "integer" }, { "type": "string" } ] }""" true
                  expectCompilable """{ "oneOf": [ { "type": "integer" }, { "type": "string" } ], "minLength": 2 }""" false
              }

              // "type" wins over "oneOf" in parsing; the ignored oneOf must at least force runtime validation.
              test "a typed node with an ignored oneOf is not compilable" {
                  let schemaText = """{ "type": "string", "oneOf": [ { "minLength": 1 }, { "maxLength": 0 } ] }"""
                  match parseJsonSchema schemaText with
                  | JsonString k -> Expect.isFalse k.common.CanBeCompiled ""
                  | other -> failtestf "Expected JsonString, got %A" other
              } ]

    let keywordCaptureTests =
        testList
            "Keyword capture"
            [ test "string keywords are captured" {
                  let k = stringKeywords """{ "type": "string", "minLength": 2, "maxLength": 5, "pattern": "^a", "format": "email" }"""
                  let expected : JsonString.Specific = { MinLength = Some 2; MaxLength = Some 5; Pattern = Some "^a"; Format = Some "email" }
                  Expect.equal k.specific expected ""
              }

              test "absent string keywords are None" {
                  let k = stringKeywords """{ "type": "string" }"""
                  let expected : JsonString.Specific = { MinLength = None; MaxLength = None; Pattern = None; Format = None }
                  Expect.equal k.specific expected ""
              }

              test "numeric bounds are captured as floats" {
                  let k = numberKeywords """{ "type": "integer", "minimum": 1, "maximum": 10, "multipleOf": 2 }"""
                  Expect.equal k.specific.Minimum (Some 1.0) "minimum"
                  Expect.equal k.specific.Maximum (Some 10.0) "maximum"
                  Expect.equal k.specific.MultipleOf (Some 2.0) "multipleOf"
              }

              test "integer and number stay distinct" {
                  Expect.isTrue (match parseJsonSchema """{ "type": "integer" }""" with JsonInteger _ -> true | _ -> false) "integer"
                  Expect.isTrue (match parseJsonSchema """{ "type": "number" }""" with JsonNumber _ -> true | _ -> false) "number"
              }

              test "required is recorded per property" {
                  let k, _ =
                      objectOf """{ "type": "object", "properties": { "a": { "type": "string" }, "b": { "type": "string" } }, "required": ["b"] }"""
                  Expect.equal k.specific.Required (Map.ofList [ "a", false; "b", true ]) ""
              }

              test "property order follows the schema" {
                  let _, properties =
                      objectOf """{ "type": "object", "properties": { "z": { "type": "string" }, "a": { "type": "string" }, "m": { "type": "string" } } }"""
                  Expect.equal (properties |> List.map fst) [ "z"; "a"; "m" ] ""
              }

              test "object structure keywords are captured" {
                  let k, _ =
                      objectOf """{ "type": "object", "minProperties": 1, "maxProperties": 4, "additionalProperties": false, "patternProperties": { "^x": {} } }"""
                  Expect.equal k.specific.MinProperties (Some 1) "minProperties"
                  Expect.equal k.specific.MaxProperties (Some 4) "maxProperties"
                  Expect.isFalse k.specific.AllowAdditionalProperties "additionalProperties"
                  Expect.isTrue k.specific.HasPatternProperties "patternProperties"
              }

              test "an additionalProperties schema is recorded" {
                  let k, _ = objectOf """{ "type": "object", "additionalProperties": { "type": "string" } }"""
                  Expect.isTrue k.specific.HasAdditionalPropertiesSchema ""
              }

              test "array keywords are captured" {
                  let k = arrayKeywords (TestHelpers.intArray ", \"minItems\": 2, \"maxItems\": 4, \"uniqueItems\": true")
                  Expect.equal k.specific.MinItems (Some 2) "minItems"
                  Expect.equal k.specific.MaxItems (Some 4) "maxItems"
                  Expect.isTrue k.specific.UniqueItems "uniqueItems"
              }

              test "a schema with neither type nor oneOf is rejected" {
                  Expect.throws (fun () -> parseJsonSchema "{}" |> ignore) "untyped schema"
              } ]

    let pathTests =
        testList
            "Path"
            [ test "root is #" { Expect.equal (pathOf (parseJsonSchema """{ "type": "string" }""")) "#" "" }

              test "array items get #/items" {
                  match parseJsonSchema (TestHelpers.intArray "") with
                  | JsonArray(inner, _) -> Expect.equal (pathOf inner) "#/items" ""
                  | other -> failtestf "%A" other
              }

              test "oneOf branches are indexed" {
                  match parseJsonSchema """{ "oneOf": [ { "type": "integer" }, { "type": "string" }, { "type": "boolean" } ] }""" with
                  | JsonOneOf(_, head, tail) ->
                      Expect.equal (head :: tail |> List.map pathOf) [ "#/oneOf/0"; "#/oneOf/1"; "#/oneOf/2" ] ""
                  | other -> failtestf "%A" other
              }

              test "a oneOf inside a property keeps the full path" {
                  match parseJsonSchema """{ "type": "object", "properties": { "p": { "oneOf": [ { "type": "integer" }, { "type": "string" } ] } } }""" with
                  | JsonObject(_, [ "p", (JsonOneOf(_, _, [ second ]) as oneOf) ]) ->
                      Expect.equal (pathOf oneOf) "#/properties/p" "oneOf node"
                      Expect.equal (pathOf second) "#/properties/p/oneOf/1" "second branch"
                  | other -> failtestf "%A" other
              }

              test "every node in a schema has a distinct path" {
                  let schemaText =
                      """
                      { "type": "object",
                        "properties": {
                          "a": { "type": "array", "items": { "type": "array", "items": { "type": "integer" } } },
                          "b": { "oneOf": [ { "type": "object", "properties": { "x": { "type": "string" } } }, { "type": "integer" } ] }
                        } }"""

                  let rec paths schemaType =
                      pathOf schemaType
                      :: match schemaType with
                         | JsonObject(_, properties) -> properties |> List.collect (snd >> paths)
                         | JsonArray(inner, _) -> paths inner
                         | JsonOneOf(_, head, tail) -> head :: tail |> List.collect paths
                         | _ -> []

                  let all = paths (parseJsonSchema schemaText)
                  Expect.equal (List.distinct all) all "convert caches by Path, so paths must be unique"
              } ]

    [<Tests>]
    let tests = testList "SchemaKeywordTests" [ canBeCompiledTests; keywordCaptureTests; pathTests ]
