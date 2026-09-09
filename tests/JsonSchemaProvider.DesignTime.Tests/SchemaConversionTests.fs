namespace JsonSchemaProvider.Tests

module SchemaConversionTests =
    open JsonSchemaProvider.DesignTime.SchemaConversion
    open Expecto

    [<Literal>]
    let flatObject =
        """
        {
          "type": "object",
          "properties": {
            "X": { "type": "string" },
            "Y": { "type": "string" },
              "Z": { "type": "integer" }
          }
        }"""

    [<Literal>]
    let nestedArrayWithObjectItems =
        """
        {
          "type": "object",
          "properties": {
            "values": {
              "type": "array",
              "items": {
                "type": "array",
                "items": {
                  "type": "object",
                  "properties": {
                    "propA": {"type": "integer"},
                    "propB": {"type": "string"}
                  }
                }
              }
            }
          }
        }"""

    [<Literal>]
    let nestedObjects =
        """
        {
          "type": "object",
          "properties": {
            "header": {
              "type": "object",
              "properties": {
                "id": {"type": "integer"},
                "sender": {"type": "string"},
                "resend": {"type": "boolean"},
                "time": {
                  "type": "object",
                  "properties": {
                    "hour": {"type": "integer"},
                    "minute": {"type": "integer"},
                    "second": {"type": "integer"}
                  },
                  "required": ["hour", "minute", "second"]
                }
              },
              "required": ["id", "sender"]
            },
            "body": {
              "type": "object",
              "properties": {
                "length": {"type": "integer"},
                "payload": {"type": "string"}
              },
              "required": ["length", "payload"]
            }
          },
          "required": ["body"]
        }"""

    // Path is computed via NJsonSchema's own JsonPathUtilities.GetJsonPath(root, node) - a JSON
    // Pointer identifying this node's own position in the document. Every FSharpType/JsonSchemaType
    // case now carries its own Path (via `common`), not just objects - see
    // ideas/oneof-runtime-disambiguation.md's "Superseded: path lives on every type's own Keywords"
    // section. Values below were captured by running the real conversion against these exact
    // schemas (not guessed), so a mismatch here means Path computation itself has changed, not
    // just that this literal is stale.
    let private commonAt (path: string) : JsonSchemaProvider.Common.Keywords = { Path = path }

    let private objKeywords (path: string) (required: Map<string, bool>) : JsonSchemaProvider.JsonObject.Keywords =
        { common = commonAt path; specific = { Required = required } }

    let private arrKeywords (path: string) : JsonSchemaProvider.JsonArray.Keywords =
        { common = commonAt path; specific = { MinItems = None } }

    let private intKeywordsAt (path: string) : JsonSchemaProvider.JsonNumber.Keywords =
        { common = commonAt path
          specific =
            { minimum = None
              maximum = None
              exclusiveMinimum = None
              exclusiveMaximum = None
              multipleOf = None } }

    let private stringKeywordsAt (path: string) : JsonSchemaProvider.JsonString.Keywords =
        { common = commonAt path
          specific = { minLength = None; maxLength = None; pattern = None; format = None } }

    let private boolKeywordsAt (path: string) : JsonSchemaProvider.JsonBoolean.Keywords =
        { common = commonAt path }

    let nestedArrayWithObjectItemsShouldBeParsedCorrectly =
        test "NestedArrayWithObjectItems should be parsed correctly" {
            let actual = parseJsonSchema nestedArrayWithObjectItems

            let expected =
                JsonObject(
                    objKeywords "#" (Map.ofList [ "values", false ]),
                    [ ("values",
                       JsonArray(
                           JsonArray(
                               JsonObject(
                                   objKeywords "#/properties/values/items/items" (Map.ofList [ "propA", false; "propB", false ]),
                                   [ ("propA", JsonInteger(intKeywordsAt "#/properties/values/items/items/properties/propA"))
                                     ("propB", JsonString(stringKeywordsAt "#/properties/values/items/items/properties/propB")) ]
                               ),
                               arrKeywords "#/properties/values/items"
                           ),
                           arrKeywords "#/properties/values"
                       )) ]
                )

            Expect.equal actual expected ""
        }

    let nestedObjectsShouldBeClassTreeWithFourClasses =
        test "NestedObjects should be class tree with four classes" {
            let actual =
                parseJsonSchema nestedObjects
                |> jsonSchemaTypeToFSharpType

            let expected =
                FSharpClass(
                    objKeywords "#" (Map.ofList [ "header", false; "body", true ]),
                    [ ("header",
                       FSharpClass(
                           objKeywords
                               "#/properties/header"
                               (Map.ofList [ "id", true; "sender", true; "resend", false; "time", false ]),
                           [ ("id", FSharpInt(intKeywordsAt "#/properties/header/properties/id"))
                             ("sender", FSharpString(stringKeywordsAt "#/properties/header/properties/sender"))
                             ("resend", FSharpBool(boolKeywordsAt "#/properties/header/properties/resend"))
                             ("time",
                              FSharpClass(
                                  objKeywords
                                      "#/properties/header/properties/time"
                                      (Map.ofList [ "hour", true; "minute", true; "second", true ]),
                                  [ ("hour", FSharpInt(intKeywordsAt "#/properties/header/properties/time/properties/hour"))
                                    ("minute", FSharpInt(intKeywordsAt "#/properties/header/properties/time/properties/minute"))
                                    ("second", FSharpInt(intKeywordsAt "#/properties/header/properties/time/properties/second")) ]
                              )) ]
                       ))
                      ("body",
                       FSharpClass(
                           objKeywords "#/properties/body" (Map.ofList [ "length", true; "payload", true ]),
                           [ ("length", FSharpInt(intKeywordsAt "#/properties/body/properties/length"))
                             ("payload", FSharpString(stringKeywordsAt "#/properties/body/properties/payload")) ]
                       )) ]
                )

            Expect.equal actual expected ""
        }

    let nestedArrayWithObjectItemsShouldBeClassTreeWithTwoClasses =
        test "NestedArrayWithObjectItems should be class tree with two classes" {
            let actual =
                parseJsonSchema nestedArrayWithObjectItems
                |> jsonSchemaTypeToFSharpType

            let expected =
                FSharpClass(
                    objKeywords "#" (Map.ofList [ "values", false ]),
                    [ ("values",
                       FSharpList(
                           FSharpList(
                               FSharpClass(
                                   objKeywords "#/properties/values/items/items" (Map.ofList [ "propA", false; "propB", false ]),
                                   [ ("propA", FSharpInt(intKeywordsAt "#/properties/values/items/items/properties/propA"))
                                     ("propB", FSharpString(stringKeywordsAt "#/properties/values/items/items/properties/propB")) ]
                               ),
                               arrKeywords "#/properties/values/items"
                           ),
                           arrKeywords "#/properties/values"
                       )) ]
                )

            Expect.equal actual expected ""
        }

    [<Tests>]
    let tests =
        testList
            "JsonSchemaProvider.Tests.SchemaConversionTests"
            [ nestedArrayWithObjectItemsShouldBeParsedCorrectly
              nestedArrayWithObjectItemsShouldBeClassTreeWithTwoClasses
              nestedObjectsShouldBeClassTreeWithFourClasses ]
