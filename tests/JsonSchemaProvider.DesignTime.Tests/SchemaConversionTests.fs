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

    let jsonIntegerNoneKeywords : JsonSchemaProvider.JsonInteger.SpecificKeywords =
        { minimum = None
          maximum = None
          exclusiveMinimum = None
          exclusiveMaximum = None
          multipleOf = None }

    // Path is computed via NJsonSchema's own JsonPathUtilities.GetJsonPath(root, node) - a JSON
    // Pointer identifying this object's own position in the document. Values below were captured
    // by running the real conversion against these exact schemas (not guessed), so a mismatch here
    // means Path computation itself has changed, not just that this literal is stale.
    let nestedArrayWithObjectItemsShouldBeParsedCorrectly =
        test "NestedArrayWithObjectItems should be parsed correctly" {
            let actual = parseJsonSchema nestedArrayWithObjectItems

            let expected =
                JsonObject(
                    { Required = Map.ofList [ "values", false ]; Path = "#" },
                    [ ("values",
                       JsonArray(
                           JsonArray(
                               JsonObject(
                                   { Required = Map.ofList [ "propA", false; "propB", false ]
                                     Path = "#/properties/values/items/items" },
                                   [ ("propA", JsonInteger jsonIntegerNoneKeywords)
                                     ("propB", JsonString) ]
                               ),
                               { MinItems = None }
                           ),
                           { MinItems = None }
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
                    { Required = Map.ofList [ "header", false; "body", true ]; Path = "#" },
                    [ ("header",
                       FSharpClass(
                           { Required = Map.ofList [ "id", true; "sender", true; "resend", false; "time", false ]
                             Path = "#/properties/header" },
                           [ ("id", FSharpInt jsonIntegerNoneKeywords)
                             ("sender", FSharpString)
                             ("resend", FSharpBool)
                             ("time",
                              FSharpClass(
                                  { Required = Map.ofList [ "hour", true; "minute", true; "second", true ]
                                    Path = "#/properties/header/properties/time" },
                                  [ ("hour", FSharpInt jsonIntegerNoneKeywords)
                                    ("minute", FSharpInt jsonIntegerNoneKeywords)
                                    ("second", FSharpInt jsonIntegerNoneKeywords) ]
                              )) ]
                       ))
                      ("body",
                       FSharpClass(
                           { Required = Map.ofList [ "length", true; "payload", true ]
                             Path = "#/properties/body" },
                           [ ("length", FSharpInt jsonIntegerNoneKeywords)
                             ("payload", FSharpString) ]
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
                    { Required = Map.ofList [ "values", false ]; Path = "#" },
                    [ ("values",
                       FSharpList(
                           FSharpList(
                               FSharpClass(
                                   { Required = Map.ofList [ "propA", false; "propB", false ]
                                     Path = "#/properties/values/items/items" },
                                   [ ("propA", FSharpInt jsonIntegerNoneKeywords)
                                     ("propB", FSharpString) ]
                               ),
                               { MinItems = None }
                           ),
                           { MinItems = None }
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
