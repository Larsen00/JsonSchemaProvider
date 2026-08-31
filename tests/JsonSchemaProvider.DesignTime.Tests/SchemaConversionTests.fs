namespace JsonSchemaProvider.Tests

module SchemaConversionTests =
    open System
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

    // FSharpClass carries a ClassID generated via Guid.NewGuid(), so a literal expected tree can
    // never match an actual one by identity. This replaces every ClassID with a fixed placeholder
    // so tests can assert on tree shape instead.
    let rec normalizeClassIds (fsharpType: FSharpType) : FSharpType =
        match fsharpType with
        | FSharpClass(_, properties) ->
            FSharpClass(
                Guid.Empty,
                properties
                |> List.map (fun (name, keywords, propertyType) -> name, keywords, normalizeClassIds propertyType)
            )
        | FSharpList(inner, keywords) -> FSharpList(normalizeClassIds inner, keywords)
        | FSharpOneOf types -> FSharpOneOf(List.map normalizeClassIds types)
        | FSharpBool
        | FSharpInt _
        | FSharpDouble
        | FSharpString -> fsharpType

    let nestedArrayWithObjectItemsShouldBeParsedCorrectly =
        test "NestedArrayWithObjectItems should be parsed correctly" {
            let actual = parseJsonSchema nestedArrayWithObjectItems

            let expected =
                JsonObject(
                    [ ("values",
                       { Required = false },
                       JsonArray(
                           JsonArray(
                               JsonObject(
                                   [ ("propA", { Required = false }, JsonInteger jsonIntegerNoneKeywords)
                                     ("propB", { Required = false }, JsonString) ]
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
                |> normalizeClassIds

            let expected =
                FSharpClass(
                    Guid.Empty,
                    [ ("header",
                       { Required = false },
                       FSharpClass(
                           Guid.Empty,
                           [ ("id", { Required = true }, FSharpInt jsonIntegerNoneKeywords)
                             ("sender", { Required = true }, FSharpString)
                             ("resend", { Required = false }, FSharpBool)
                             ("time",
                              { Required = false },
                              FSharpClass(
                                  Guid.Empty,
                                  [ ("hour", { Required = true }, FSharpInt jsonIntegerNoneKeywords)
                                    ("minute", { Required = true }, FSharpInt jsonIntegerNoneKeywords)
                                    ("second", { Required = true }, FSharpInt jsonIntegerNoneKeywords) ]
                              )) ]
                       ))
                      ("body",
                       { Required = true },
                       FSharpClass(
                           Guid.Empty,
                           [ ("length", { Required = true }, FSharpInt jsonIntegerNoneKeywords)
                             ("payload", { Required = true }, FSharpString) ]
                       )) ]
                )

            Expect.equal actual expected ""
        }

    let nestedArrayWithObjectItemsShouldBeClassTreeWithTwoClasses =
        test "NestedArrayWithObjectItems should be class tree with two classes" {
            let actual =
                parseJsonSchema nestedArrayWithObjectItems
                |> jsonSchemaTypeToFSharpType
                |> normalizeClassIds

            let expected =
                FSharpClass(
                    Guid.Empty,
                    [ ("values",
                       { Required = false },
                       FSharpList(
                           FSharpList(
                               FSharpClass(
                                   Guid.Empty,
                                   [ ("propA", { Required = false }, FSharpInt jsonIntegerNoneKeywords)
                                     ("propB", { Required = false }, FSharpString) ]
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
