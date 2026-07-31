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

    let nestedArrayWithObjectItemsShouldBeParsedCorrectly =
        test "NestedArrayWithObjectItems should be parsed correctly" {
            let actual = parseJsonSchema nestedArrayWithObjectItems

            let expected =
                JsonObject(
                    [ { Name = "values"
                        Optional = true
                        PropertyType =
                          JsonArray(
                              JsonArray(
                                  JsonObject(

                                      [ { Name = "propA"
                                          Optional = true
                                          PropertyType = JsonInteger }
                                        { Name = "propB"
                                          Optional = true
                                          PropertyType = JsonString } ]
                                  ),{ MinItems = None}
                              ), { MinItems = None}
                          ) } ]
                )

            Expect.equal actual expected ""

        }

    let nestedObjectsShouldBeClassTreeWithFourClasses =
        test "NestedObjects should be class tree with four classes" {
            let actual =
                parseJsonSchema nestedObjects |> jsonObjectToFSharpClass "NestedObjects"

            let expected =
                FSharpClass(
                    "NestedObjects",
                    [ { Name = "header"
                        Optional = true
                        FSharpType =
                          FSharpClass(
                              "header",
                              [ { Name = "id"
                                  Optional = false
                                  FSharpType = FSharpInt }
                                { Name = "sender"
                                  Optional = false
                                  FSharpType = FSharpString }
                                { Name = "resend"
                                  Optional = true
                                  FSharpType = FSharpBool }
                                { Name = "time"
                                  Optional = true
                                  FSharpType =
                                    FSharpClass(
                                        "time",
                                        [ { Name = "hour"
                                            Optional = false
                                            FSharpType = FSharpInt }
                                          { Name = "minute"
                                            Optional = false
                                            FSharpType = FSharpInt }
                                          { Name = "second"
                                            Optional = false
                                            FSharpType = FSharpInt } ]
                                    ) } ]
                          ) }
                      { Name = "body"
                        Optional = false
                        FSharpType =
                          FSharpClass(
                              "body",
                              [ { Name = "length"
                                  Optional = false
                                  FSharpType = FSharpInt }
                                { Name = "payload"
                                  Optional = false
                                  FSharpType = FSharpString } ]
                          ) } ]
                )

            Expect.equal actual expected ""
        }

    let nestedArrayWithObjectItemsShouldBeClassTreeWithTwoClasses =
        test "NestedArrayWithObjectItems should be vlass tree with two classes" {
            let actual =
                parseJsonSchema nestedArrayWithObjectItems
                |> jsonObjectToFSharpClass "NestedArrayWithObjectItems"

            let expected =
                FSharpClass(
                    "NestedArrayWithObjectItems",
                    [ { Name = "values"
                        Optional = true
                        FSharpType =
                          FSharpList(
                              FSharpList(
                                  FSharpClass(
                                      "values",
                                      [ { Name = "propA"
                                          Optional = true
                                          FSharpType = FSharpInt }
                                        { Name = "propB"
                                          Optional = true
                                          FSharpType = FSharpString } ]
                                  ),
                                  { MinItems = None }
                              ),
                              { MinItems = None }
                          ) } ]
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
