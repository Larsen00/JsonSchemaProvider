namespace JsonSchemaProvider.Tests

module JsonSchemaProviderTests =
    open Expecto
    open JsonSchemaProvider

    [<Literal>]
    let flatSchema =
        """
        {
          "type": "object",
          "properties": {
            "X": {
            "type": "string"
            },
            "Y": {
            "type": "string"
            },
            "Z": {
            "type": "integer"
            },
          }
        }"""

    // Isolates generateIsNullCheck's FSharpBool/FSharpDouble branches specifically - flatSchema's
    // optional Z:integer already exercises the FSharpInt branch (both present and absent), but
    // nothing else in this suite has an *optional* (non-required) boolean or number property, so
    // those two branches - each its own separate pattern match arm, not shared code - had no
    // coverage at all.
    [<Literal>]
    let optionalPrimitivesSchema =
        """
        {
          "type": "object",
          "properties": {
            "flag": {
              "type": "boolean"
            },
            "amount": {
              "type": "number"
            }
          }
        }"""

    [<Literal>]
    let requiredPropertiesSchema =
        """
        {
          "type": "object",
          "properties": {
            "X": {
              "type": "string"
            },
            "Y": {
              "type": "string"
            },
            "Z": {
              "type": "integer"
            },
          },
          "required": ["X", "Y", "Z"]
        }"""

    [<Literal>]
    let patternSchema =
        """
        {
          "type": "object",
          "properties": {
            "X": {
              "type": "string",
              "pattern": "^[a-z]+$"
            }
          }
        }"""

    [<Literal>]
    let cityPosition =
        """
        {
          "type": "object",
          "properties": {
            "city": {"type": "string"},
            "globalPosition": {
              "type": "object",
              "properties": {
                "lat": {"type": "number"},
                "lon": {"type": "number"}
              },
              "required": ["lat", "lon"]
            }
          },
          "required": ["city", "globalPosition"]
        }"""

    [<Literal>]
    let numberArray =
        """
        {
          "type": "object",
          "properties": {
            "values": {
              "type": "array",
              "items": {"type": "number"}
            }
          },
          "required": ["values"]
        }"""

    [<Literal>]
    let integerArray =
        """
        {
          "type": "object",
          "properties": {
            "values": {
              "type": "array",
              "items": {"type": "integer"}
            }
          },
          "required": ["values"]
        }"""

    [<Literal>]
    let nestedArray =
        """
        {
          "type": "object",
          "properties": {
            "values": {
              "type": "array",
              "items": {
                "type": "array",
                "items": {"type": "string"}
              }
            }
          },
          "required": ["values"]
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

    type Flat = JsonSchemaProvider<schema=flatSchema>
    type OptionalPrimitives = JsonSchemaProvider<schema=optionalPrimitivesSchema>
    type RequiredProperties = JsonSchemaProvider<schema=requiredPropertiesSchema>
    type PatternSchema = JsonSchemaProvider<schema=patternSchema>
    type CityPosition = JsonSchemaProvider<schema=cityPosition>
    type NumberArray = JsonSchemaProvider<schema=numberArray>
    type IntegerArray = JsonSchemaProvider<schema=integerArray>
    type NestedArray = JsonSchemaProvider<schema=nestedArray>
    type NestedArrayWithObjectItems = JsonSchemaProvider<schema=nestedArrayWithObjectItems>

    let validRecordShouldBeParsed =
        test "valid record should be parsed" {
            let flat = Flat.Parse("""{"X": "x", "Z": 1}""")
            Expect.equal flat.X (Some("x")) """flat.X = Some("x")"""
            Expect.equal flat.Y None """flat.Y = None"""
            Expect.equal flat.Z (Some(1)) "flat.Z = Some(1)"
        }

    let createMethodShouldReturnRecord =
        test "create method should return record" {
            let flat = Flat.Create(X = "x", Z = 1)
            Expect.equal flat.X (Some("x")) """flat.X = Some("x")"""
            Expect.equal flat.Y None """flat.Y = None"""
            Expect.equal flat.Z (Some(1)) "flat.Z = Some(1)"
        }

    let optionalBoolAndNumberPresentRoundTrip =
        test "optional boolean and number properties round-trip when present" {
            let v = OptionalPrimitives.Create(flag = true, amount = 1.5)
            Expect.equal v.flag (Some true) "flag = Some true"
            Expect.equal v.amount (Some 1.5) "amount = Some 1.5"
        }

    let optionalBoolAndNumberAbsentGiveNone =
        test "optional boolean and number properties give None when absent" {
            let v = OptionalPrimitives.Create()
            Expect.equal v.flag None "flag = None"
            Expect.equal v.amount None "amount = None"
        }

    let requiredPropertiesShouldNotBeParsedIntoOption =
        test "required properties should not be parsed into Option" {
            let requiredProperties =
                RequiredProperties.Parse("""{"X": "x", "Y": "y", "Z": 1}""")

            Expect.equal requiredProperties.X "x" """requiredProperties.X = "x" """
            Expect.equal requiredProperties.Y "y" """requiredProperties.Y = "y" """
            Expect.equal requiredProperties.Z 1 "flat.Z = 1"
        }

    let validationErrorShouldBeDetectedByCreate =
        test "validation error should be detected by Create" {
            Expect.isError (PatternSchema.Create(X = "a1")) "Create should return Error for invalid pattern"
        }

    let validationErrorShouldBeDetectedByParse =
        test "validation error should be detected by Parse" {
            Expect.throws
                (fun _ -> PatternSchema.Parse("""{"X": "a1"}""") |> ignore)
                "Parse throws validation exception"
        }

    let valueFromNestedObjectsShouldBeCreated =
        test "value from nested objects should be created" {
            let globalPosition = CityPosition.globalPositionObj.Create(lat = 52.520007, lon = 13.404954)

            let created = CityPosition.Create(city = "Berlin", globalPosition = globalPosition)

            Expect.equal created.globalPosition.lat 52.520007 "create and select nested are equal"
        }

    let selectFromNumberArrayShouldYieldInputValue =
        let numArray = NumberArray.Create([ 11.0; 12.0; 11.6; 12.1 ])

        test "select from number array should yield input value" {
            Expect.equal numArray.values[1] 12.0 "numArray.values[1] = 12.0"
        }

    let selectFromIntegerArrayShouldYieldInputValue =
        let numArray = IntegerArray.Create([ 11; 12; 10; 13 ])

        test "select from integer array should yield input value" {
            Expect.equal numArray.values[1] 12 "numArray.values[1] = 12"
        }

    let selectFromNestedArrayShouldYieldInputValue =
        let array = NestedArray.Create([ [ "a"; "b" ] ])

        test "select from nested array should yield input value" {
            Expect.equal (array.values[0][1]) "b" "nestedArray.values[0][1] = \"b\""
        }

    let selectFromNestedArrayWithObjectItemsShouldYieldInputValue =
        let item = NestedArrayWithObjectItems.valuesItem.Create(propA = 5)
        let array = NestedArrayWithObjectItems.Create([ [ item ] ])

        test "select from nested array with object items should yield input value" {
            Expect.equal
                (array.values.Value[0][0]).propA
                (Some(5))
                "nestedArrayWithObjectItems.values.Value[0][0]).propA = 5"
        }

    [<Tests>]
    let tests =
        testList
            "JsonSchemaProvider.Tests.JsonSchemaProviderTests"
            [ validRecordShouldBeParsed
              createMethodShouldReturnRecord
              optionalBoolAndNumberPresentRoundTrip
              optionalBoolAndNumberAbsentGiveNone
              requiredPropertiesShouldNotBeParsedIntoOption
              validationErrorShouldBeDetectedByCreate
              validationErrorShouldBeDetectedByParse
              valueFromNestedObjectsShouldBeCreated
              selectFromNumberArrayShouldYieldInputValue
              selectFromIntegerArrayShouldYieldInputValue
              selectFromNestedArrayShouldYieldInputValue
              selectFromNestedArrayWithObjectItemsShouldYieldInputValue ]
