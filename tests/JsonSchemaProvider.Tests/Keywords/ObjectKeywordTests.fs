namespace JsonSchemaProvider.Tests

module ObjectKeywordTests =
    open Expecto
    open JsonSchemaProvider

    // ---- properties / required / optional round-tripping ----

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

    type Flat = JsonSchemaProvider<schema=flatSchema>
    type OptionalPrimitives = JsonSchemaProvider<schema=optionalPrimitivesSchema>
    type RequiredProperties = JsonSchemaProvider<schema=requiredPropertiesSchema>
    type CityPosition = JsonSchemaProvider<schema=cityPosition>

    let validRecordShouldBeParsed =
        test "valid record should be parsed" {
            let flat = Expect.wantOk (Flat.Parse("""{"X": "x", "Z": 1}""")) "Parse should succeed"
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
                Expect.wantOk (RequiredProperties.Parse("""{"X": "x", "Y": "y", "Z": 1}""")) "Parse should succeed"

            Expect.equal requiredProperties.X "x" """requiredProperties.X = "x" """
            Expect.equal requiredProperties.Y "y" """requiredProperties.Y = "y" """
            Expect.equal requiredProperties.Z 1 "flat.Z = 1"
        }

    let valueFromNestedObjectsShouldBeCreated =
        test "value from nested objects should be created" {
            let globalPosition = CityPosition.globalPositionObj.Create(lat = 52.520007, lon = 13.404954)

            let created = CityPosition.Create(city = "Berlin", globalPosition = globalPosition)

            Expect.equal created.globalPosition.lat 52.520007 "create and select nested are equal"
        }

    // ---- minProperties / maxProperties / additionalProperties / patternProperties ----
    // Same point as NumberKeywordTests/StringKeywordTests: validation runs via NJsonSchema's own
    // schema.Validate against the real schema text, independent of which keywords
    // SchemaConversion.fs's FSharpType model happens to act on - so it enforces keywords the
    // type-level model doesn't represent at all (minProperties/maxProperties/patternProperties).

    // All three properties optional and unconstrained individually - minProperties/maxProperties
    // are the only thing that can reject or accept here, isolating them from every other keyword.
    [<Literal>]
    let minPropertiesSchema =
        """
        { "type": "object", "properties": { "a": {"type": "integer"}, "b": {"type": "integer"}, "c": {"type": "integer"} }, "minProperties": 2 }"""

    type MinPropertiesObject = JsonSchemaProvider<schema=minPropertiesSchema>

    [<Literal>]
    let maxPropertiesSchema =
        """
        { "type": "object", "properties": { "a": {"type": "integer"}, "b": {"type": "integer"}, "c": {"type": "integer"} }, "maxProperties": 2 }"""

    type MaxPropertiesObject = JsonSchemaProvider<schema=maxPropertiesSchema>

    // additionalProperties/patternProperties can never be violated through Create - its parameter
    // list is fixed by the schema's own declared properties, so there's no way to pass an "extra"
    // named argument. Both are only reachable through Parse, which takes raw, unconstrained JSON
    // text. This exercises Parse's pre-existing validation (see baseline-validation-behavior.md
    // finding 1 - root Parse already validated before any of today's work), not the nested/
    // primitive-root fix itself - included here for keyword-category completeness regardless.
    [<Literal>]
    let additionalPropertiesSchema =
        """
        { "type": "object", "properties": { "a": {"type": "integer"} }, "additionalProperties": false }"""

    type AdditionalPropertiesObject = JsonSchemaProvider<schema=additionalPropertiesSchema>

    [<Literal>]
    let patternPropertiesSchema =
        """
        { "type": "object", "patternProperties": { "^S_": {"type": "string"} }, "additionalProperties": false }"""

    type PatternPropertiesObject = JsonSchemaProvider<schema=patternPropertiesSchema>

    let tooFewPropertiesAreRejected =
        test "minProperties rejects an object built with too few properties set" {
            Expect.isError
                (MinPropertiesObject.Create(a = 1))
                "only 1 property set is below minProperties=2"
        }

    let enoughPropertiesAreAccepted =
        test "minProperties accepts an object with enough properties set" {
            let result = Expect.wantOk (MinPropertiesObject.Create(a = 1, b = 2)) "Create should succeed"
            Expect.equal (result.a, result.b) (Some 1, Some 2) "2 properties set satisfies minProperties=2"
        }

    let tooManyPropertiesAreRejected =
        test "maxProperties rejects an object built with too many properties set" {
            Expect.isError
                (MaxPropertiesObject.Create(a = 1, b = 2, c = 3))
                "3 properties set exceeds maxProperties=2"
        }

    let withinMaxPropertiesIsAccepted =
        test "maxProperties accepts an object within the limit" {
            let result = Expect.wantOk (MaxPropertiesObject.Create(a = 1, b = 2)) "Create should succeed"
            Expect.equal (result.a, result.b) (Some 1, Some 2) "2 properties set is within maxProperties=2"
        }

    let additionalPropertyIsRejectedByParse =
        test "additionalProperties=false rejects an undeclared property via Parse" {
            Expect.isError
                (AdditionalPropertiesObject.Parse("""{"a": 1, "extra": true}"""))
                "\"extra\" isn't declared and additionalProperties is false"
        }

    let onlyDeclaredPropertyIsAcceptedByParse =
        test "additionalProperties=false accepts an object with only declared properties via Parse" {
            let result = Expect.wantOk (AdditionalPropertiesObject.Parse("""{"a": 1}""")) "Parse should succeed"
            Expect.equal result.a (Some 1) "only declared properties present"
        }

    let nonMatchingPropertyNameIsRejectedByParse =
        test "patternProperties rejects a property name that matches neither the pattern nor additionalProperties" {
            Expect.isError
                (PatternPropertiesObject.Parse("""{"S_x": "ok", "other": 1}"""))
                "\"other\" doesn't match the S_ pattern and additionalProperties is false"
        }

    let matchingPatternPropertyIsAcceptedByParse =
        test "patternProperties accepts a property name matching the pattern" {
            Expect.isOk (PatternPropertiesObject.Parse("""{"S_x": "ok"}""")) "Parse should succeed"
        }

    [<Tests>]
    let tests =
        testList
            "JsonSchemaProvider.Tests.ObjectKeywordTests"
            [ validRecordShouldBeParsed
              createMethodShouldReturnRecord
              optionalBoolAndNumberPresentRoundTrip
              optionalBoolAndNumberAbsentGiveNone
              requiredPropertiesShouldNotBeParsedIntoOption
              valueFromNestedObjectsShouldBeCreated
              tooFewPropertiesAreRejected
              enoughPropertiesAreAccepted
              tooManyPropertiesAreRejected
              withinMaxPropertiesIsAccepted
              additionalPropertyIsRejectedByParse
              onlyDeclaredPropertyIsAcceptedByParse
              nonMatchingPropertyNameIsRejectedByParse
              matchingPatternPropertyIsAcceptedByParse ]
