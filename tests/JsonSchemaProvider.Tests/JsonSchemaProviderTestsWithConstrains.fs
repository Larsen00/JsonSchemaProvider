namespace JsonSchemaProvider.Tests

module JsonSchemaProviderTestsWithConstrains =
    open Expecto
    open JsonSchemaProvider

    [<Literal>]
    let ageSchema =
        """
        {
          "type": "object",
          "properties": {
            "age": { "type": "integer", "minimum": 5, "maximum": 10 }
          },
          "required": ["age"]
        }"""

    type Age = JsonSchemaProvider<schema=ageSchema>

    // Nests the same constrained property one level down, so we can compare root-level
    // enforcement against a nested class's Create called directly (see
    // notes/wiring-validatedtypes-into-jsonschemaprovider.md's open question, and the
    // baseline-behavior discussion in notes/ around schema.Validate).
    [<Literal>]
    let nestedAgeSchema =
        """
        {
          "type": "object",
          "properties": {
            "person": {
              "type": "object",
              "properties": {
                "age": { "type": "integer", "minimum": 5, "maximum": 10 }
              },
              "required": ["age"]
            }
          },
          "required": ["person"]
        }"""

    type NestedAge = JsonSchemaProvider<schema=nestedAgeSchema>

    // Two levels of nesting, so a Path like "#/properties/person/properties/address/properties/zip"
    // is actually exercised, not just the one-level-deep "#/properties/person" case above.
    [<Literal>]
    let twoLevelNestedSchema =
        """
        {
          "type": "object",
          "properties": {
            "person": {
              "type": "object",
              "properties": {
                "address": {
                  "type": "object",
                  "properties": {
                    "zip": { "type": "integer", "minimum": 1000, "maximum": 9999 }
                  },
                  "required": ["zip"]
                }
              },
              "required": ["address"]
            }
          },
          "required": ["person"]
        }"""

    type TwoLevelNested = JsonSchemaProvider<schema=twoLevelNestedSchema>

    // Two sibling nested classes with DIFFERENT ranges. If Path-based resolution ever picked the
    // wrong sibling's sub-schema (e.g. a stale/shared cache entry, or a path collision), a value
    // valid for one but not the other would slip through incorrectly in at least one direction.
    [<Literal>]
    let siblingConstraintsSchema =
        """
        {
          "type": "object",
          "properties": {
            "small": {
              "type": "object",
              "properties": { "value": { "type": "integer", "minimum": 0, "maximum": 10 } },
              "required": ["value"]
            },
            "large": {
              "type": "object",
              "properties": { "value": { "type": "integer", "minimum": 100, "maximum": 200 } },
              "required": ["value"]
            }
          },
          "required": ["small", "large"]
        }"""

    type SiblingConstraints = JsonSchemaProvider<schema=siblingConstraintsSchema>

    // A class reached through an array item, not a plain property - exercises
    // buildClassMapHelper's FSharpList unwrapping combined with Path resolution.
    [<Literal>]
    let arrayItemConstraintsSchema =
        """
        {
          "type": "object",
          "properties": {
            "scores": {
              "type": "array",
              "items": {
                "type": "object",
                "properties": {
                  "points": { "type": "integer", "minimum": 0, "maximum": 100 }
                },
                "required": ["points"]
              }
            }
          },
          "required": ["scores"]
        }"""

    type ArrayItemConstraints = JsonSchemaProvider<schema=arrayItemConstraintsSchema>

    let validRecordShouldBeCreated =
        test "in-range value is accepted by Create" {
            let record = Age.Create(age = 7)
            Expect.equal record.age 7 "record.age = 7"
        }

    let validRecordShouldBeParsed =
        test "in-range value is accepted by Parse" {
            let record = Age.Parse("""{"age": 7}""")
            Expect.equal record.age 7 "record.age = 7"
        }

    let belowMinimumShouldBeRejectedByCreate =
        test "below-minimum value is rejected by root Create" {
            Expect.throws (fun () -> Age.Create(age = 3) |> ignore) "Create should reject age below minimum"
        }

    let aboveMaximumShouldBeRejectedByCreate =
        test "above-maximum value is rejected by root Create" {
            Expect.throws (fun () -> Age.Create(age = 20) |> ignore) "Create should reject age above maximum"
        }

    let belowMinimumShouldBeRejectedByParse =
        test "below-minimum value is rejected by Parse" {
            Expect.throws
                (fun () -> Age.Parse("""{"age": 3}""") |> ignore)
                "Parse should reject age below minimum"
        }

    // Gap closed: generateCreateInvokeCode now resolves each nested class's own sub-schema via
    // Path (see notes/nested-create-subschema-resolution.md) and validates against it directly,
    // instead of only validating when nestedClass = false. Calling a nested class's Create
    // directly, without ever going through the root, is now enforced on its own.
    let nestedCreateValidatesConstraints =
        test "nested class Create enforces constraints when called directly" {
            Expect.throws
                (fun () -> NestedAge.personObj.Create(age = 3) |> ignore)
                "nested Create should reject age below minimum on its own"
        }

    let nestedCreateAcceptsInRangeValue =
        test "nested class Create accepts an in-range value when called directly" {
            let person = NestedAge.personObj.Create(age = 7)
            Expect.equal person.age 7 "nested Create should accept age within range"
        }

    let twoLevelNestedCreateRejectsOutOfRange =
        test "a class nested two levels deep enforces its own constraints when called directly" {
            Expect.throws
                (fun () -> TwoLevelNested.personObj.addressObj.Create(zip = 500) |> ignore)
                "two-levels-deep nested Create should reject zip below minimum"
        }

    let twoLevelNestedCreateAcceptsInRange =
        test "a class nested two levels deep accepts an in-range value" {
            let address = TwoLevelNested.personObj.addressObj.Create(zip = 5000)
            Expect.equal address.zip 5000 "two-levels-deep nested Create should accept zip within range"
        }

    let siblingNestedClassesValidateAgainstTheirOwnConstraints =
        test "two sibling nested classes each validate against their own sub-schema, not each other's" {
            let smallValue = SiblingConstraints.smallObj.Create(value = 5)
            Expect.equal smallValue.value 5 "small.value=5 is within small's own range"

            Expect.throws
                (fun () -> SiblingConstraints.smallObj.Create(value = 150) |> ignore)
                "small.value=150 is outside small's own range, even though it's within large's"

            let largeValue = SiblingConstraints.largeObj.Create(value = 150)
            Expect.equal largeValue.value 150 "large.value=150 is within large's own range"

            Expect.throws
                (fun () -> SiblingConstraints.largeObj.Create(value = 5) |> ignore)
                "large.value=5 is outside large's own range, even though it's within small's"
        }

    let arrayItemNestedClassValidatesConstraints =
        test "a class reached through an array item's own Create enforces constraints" {
            Expect.throws
                (fun () -> ArrayItemConstraints.scoresObj.Create(points = 150) |> ignore)
                "array-item nested Create should reject points above maximum"

            let item = ArrayItemConstraints.scoresObj.Create(points = 50)
            Expect.equal item.points 50 "array-item nested Create should accept points within range"
        }

    [<Tests>]
    let tests =
        testList
            "JsonSchemaProvider.Tests.JsonSchemaProviderTestsWithConstrains"
            [ validRecordShouldBeCreated
              validRecordShouldBeParsed
              belowMinimumShouldBeRejectedByCreate
              aboveMaximumShouldBeRejectedByCreate
              belowMinimumShouldBeRejectedByParse
              nestedCreateValidatesConstraints
              nestedCreateAcceptsInRangeValue
              twoLevelNestedCreateRejectsOutOfRange
              twoLevelNestedCreateAcceptsInRange
              siblingNestedClassesValidateAgainstTheirOwnConstraints
              arrayItemNestedClassValidatesConstraints ]
