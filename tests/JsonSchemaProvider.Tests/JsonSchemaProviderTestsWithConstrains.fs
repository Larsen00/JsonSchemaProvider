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

    [<Tests>]
    let tests =
        testList
            "JsonSchemaProvider.Tests.JsonSchemaProviderTestsWithConstrains"
            [ validRecordShouldBeCreated
              validRecordShouldBeParsed
              belowMinimumShouldBeRejectedByCreate
              aboveMaximumShouldBeRejectedByCreate
              belowMinimumShouldBeRejectedByParse
              nestedCreateValidatesConstraints ]
