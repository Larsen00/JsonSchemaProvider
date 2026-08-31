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

    // Characterizes the current gap discussed in notes/: nested classes' own Create skips
    // schema.Validate entirely (ExprGenerator.fs's generateCreateInvokeCode only validates when
    // nestedClass = false). Calling the nested class's Create directly, without ever going
    // through the root, bypasses enforcement completely today. This test documents that gap as
    // it exists NOW - it should start failing (in a good way) once the ValidatedTypes fallback
    // lands and starts checking per-property regardless of nesting.
    let nestedCreateDoesNotValidateConstraintsYet =
        test "KNOWN GAP: nested class Create does not enforce constraints when called directly" {
            let person = NestedAge.personObj.Create(age = 3)
            Expect.equal person.age 3 "nested Create accepted an out-of-range value without raising"
        }

    // But the ROOT's Create still catches it, because schema.Validate checks the whole
    // assembled document against the whole schema in one pass - even though the nested Create
    // that built `person` didn't check anything itself.
    let rootCreateCatchesInvalidValueInsideNestedObject =
        test "root Create rejects an out-of-range value nested inside a sub-object" {
            let invalidPerson = NestedAge.personObj.Create(age = 3)
            Expect.throws
                (fun () -> NestedAge.Create(person = invalidPerson) |> ignore)
                "root Create should reject the whole document when a nested property is invalid"
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
              nestedCreateDoesNotValidateConstraintsYet
              rootCreateCatchesInvalidValueInsideNestedObject ]
