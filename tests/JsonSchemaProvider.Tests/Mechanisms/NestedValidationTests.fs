namespace JsonSchemaProvider.Tests

// Every generated nested class resolves and validates against its own sub-schema by Path, not
// just when reached through the root's Create. minimum/maximum is the vehicle; the point is the
// resolution mechanism (nesting depth, siblings, array items), not integer-range coverage itself
// (see Keywords/NumberKeywordTests.fs for that).
module NestedValidationTests =
    open Expecto
    open JsonSchemaProvider

    [<Literal>]
    let ageSchema = """{ "type": "object", "properties": { "age": { "type": "integer", "minimum": 5, "maximum": 10 } }, "required": ["age"] }"""
    type Age = JsonSchemaProvider<schema=ageSchema>

    let validRecordShouldBeCreated =
        test "in-range value is accepted by Create" {
            let record = Expect.wantOk (Age.Create(age = 7)) "Create should succeed"
            Expect.equal record.age 7 "record.age = 7"
        }

    let validRecordShouldBeParsed =
        test "in-range value is accepted by Parse" {
            let record = Expect.wantOk (Age.Parse("""{"age": 7}""")) "Parse should succeed"
            Expect.equal record.age 7 "record.age = 7"
        }

    let belowMinimumShouldBeRejectedByCreate =
        test "below-minimum value is rejected by root Create" {
            Expect.isError (Age.Create(age = 3)) "below minimum"
        }

    let aboveMaximumShouldBeRejectedByCreate =
        test "above-maximum value is rejected by root Create" {
            Expect.isError (Age.Create(age = 20)) "above maximum"
        }

    let belowMinimumShouldBeRejectedByParse =
        test "below-minimum value is rejected by Parse" {
            Expect.isError (Age.Parse("""{"age": 3}""")) "below minimum"
        }

    // Same constraint nested one level down, so a nested class's own Create can be compared
    // against root-level enforcement.
    [<Literal>]
    let nestedAgeSchema =
        """{ "type": "object", "properties": { "person": { "type": "object", "properties": { "age": { "type": "integer", "minimum": 5, "maximum": 10 } }, "required": ["age"] } }, "required": ["person"] }"""
    type NestedAge = JsonSchemaProvider<schema=nestedAgeSchema>

    let nestedCreateValidatesConstraints =
        test "nested class Create enforces constraints when called directly" {
            Expect.isError (NestedAge.personObj.Create(age = 3)) "below minimum, enforced on its own"
        }

    let nestedCreateAcceptsInRangeValue =
        test "nested class Create accepts an in-range value when called directly" {
            let person = Expect.wantOk (NestedAge.personObj.Create(age = 7)) "nested Create should succeed"
            Expect.equal person.age 7 "in range"
        }

    // Two levels of nesting, so a Path like "#/properties/person/properties/address/properties/zip"
    // is exercised, not just the one-level-deep case above.
    [<Literal>]
    let twoLevelNestedSchema =
        """{ "type": "object", "properties": { "person": { "type": "object", "properties": { "address": { "type": "object", "properties": { "zip": { "type": "integer", "minimum": 1000, "maximum": 9999 } }, "required": ["zip"] } }, "required": ["address"] } }, "required": ["person"] }"""
    type TwoLevelNested = JsonSchemaProvider<schema=twoLevelNestedSchema>

    let twoLevelNestedCreateRejectsOutOfRange =
        test "a class nested two levels deep enforces its own constraints when called directly" {
            Expect.isError (TwoLevelNested.personObj.addressObj.Create(zip = 500)) "below minimum"
        }

    let twoLevelNestedCreateAcceptsInRange =
        test "a class nested two levels deep accepts an in-range value" {
            let address = Expect.wantOk (TwoLevelNested.personObj.addressObj.Create(zip = 5000)) "nested Create should succeed"
            Expect.equal address.zip 5000 "in range"
        }

    // Two siblings with DIFFERENT ranges - if Path-based resolution ever picked the wrong
    // sibling's sub-schema, a value valid for one but not the other would slip through.
    [<Literal>]
    let siblingConstraintsSchema =
        """
        {
          "type": "object",
          "properties": {
            "small": { "type": "object", "properties": { "value": { "type": "integer", "minimum": 0, "maximum": 10 } }, "required": ["value"] },
            "large": { "type": "object", "properties": { "value": { "type": "integer", "minimum": 100, "maximum": 200 } }, "required": ["value"] }
          },
          "required": ["small", "large"]
        }"""
    type SiblingConstraints = JsonSchemaProvider<schema=siblingConstraintsSchema>

    let siblingNestedClassesValidateAgainstTheirOwnConstraints =
        test "two sibling nested classes each validate against their own sub-schema, not each other's" {
            let smallValue = Expect.wantOk (SiblingConstraints.smallObj.Create(value = 5)) "small Create should succeed"
            Expect.equal smallValue.value 5 "within small's own range"

            Expect.isError (SiblingConstraints.smallObj.Create(value = 150)) "outside small's range, even though within large's"

            let largeValue = Expect.wantOk (SiblingConstraints.largeObj.Create(value = 150)) "large Create should succeed"
            Expect.equal largeValue.value 150 "within large's own range"

            Expect.isError (SiblingConstraints.largeObj.Create(value = 5)) "outside large's range, even though within small's"
        }

    // A class reached through an array item, not a plain property.
    [<Literal>]
    let arrayItemConstraintsSchema =
        """{ "type": "object", "properties": { "scores": { "type": "array", "items": { "type": "object", "properties": { "points": { "type": "integer", "minimum": 0, "maximum": 100 } }, "required": ["points"] } } }, "required": ["scores"] }"""
    type ArrayItemConstraints = JsonSchemaProvider<schema=arrayItemConstraintsSchema>

    let arrayItemNestedClassValidatesConstraints =
        test "a class reached through an array item's own Create enforces constraints" {
            Expect.isError (ArrayItemConstraints.scoresItem.Create(points = 150)) "above maximum"
            let item = Expect.wantOk (ArrayItemConstraints.scoresItem.Create(points = 50)) "nested Create should succeed"
            Expect.equal item.points 50 "within range"
        }

    [<Tests>]
    let tests =
        testList
            "JsonSchemaProvider.Tests.NestedValidationTests"
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
