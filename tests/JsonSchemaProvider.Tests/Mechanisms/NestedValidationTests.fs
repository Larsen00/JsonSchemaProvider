namespace JsonSchemaProvider.Tests

// Every generated nested class resolves and validates against its own sub-schema by Path, not
// just when reached through the root's Create. minimum/maximum is the vehicle; the point is the
// resolution mechanism (nesting depth, siblings, array items), not integer-range coverage itself
// (see Keywords/NumberKeywordTests.fs for that).
module NestedValidationTests =
    open Xunit
    open JsonSchemaProvider

    // #region create-usage
    [<Literal>]
    let ageSchema = """
    {
        "type": "object",
        "properties": {
            "age": {
                "type": "integer",
                "minimum": 5,
                "maximum": 10
            }
        },
        "required": ["age"]
    }
    """

    type Age = JsonSchemaProvider<schema=ageSchema>

    let message =
        match Age.Create(age = 7) with
        | Ok person -> sprintf "Age: %d" person.age
        | Error errors -> sprintf "Invalid: %A" errors
    // #endregion

    [<Fact>]
    let ``user-style match on Create takes the Ok arm for an in-range value`` () =
        expectEqual message "Age: 7"

    [<Fact>]
    let ``in-range value is accepted by Create`` () =
        let record = expectOk (Age.Create(age = 7))
        expectEqual record.age 7

    [<Fact>]
    let ``in-range value is accepted by Parse`` () =
        let record = expectOk (Age.Parse("""{"age": 7}"""))
        expectEqual record.age 7

    [<Fact>]
    let ``below-minimum value is rejected by root Create`` () =
        expectError (Age.Create(age = 3)) // below minimum

    [<Fact>]
    let ``above-maximum value is rejected by root Create`` () =
        expectError (Age.Create(age = 20)) // above maximum

    [<Fact>]
    let ``below-minimum value is rejected by Parse`` () =
        expectError (Age.Parse("""{"age": 3}""")) // below minimum

    // Same constraint nested one level down, so a nested class's own Create can be compared
    // against root-level enforcement.
    [<Literal>]
    let nestedAgeSchema =
        """{ "type": "object", "properties": { "person": { "type": "object", "properties": { "age": { "type": "integer", "minimum": 5, "maximum": 10 } }, "required": ["age"] } }, "required": ["person"] }"""
    type NestedAge = JsonSchemaProvider<schema=nestedAgeSchema>

    [<Fact>]
    let ``nested class Create enforces constraints when called directly`` () =
        expectError (NestedAge.personObj.Create(age = 3)) // below minimum, enforced on its own

    [<Fact>]
    let ``nested class Create accepts an in-range value when called directly`` () =
        let person = expectOk (NestedAge.personObj.Create(age = 7))
        expectEqual person.age 7

    // Two levels of nesting, so a Path like "#/properties/person/properties/address/properties/zip"
    // is exercised, not just the one-level-deep case above.
    [<Literal>]
    let twoLevelNestedSchema =
        """{ "type": "object", "properties": { "person": { "type": "object", "properties": { "address": { "type": "object", "properties": { "zip": { "type": "integer", "minimum": 1000, "maximum": 9999 } }, "required": ["zip"] } }, "required": ["address"] } }, "required": ["person"] }"""
    type TwoLevelNested = JsonSchemaProvider<schema=twoLevelNestedSchema>

    [<Fact>]
    let ``a class nested two levels deep enforces its own constraints when called directly`` () =
        expectError (TwoLevelNested.personObj.addressObj.Create(zip = 500)) // below minimum

    [<Fact>]
    let ``a class nested two levels deep accepts an in-range value`` () =
        let address = expectOk (TwoLevelNested.personObj.addressObj.Create(zip = 5000))
        expectEqual address.zip 5000

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

    [<Fact>]
    let ``two sibling nested classes each validate against their own sub-schema, not each other's`` () =
        let smallValue = expectOk (SiblingConstraints.smallObj.Create(value = 5))
        expectEqual smallValue.value 5

        expectError (SiblingConstraints.smallObj.Create(value = 150)) // outside small's range, even though within large's

        let largeValue = expectOk (SiblingConstraints.largeObj.Create(value = 150))
        expectEqual largeValue.value 150

        expectError (SiblingConstraints.largeObj.Create(value = 5)) // outside large's range, even though within small's

    // A class reached through an array item, not a plain property.
    [<Literal>]
    let arrayItemConstraintsSchema =
        """{ "type": "object", "properties": { "scores": { "type": "array", "items": { "type": "object", "properties": { "points": { "type": "integer", "minimum": 0, "maximum": 100 } }, "required": ["points"] } } }, "required": ["scores"] }"""
    type ArrayItemConstraints = JsonSchemaProvider<schema=arrayItemConstraintsSchema>

    [<Fact>]
    let ``a class reached through an array item's own Create enforces constraints`` () =
        expectError (ArrayItemConstraints.scoresItem.Create(points = 150)) // above maximum
        let item = expectOk (ArrayItemConstraints.scoresItem.Create(points = 50))
        expectEqual item.points 50

    // Parse on nested classes: each must validate against its own sub-schema, like Create above.
    [<Fact>]
    let ``nested class Parse accepts an in-range value when called directly`` () =
        let person = expectOk (NestedAge.personObj.Parse("""{"age": 7}"""))
        expectEqual person.age 7

    [<Fact>]
    let ``nested class Parse enforces constraints when called directly`` () =
        expectError (NestedAge.personObj.Parse("""{"age": 3}""")) // below minimum, enforced on its own

    [<Fact>]
    let ``nested class Parse enforces its own required list`` () =
        expectError (NestedAge.personObj.Parse("""{}""")) // age is required by the nested sub-schema

    [<Fact>]
    let ``nested class Parse reports malformed JSON as Error`` () =
        expectError (NestedAge.personObj.Parse("""{"age": """)) // syntax check runs on nested Parse too

    [<Fact>]
    let ``a class nested two levels deep validates its own constraints in Parse`` () =
        expectError (TwoLevelNested.personObj.addressObj.Parse("""{"zip": 500}""")) // below minimum
        let address = expectOk (TwoLevelNested.personObj.addressObj.Parse("""{"zip": 5000}"""))
        expectEqual address.zip 5000

    [<Fact>]
    let ``two sibling nested classes each validate against their own sub-schema in Parse`` () =
        let smallValue = expectOk (SiblingConstraints.smallObj.Parse("""{"value": 5}"""))
        expectEqual smallValue.value 5
        expectError (SiblingConstraints.smallObj.Parse("""{"value": 150}""")) // outside small's range, even though within large's

        let largeValue = expectOk (SiblingConstraints.largeObj.Parse("""{"value": 150}"""))
        expectEqual largeValue.value 150
        expectError (SiblingConstraints.largeObj.Parse("""{"value": 5}""")) // outside large's range, even though within small's

    [<Fact>]
    let ``a class reached through an array item validates its own constraints in Parse`` () =
        expectError (ArrayItemConstraints.scoresItem.Parse("""{"points": 150}""")) // above maximum
        let item = expectOk (ArrayItemConstraints.scoresItem.Parse("""{"points": 50}"""))
        expectEqual item.points 50
