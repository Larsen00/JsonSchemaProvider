namespace JsonSchemaProvider.Tests

// Broader coverage of JSON Schema keyword categories against real validation (Create/Parse),
// not just integer minimum/maximum. The point of every test here is the same as the fix in
// notes/nested-create-subschema-resolution.md: validation runs via NJsonSchema's own
// schema.Validate against the real schema text, independent of which keywords
// SchemaConversion.fs's FSharpType/Keywords model happens to capture or act on at the type
// level - so it should enforce keywords the type-level model doesn't represent at all
// (uniqueItems, minProperties, ...) or captures as inert data only (format/pattern/minLength on
// FSharpString) just as correctly as the ones it actively acts on (minimum/maximum on FSharpInt,
// which nothing here enforces at compile time either - see ideas/compile-time-*-constraints.md).
//
// Not covered: the "dependencies"/"dependentRequired" keyword. Verified directly against
// SchemaCache.parseSchema (bypassing the type provider entirely) that neither keyword name
// triggers any validation error in this repo's NJsonSchema version/configuration - a genuine
// limitation of the underlying library setup here, not something worth asserting a test against.
module KeywordCoverageTests =
    open Expecto
    open JsonSchemaProvider

    // ---- number: exclusiveMinimum / exclusiveMaximum / multipleOf ----

    [<Literal>]
    let exclusiveRangeSchema =
        """
        { "type": "object", "properties": { "value": { "type": "number", "exclusiveMinimum": 0, "exclusiveMaximum": 10 } }, "required": ["value"] }"""

    type ExclusiveRange = JsonSchemaProvider<schema=exclusiveRangeSchema>

    [<Literal>]
    let multipleOfSchema =
        """
        { "type": "object", "properties": { "value": { "type": "integer", "multipleOf": 5 } }, "required": ["value"] }"""

    type MultipleOf = JsonSchemaProvider<schema=multipleOfSchema>

    let exclusiveMinimumBoundaryIsRejected =
        test "exclusiveMinimum rejects the boundary value itself" {
            Expect.throws
                (fun () -> ExclusiveRange.Create(value = 0.0) |> ignore)
                "0 should be rejected - exclusiveMinimum excludes the bound"
        }

    let exclusiveMaximumBoundaryIsRejected =
        test "exclusiveMaximum rejects the boundary value itself" {
            Expect.throws
                (fun () -> ExclusiveRange.Create(value = 10.0) |> ignore)
                "10 should be rejected - exclusiveMaximum excludes the bound"
        }

    let insideExclusiveRangeIsAccepted =
        test "a value strictly inside an exclusive range is accepted" {
            let result = ExclusiveRange.Create(value = 5.0)
            Expect.equal result.value 5.0 "5 is strictly between the exclusive bounds"
        }

    let nonMultipleIsRejected =
        test "multipleOf rejects a non-multiple value" {
            Expect.throws
                (fun () -> MultipleOf.Create(value = 7) |> ignore)
                "7 is not a multiple of 5"
        }

    let multipleIsAccepted =
        test "multipleOf accepts a genuine multiple" {
            let result = MultipleOf.Create(value = 10)
            Expect.equal result.value 10 "10 is a multiple of 5"
        }

    // ---- string: minLength / maxLength / format ----

    [<Literal>]
    let stringLengthSchema =
        """
        { "type": "object", "properties": { "value": { "type": "string", "minLength": 3, "maxLength": 5 } }, "required": ["value"] }"""

    type StringLength = JsonSchemaProvider<schema=stringLengthSchema>

    [<Literal>]
    let emailFormatSchema =
        """
        { "type": "object", "properties": { "value": { "type": "string", "format": "email" } }, "required": ["value"] }"""

    type EmailFormat = JsonSchemaProvider<schema=emailFormatSchema>

    let tooShortStringIsRejected =
        test "minLength rejects a too-short string" {
            Expect.throws (fun () -> StringLength.Create(value = "ab") |> ignore) "\"ab\" is below minLength"
        }

    let tooLongStringIsRejected =
        test "maxLength rejects a too-long string" {
            Expect.throws (fun () -> StringLength.Create(value = "abcdef") |> ignore) "\"abcdef\" is above maxLength"
        }

    let stringWithinLengthRangeIsAccepted =
        test "a string within [minLength, maxLength] is accepted" {
            let result = StringLength.Create(value = "abc")
            Expect.equal result.value "abc" "\"abc\" is within [3, 5]"
        }

    let invalidEmailFormatIsRejected =
        test "format=email rejects a non-email string" {
            Expect.throws
                (fun () -> EmailFormat.Create(value = "not-an-email") |> ignore)
                "\"not-an-email\" does not match the email format"
        }

    let validEmailFormatIsAccepted =
        test "format=email accepts a genuine email string" {
            let result = EmailFormat.Create(value = "a@b.com")
            Expect.equal result.value "a@b.com" "\"a@b.com\" matches the email format"
        }

    // ---- array: uniqueItems / maxItems / minItems (runtime) / per-item constraints ----

    [<Literal>]
    let uniqueItemsSchema =
        """
        { "type": "object", "properties": { "values": { "type": "array", "items": { "type": "integer" }, "uniqueItems": true } }, "required": ["values"] }"""

    type UniqueItemsArray = JsonSchemaProvider<schema=uniqueItemsSchema>

    [<Literal>]
    let maxItemsSchema =
        """
        { "type": "object", "properties": { "values": { "type": "array", "items": { "type": "integer" }, "maxItems": 2 } }, "required": ["values"] }"""

    type MaxItemsArray = JsonSchemaProvider<schema=maxItemsSchema>

    // Plain list (no compileMinItems) - ArrayTests.fs only covers the compile-time tuple shape
    // for minItems, never whether Create actually REJECTS too few items at runtime.
    [<Literal>]
    let minItemsRuntimeSchema =
        """
        { "type": "object", "properties": { "tags": { "type": "array", "items": { "type": "string" }, "minItems": 2 } }, "required": ["tags"] }"""

    type MinItemsRuntime = JsonSchemaProvider<schema=minItemsRuntimeSchema>

    [<Literal>]
    let arrayItemConstraintSchema =
        """
        { "type": "object", "properties": { "values": { "type": "array", "items": { "type": "integer", "minimum": 0 } } }, "required": ["values"] }"""

    type ArrayItemConstraint = JsonSchemaProvider<schema=arrayItemConstraintSchema>

    let duplicateItemsAreRejected =
        test "uniqueItems rejects an array with a duplicate" {
            Expect.throws
                (fun () -> UniqueItemsArray.Create(values = [ 1; 2; 2 ]) |> ignore)
                "[1;2;2] has a duplicate"
        }

    let allUniqueItemsAreAccepted =
        test "uniqueItems accepts an array with no duplicates" {
            let result = UniqueItemsArray.Create(values = [ 1; 2; 3 ])
            Expect.equal result.values [ 1; 2; 3 ] "[1;2;3] has no duplicates"
        }

    let tooManyItemsAreRejected =
        test "maxItems rejects an array with too many elements" {
            Expect.throws
                (fun () -> MaxItemsArray.Create(values = [ 1; 2; 3 ]) |> ignore)
                "3 elements exceeds maxItems=2"
        }

    let withinMaxItemsIsAccepted =
        test "maxItems accepts an array within the limit" {
            let result = MaxItemsArray.Create(values = [ 1; 2 ])
            Expect.equal result.values [ 1; 2 ] "2 elements is within maxItems=2"
        }

    let tooFewItemsAreRejectedAtRuntime =
        test "minItems is enforced at runtime by Create, not just as a compile-time tuple shape" {
            Expect.throws
                (fun () -> MinItemsRuntime.Create(tags = [ "a" ]) |> ignore)
                "1 element is below minItems=2"
        }

    let enoughItemsAreAcceptedAtRuntime =
        test "minItems is satisfied at runtime by Create when there are enough elements" {
            let result = MinItemsRuntime.Create(tags = [ "a"; "b" ])
            Expect.equal result.tags [ "a"; "b" ] "2 elements satisfies minItems=2"
        }

    let arrayWithAnOutOfRangeItemIsRejected =
        test "a constraint on the array's own item schema is enforced against every element" {
            Expect.throws
                (fun () -> ArrayItemConstraint.Create(values = [ 1; -1; 3 ]) |> ignore)
                "-1 violates the item schema's own minimum, even though the array itself has no length constraint"
        }

    let arrayWithAllInRangeItemsIsAccepted =
        test "an array whose every element satisfies the item schema is accepted" {
            let result = ArrayItemConstraint.Create(values = [ 1; 2; 3 ])
            Expect.equal result.values [ 1; 2; 3 ] "every element satisfies the item schema's minimum"
        }

    // ---- object: minProperties / maxProperties / additionalProperties / patternProperties ----

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
            Expect.throws
                (fun () -> MinPropertiesObject.Create(a = 1) |> ignore)
                "only 1 property set is below minProperties=2"
        }

    let enoughPropertiesAreAccepted =
        test "minProperties accepts an object with enough properties set" {
            let result = MinPropertiesObject.Create(a = 1, b = 2)
            Expect.equal (result.a, result.b) (Some 1, Some 2) "2 properties set satisfies minProperties=2"
        }

    let tooManyPropertiesAreRejected =
        test "maxProperties rejects an object built with too many properties set" {
            Expect.throws
                (fun () -> MaxPropertiesObject.Create(a = 1, b = 2, c = 3) |> ignore)
                "3 properties set exceeds maxProperties=2"
        }

    let withinMaxPropertiesIsAccepted =
        test "maxProperties accepts an object within the limit" {
            let result = MaxPropertiesObject.Create(a = 1, b = 2)
            Expect.equal (result.a, result.b) (Some 1, Some 2) "2 properties set is within maxProperties=2"
        }

    let additionalPropertyIsRejectedByParse =
        test "additionalProperties=false rejects an undeclared property via Parse" {
            Expect.throws
                (fun () -> AdditionalPropertiesObject.Parse("""{"a": 1, "extra": true}""") |> ignore)
                "\"extra\" isn't declared and additionalProperties is false"
        }

    let onlyDeclaredPropertyIsAcceptedByParse =
        test "additionalProperties=false accepts an object with only declared properties via Parse" {
            let result = AdditionalPropertiesObject.Parse("""{"a": 1}""")
            Expect.equal result.a (Some 1) "only declared properties present"
        }

    let nonMatchingPropertyNameIsRejectedByParse =
        test "patternProperties rejects a property name that matches neither the pattern nor additionalProperties" {
            Expect.throws
                (fun () -> PatternPropertiesObject.Parse("""{"S_x": "ok", "other": 1}""") |> ignore)
                "\"other\" doesn't match the S_ pattern and additionalProperties is false"
        }

    let matchingPatternPropertyIsAcceptedByParse =
        test "patternProperties accepts a property name matching the pattern" {
            PatternPropertiesObject.Parse("""{"S_x": "ok"}""") |> ignore
        }

    [<Tests>]
    let tests =
        testList
            "JsonSchemaProvider.Tests.KeywordCoverageTests"
            [ exclusiveMinimumBoundaryIsRejected
              exclusiveMaximumBoundaryIsRejected
              insideExclusiveRangeIsAccepted
              nonMultipleIsRejected
              multipleIsAccepted
              tooShortStringIsRejected
              tooLongStringIsRejected
              stringWithinLengthRangeIsAccepted
              invalidEmailFormatIsRejected
              validEmailFormatIsAccepted
              duplicateItemsAreRejected
              allUniqueItemsAreAccepted
              tooManyItemsAreRejected
              withinMaxItemsIsAccepted
              tooFewItemsAreRejectedAtRuntime
              enoughItemsAreAcceptedAtRuntime
              arrayWithAnOutOfRangeItemIsRejected
              arrayWithAllInRangeItemsIsAccepted
              tooFewPropertiesAreRejected
              enoughPropertiesAreAccepted
              tooManyPropertiesAreRejected
              withinMaxPropertiesIsAccepted
              additionalPropertyIsRejectedByParse
              onlyDeclaredPropertyIsAcceptedByParse
              nonMatchingPropertyNameIsRejectedByParse
              matchingPatternPropertyIsAcceptedByParse ]
