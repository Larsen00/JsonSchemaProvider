namespace JsonSchemaProvider.Tests

// Object-nesting coverage: properties/required/optional round-tripping, object's own keywords
// (minProperties/maxProperties/additionalProperties/patternProperties), and every keyword from
// NumberKeywordTests/StringKeywordTests/ArrayKeywordTests tested again nested in a property
// instead of at the schema root, to prove the nesting itself doesn't break the keyword.
module ObjectKeywordTests =
    open Expecto
    open JsonSchemaProvider

    [<Literal>]
    let flatSchema =
        """{ "type": "object", "properties": { "X": {"type": "string"}, "Y": {"type": "string"}, "Z": {"type": "integer"} } }"""
    type Flat = JsonSchemaProvider<schema=flatSchema>

    let validRecordShouldBeParsed =
        test "valid record should be parsed" {
            let flat = Expect.wantOk (Flat.Parse("""{"X": "x", "Z": 1}""")) "Parse should succeed"
            Expect.equal flat.X (Some "x") "flat.X = Some x"
            Expect.equal flat.Y None "flat.Y = None"
            Expect.equal flat.Z (Some 1) "flat.Z = Some 1"
        }

    let createMethodShouldReturnRecord =
        test "create method should return record" {
            let flat = Flat.Create(X = "x", Z = 1)
            Expect.equal flat.X (Some "x") "flat.X = Some x"
            Expect.equal flat.Y None "flat.Y = None"
            Expect.equal flat.Z (Some 1) "flat.Z = Some 1"
        }

    // flatSchema's optional Z:integer covers the FSharpInt branch; this covers the FSharpBool/
    // FSharpDouble optional-property branches, which nothing else in this file exercises.
    [<Literal>]
    let optionalPrimitivesSchema =
        """{ "type": "object", "properties": { "flag": {"type": "boolean"}, "amount": {"type": "number"} } }"""
    type OptionalPrimitives = JsonSchemaProvider<schema=optionalPrimitivesSchema>

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

    [<Literal>]
    let requiredPropertiesSchema =
        """{ "type": "object", "properties": { "X": {"type": "string"}, "Y": {"type": "string"}, "Z": {"type": "integer"} }, "required": ["X", "Y", "Z"] }"""
    type RequiredProperties = JsonSchemaProvider<schema=requiredPropertiesSchema>

    let requiredPropertiesShouldNotBeParsedIntoOption =
        test "required properties should not be parsed into Option" {
            let v = Expect.wantOk (RequiredProperties.Parse("""{"X": "x", "Y": "y", "Z": 1}""")) "Parse should succeed"
            Expect.equal v.X "x" "v.X = x"
            Expect.equal v.Y "y" "v.Y = y"
            Expect.equal v.Z 1 "v.Z = 1"
        }

    [<Literal>]
    let cityPosition =
        """
        {
          "type": "object",
          "properties": {
            "city": {"type": "string"},
            "globalPosition": {
              "type": "object",
              "properties": { "lat": {"type": "number"}, "lon": {"type": "number"} },
              "required": ["lat", "lon"]
            }
          },
          "required": ["city", "globalPosition"]
        }"""
    type CityPosition = JsonSchemaProvider<schema=cityPosition>

    let valueFromNestedObjectsShouldBeCreated =
        test "value from nested objects should be created" {
            let globalPosition = CityPosition.globalPositionObj.Create(lat = 52.520007, lon = 13.404954)
            let created = CityPosition.Create(city = "Berlin", globalPosition = globalPosition)
            Expect.equal created.globalPosition.lat 52.520007 "create and select nested are equal"
        }

    // -- number keyword nested in a property (root-level versions: NumberKeywordTests.fs) --

    [<Literal>]
    let exclusiveRangeSchema =
        """{ "type": "object", "properties": { "value": { "type": "number", "exclusiveMinimum": 0, "exclusiveMaximum": 10 } }, "required": ["value"] }"""
    type ExclusiveRange = JsonSchemaProvider<schema=exclusiveRangeSchema>

    let exclusiveMinimumBoundaryIsRejected =
        test "object property: exclusiveMinimum rejects the boundary value itself" {
            Expect.isError (ExclusiveRange.Create(value = 0.0)) "0 is excluded by exclusiveMinimum"
        }

    let exclusiveMaximumBoundaryIsRejected =
        test "object property: exclusiveMaximum rejects the boundary value itself" {
            Expect.isError (ExclusiveRange.Create(value = 10.0)) "10 is excluded by exclusiveMaximum"
        }

    let insideExclusiveRangeIsAccepted =
        test "object property: a value strictly inside an exclusive range is accepted" {
            let result = Expect.wantOk (ExclusiveRange.Create(value = 5.0)) "Create should succeed"
            Expect.equal result.value 5.0 "5 is strictly between the bounds"
        }

    [<Literal>]
    let multipleOfSchema =
        """{ "type": "object", "properties": { "value": { "type": "integer", "multipleOf": 5 } }, "required": ["value"] }"""
    type MultipleOf = JsonSchemaProvider<schema=multipleOfSchema>

    let nonMultipleIsRejected =
        test "object property: multipleOf rejects a non-multiple value" {
            Expect.isError (MultipleOf.Create(value = 7)) "7 is not a multiple of 5"
        }

    let multipleIsAccepted =
        test "object property: multipleOf accepts a genuine multiple" {
            let result = Expect.wantOk (MultipleOf.Create(value = 10)) "Create should succeed"
            Expect.equal result.value 10 "10 is a multiple of 5"
        }

    // -- string keyword nested in a property (root-level versions: StringKeywordTests.fs) --

    [<Literal>]
    let patternSchema =
        """{ "type": "object", "properties": { "X": { "type": "string", "pattern": "^[a-z]+$" } } }"""
    type PatternSchema = JsonSchemaProvider<schema=patternSchema>

    let validationErrorShouldBeDetectedByCreate =
        test "object property: validation error should be detected by Create" {
            Expect.isError (PatternSchema.Create(X = "a1")) "Create should return Error for invalid pattern"
        }

    let validationErrorShouldBeDetectedByParse =
        test "object property: validation error should be detected by Parse" {
            Expect.isError (PatternSchema.Parse("""{"X": "a1"}""")) "Parse throws validation exception"
        }

    [<Literal>]
    let stringLengthSchema =
        """{ "type": "object", "properties": { "value": { "type": "string", "minLength": 3, "maxLength": 5 } }, "required": ["value"] }"""
    type StringLength = JsonSchemaProvider<schema=stringLengthSchema>

    let tooShortStringIsRejected =
        test "object property: minLength rejects a too-short string" {
            Expect.isError (StringLength.Create(value = "ab")) "\"ab\" is below minLength"
        }

    let tooLongStringIsRejected =
        test "object property: maxLength rejects a too-long string" {
            Expect.isError (StringLength.Create(value = "abcdef")) "\"abcdef\" is above maxLength"
        }

    let stringWithinLengthRangeIsAccepted =
        test "object property: a string within [minLength, maxLength] is accepted" {
            let result = Expect.wantOk (StringLength.Create(value = "abc")) "Create should succeed"
            Expect.equal result.value "abc" "within [3, 5]"
        }

    let stringAtMaxLengthIsAccepted =
        test "object property: maxLength accepts a string at the limit" {
            let result = Expect.wantOk (StringLength.Create(value = "abcde")) "Create should succeed"
            Expect.equal result.value "abcde" "exactly 5 characters"
        }

    [<Literal>]
    let emailFormatSchema =
        """{ "type": "object", "properties": { "value": { "type": "string", "format": "email" } }, "required": ["value"] }"""
    type EmailFormat = JsonSchemaProvider<schema=emailFormatSchema>

    let invalidEmailFormatIsRejected =
        test "object property: format=email rejects a non-email string" {
            Expect.isError (EmailFormat.Create(value = "not-an-email")) "does not match the email format"
        }

    let validEmailFormatIsAccepted =
        test "object property: format=email accepts a genuine email string" {
            let result = Expect.wantOk (EmailFormat.Create(value = "a@b.com")) "Create should succeed"
            Expect.equal result.value "a@b.com" "matches the email format"
        }

    // -- array keyword nested in a property (root-level versions: ArrayKeywordTests.fs) --

    [<Literal>]
    let stringArrayMin2Schema =
        """{ "type": "object", "properties": { "tags": { "type": "array", "items": {"type": "string"}, "minItems": 2 } }, "required": ["tags"] }"""
    type PlainStringArray = JsonSchemaProvider<schema = stringArrayMin2Schema, ignoreSpecificKeywords = true>
    type StringArrayMin2 = JsonSchemaProvider<schema = stringArrayMin2Schema>

    let withoutFlagMinItemsSchemaYieldsPlainList =
        test "object property: minItems schema without compileMinItems flag yields plain list" {
            let v = Expect.wantOk (PlainStringArray.Parse("""{"tags": ["a", "b", "c"]}""")) "Parse should succeed"
            Expect.equal v.tags [ "a"; "b"; "c" ] "tags = [a; b; c]"
        }

    let min2CreateProducesCorrectTuple =
        test "object property: minItems=2 Create produces correct tuple" {
            let (h1, h2, rest) = (StringArrayMin2.Create(tags = ("a", "b", [ "c"; "d" ]))).tags
            Expect.equal h1 "a" "first element"
            Expect.equal h2 "b" "second element"
            Expect.equal rest [ "c"; "d" ] "rest"
        }

    let min2ParseProducesCorrectTuple =
        test "object property: minItems=2 Parse produces correct tuple" {
            let v = Expect.wantOk (StringArrayMin2.Parse("""{"tags": ["x", "y", "z"]}""")) "Parse should succeed"
            let (h1, h2, rest) = v.tags
            Expect.equal h1 "x" "first element"
            Expect.equal h2 "y" "second element"
            Expect.equal rest [ "z" ] "rest"
        }

    let min2ExactlyMinimumGivesEmptyRest =
        test "object property: minItems=2 with exactly 2 elements gives empty rest" {
            let v = Expect.wantOk (StringArrayMin2.Parse("""{"tags": ["a", "b"]}""")) "Parse should succeed"
            let (_, _, rest) = v.tags
            Expect.equal rest [] "rest is empty"
        }

    [<Literal>]
    let intArrayMin1Schema =
        """{ "type": "object", "properties": { "values": { "type": "array", "items": {"type": "integer"}, "minItems": 1 } }, "required": ["values"] }"""
    type IntArrayMin1 = JsonSchemaProvider<schema = intArrayMin1Schema>

    let min1CreateProducesCorrectPair =
        test "object property: minItems=1 Create produces correct pair" {
            let (head, rest) = (IntArrayMin1.Create(values = (42, [ 1; 2; 3 ]))).values
            Expect.equal head 42 "head"
            Expect.equal rest [ 1; 2; 3 ] "rest"
        }

    let min1ParseProducesCorrectPair =
        test "object property: minItems=1 Parse produces correct pair" {
            let v = Expect.wantOk (IntArrayMin1.Parse("""{"values": [10, 20, 30]}""")) "Parse should succeed"
            let (head, rest) = v.values
            Expect.equal head 10 "head"
            Expect.equal rest [ 20; 30 ] "rest"
        }

    let min1ExactlyMinimumGivesEmptyRest =
        test "object property: minItems=1 with exactly 1 element gives empty rest" {
            let v = Expect.wantOk (IntArrayMin1.Parse("""{"values": [99]}""")) "Parse should succeed"
            let (head, rest) = v.values
            Expect.equal head 99 "head"
            Expect.equal rest [] "rest is empty"
        }

    [<Literal>]
    let intArrayMin1Max2Schema =
        """{ "type": "object", "properties": { "values": { "type": "array", "items": {"type": "integer"}, "minItems": 1, "maxItems": 2 } }, "required": ["values"] }"""
    type IntArrayMin1Max2 = JsonSchemaProvider<schema = intArrayMin1Max2Schema>

    let minMaxCreateAtMinimumGivesNone =
        test "object property: minItems=1, maxItems=2: exactly minItems elements gives None for the rest" {
            let (head, rest) = (IntArrayMin1Max2.Create(values = (42, None))).values
            Expect.equal head 42 "head"
            Expect.equal rest None "no second element"
        }

    let minMaxCreateAtMaximumGivesSome =
        test "object property: minItems=1, maxItems=2: maxItems elements gives Some for the rest" {
            let (head, rest) = (IntArrayMin1Max2.Create(values = (42, Some 7))).values
            Expect.equal head 42 "head"
            Expect.equal rest (Some 7) "second element"
        }

    let minMaxParseAtMaximumGivesSome =
        test "object property: minItems=1, maxItems=2: Parse with maxItems elements gives Some for the rest" {
            let v = Expect.wantOk (IntArrayMin1Max2.Parse("""{"values": [1, 2]}""")) "Parse should succeed"
            Expect.equal v.values (1, Some 2) "parsed tuple matches the two elements"
        }

    [<Literal>]
    let intArrayMax2Schema =
        """{ "type": "object", "properties": { "values": { "type": "array", "items": {"type": "integer"}, "maxItems": 2 } }, "required": ["values"] }"""
    type IntArrayMax2 = JsonSchemaProvider<schema = intArrayMax2Schema>

    let maxOnlyEmptyArrayGivesNone =
        test "object property: maxItems=2, no minItems: no elements gives None" {
            Expect.equal (IntArrayMax2.Create(values = None)).values None "empty array with no lower bound"
        }

    let maxOnlyOneElementGivesPartialSome =
        test "object property: maxItems=2, no minItems: one element gives Some(head, None)" {
            Expect.equal (IntArrayMax2.Create(values = Some(1, None))).values (Some(1, None)) "one element, nothing after it"
        }

    let maxOnlyAtLimitGivesFullSome =
        test "object property: maxItems=2, no minItems: at the limit gives Some(head, Some tail)" {
            Expect.equal (IntArrayMax2.Create(values = Some(1, Some 2))).values (Some(1, Some 2)) "both elements present"
        }

    let maxOnlyParseAtLimit =
        test "object property: maxItems=2, no minItems: Parse at the limit round-trips both elements" {
            let v = Expect.wantOk (IntArrayMax2.Parse("""{"values": [5, 6]}""")) "Parse should succeed"
            Expect.equal v.values (Some(5, Some 6)) "both elements present"
        }

    [<Literal>]
    let intArrayMax1Schema =
        """{ "type": "object", "properties": { "values": { "type": "array", "items": {"type": "integer"}, "maxItems": 1 } }, "required": ["values"] }"""
    type IntArrayMax1 = JsonSchemaProvider<schema = intArrayMax1Schema>

    let maxOneEmptyArrayGivesNone =
        test "object property: maxItems=1: no elements gives None" {
            Expect.equal (IntArrayMax1.Create(values = None)).values None "empty array"
        }

    let maxOneSingleElementGivesSome =
        test "object property: maxItems=1: one element gives Some" {
            Expect.equal (IntArrayMax1.Create(values = Some 42)).values (Some 42) "the single element"
        }

    let maxOneParseSingleElement =
        test "object property: maxItems=1: Parse with one element round-trips it" {
            let v = Expect.wantOk (IntArrayMax1.Parse("""{"values": [7]}""")) "Parse should succeed"
            Expect.equal v.values (Some 7) "the single element"
        }

    // See ArrayKeywordTests.fs's matching block: IntArrayExact2WithFlag is declared identically
    // to IntArrayExact2 (neither passes ignoreSpecificKeywords), inherited as-is.
    [<Literal>]
    let intArrayExact2Schema =
        """{ "type": "object", "properties": { "values": { "type": "array", "items": {"type": "integer"}, "minItems": 2, "maxItems": 2 } }, "required": ["values"] }"""
    type IntArrayExact2 = JsonSchemaProvider<schema = intArrayExact2Schema>
    type IntArrayExact2WithFlag = JsonSchemaProvider<schema = intArrayExact2Schema>

    let exactWithoutFlagCreateProducesTuple =
        test "object property: minItems=maxItems=2, no flag: Create produces the exact tuple" {
            Expect.equal (IntArrayExact2.Create(values = (1, 2))).values (1, 2) "exact 2-tuple"
        }

    let exactWithoutFlagParseProducesTuple =
        test "object property: minItems=maxItems=2, no flag: Parse produces the exact tuple" {
            let v = Expect.wantOk (IntArrayExact2.Parse("""{"values": [3, 4]}""")) "Parse should succeed"
            Expect.equal v.values (3, 4) "exact 2-tuple"
        }

    let exactWithFlagCreateProducesTuple =
        test "object property: minItems=maxItems=2, second declaration: Create produces the same exact tuple" {
            Expect.equal (IntArrayExact2WithFlag.Create(values = (5, 6))).values (5, 6) "exact 2-tuple"
        }

    let exactWithFlagParseProducesTuple =
        test "object property: minItems=maxItems=2, second declaration: Parse produces the same exact tuple" {
            let v = Expect.wantOk (IntArrayExact2WithFlag.Parse("""{"values": [7, 8]}""")) "Parse should succeed"
            Expect.equal v.values (7, 8) "exact 2-tuple"
        }

    [<Literal>]
    let uniqueItemsSchema =
        """{ "type": "object", "properties": { "values": { "type": "array", "items": { "type": "integer" }, "uniqueItems": true } }, "required": ["values"] }"""
    type UniqueItemsArray = JsonSchemaProvider<schema=uniqueItemsSchema>

    let duplicateItemsAreRejected =
        test "object property: uniqueItems rejects an array with a duplicate" {
            Expect.isError (UniqueItemsArray.Create(values = [ 1; 2; 2 ])) "[1;2;2] has a duplicate"
        }

    let allUniqueItemsAreAccepted =
        test "object property: uniqueItems accepts an array with no duplicates" {
            let result = Expect.wantOk (UniqueItemsArray.Create(values = [ 1; 2; 3 ])) "Create should succeed"
            Expect.equal result.values [ 1; 2; 3 ] "no duplicates"
        }

    [<Literal>]
    let maxItemsSchema =
        """{ "type": "object", "properties": { "values": { "type": "array", "items": { "type": "integer" }, "maxItems": 2 } }, "required": ["values"] }"""
    type MaxItemsArray = JsonSchemaProvider<schema=maxItemsSchema>

    let atMaxItemsIsAccepted =
        test "object property: maxItems accepts an array at the limit" {
            Expect.equal (MaxItemsArray.Create(values = Some(1, Some 2))).values (Some(1, Some 2)) "2 elements is exactly maxItems=2"
        }

    let withinMaxItemsIsAccepted =
        test "object property: maxItems accepts an array within the limit" {
            Expect.equal (MaxItemsArray.Create(values = Some(1, None))).values (Some(1, None)) "1 element is within maxItems=2"
        }

    [<Literal>]
    let minItemsRuntimeSchema =
        """{ "type": "object", "properties": { "tags": { "type": "array", "items": { "type": "string" }, "minItems": 2 } }, "required": ["tags"] }"""
    type MinItemsRuntime = JsonSchemaProvider<schema=minItemsRuntimeSchema, ignoreSpecificKeywords=true>

    let tooFewItemsAreRejectedAtRuntime =
        test "object property: minItems is enforced at runtime by Create, not just as a compile-time tuple shape" {
            Expect.isError (MinItemsRuntime.Create(tags = [ "a" ])) "1 element is below minItems=2"
        }

    let enoughItemsAreAcceptedAtRuntime =
        test "object property: minItems is satisfied at runtime when there are enough elements" {
            let result = Expect.wantOk (MinItemsRuntime.Create(tags = [ "a"; "b" ])) "Create should succeed"
            Expect.equal result.tags [ "a"; "b" ] "2 elements satisfies minItems=2"
        }

    [<Literal>]
    let arrayItemConstraintSchema =
        """{ "type": "object", "properties": { "values": { "type": "array", "items": { "type": "integer", "minimum": 0 } } }, "required": ["values"] }"""
    type ArrayItemConstraint = JsonSchemaProvider<schema=arrayItemConstraintSchema>

    let arrayWithAnOutOfRangeItemIsRejected =
        test "object property: an item schema's own constraint is enforced against every element" {
            Expect.isError (ArrayItemConstraint.Create(values = [ 1; -1; 3 ])) "-1 violates the item schema's minimum"
        }

    let arrayWithAllInRangeItemsIsAccepted =
        test "object property: an array whose every element satisfies the item schema is accepted" {
            let result = Expect.wantOk (ArrayItemConstraint.Create(values = [ 1; 2; 3 ])) "Create should succeed"
            Expect.equal result.values [ 1; 2; 3 ] "every element satisfies the minimum"
        }

    // -- object's own keywords: minProperties / maxProperties / additionalProperties / patternProperties --

    [<Literal>]
    let minPropertiesSchema =
        """{ "type": "object", "properties": { "a": {"type": "integer"}, "b": {"type": "integer"}, "c": {"type": "integer"} }, "minProperties": 2 }"""
    type MinPropertiesObject = JsonSchemaProvider<schema=minPropertiesSchema>

    let tooFewPropertiesAreRejected =
        test "minProperties rejects an object built with too few properties set" {
            Expect.isError (MinPropertiesObject.Create(a = 1)) "only 1 property set is below minProperties=2"
        }

    let enoughPropertiesAreAccepted =
        test "minProperties accepts an object with enough properties set" {
            let result = Expect.wantOk (MinPropertiesObject.Create(a = 1, b = 2)) "Create should succeed"
            Expect.equal (result.a, result.b) (Some 1, Some 2) "2 properties set satisfies minProperties=2"
        }

    [<Literal>]
    let maxPropertiesSchema =
        """{ "type": "object", "properties": { "a": {"type": "integer"}, "b": {"type": "integer"}, "c": {"type": "integer"} }, "maxProperties": 2 }"""
    type MaxPropertiesObject = JsonSchemaProvider<schema=maxPropertiesSchema>

    let tooManyPropertiesAreRejected =
        test "maxProperties rejects an object built with too many properties set" {
            Expect.isError (MaxPropertiesObject.Create(a = 1, b = 2, c = 3)) "3 properties set exceeds maxProperties=2"
        }

    let withinMaxPropertiesIsAccepted =
        test "maxProperties accepts an object within the limit" {
            let result = Expect.wantOk (MaxPropertiesObject.Create(a = 1, b = 2)) "Create should succeed"
            Expect.equal (result.a, result.b) (Some 1, Some 2) "2 properties set is within maxProperties=2"
        }

    // additionalProperties/patternProperties can only be violated through Parse - Create's
    // parameter list is fixed by the schema's own declared properties.
    [<Literal>]
    let additionalPropertiesSchema =
        """{ "type": "object", "properties": { "a": {"type": "integer"} }, "additionalProperties": false }"""
    type AdditionalPropertiesObject = JsonSchemaProvider<schema=additionalPropertiesSchema>

    let additionalPropertyIsRejectedByParse =
        test "additionalProperties=false rejects an undeclared property via Parse" {
            Expect.isError (AdditionalPropertiesObject.Parse("""{"a": 1, "extra": true}""")) "\"extra\" isn't declared"
        }

    let onlyDeclaredPropertyIsAcceptedByParse =
        test "additionalProperties=false accepts an object with only declared properties via Parse" {
            let result = Expect.wantOk (AdditionalPropertiesObject.Parse("""{"a": 1}""")) "Parse should succeed"
            Expect.equal result.a (Some 1) "only declared properties present"
        }

    [<Literal>]
    let patternPropertiesSchema =
        """{ "type": "object", "patternProperties": { "^S_": {"type": "string"} }, "additionalProperties": false }"""
    type PatternPropertiesObject = JsonSchemaProvider<schema=patternPropertiesSchema>

    let nonMatchingPropertyNameIsRejectedByParse =
        test "patternProperties rejects a property name that matches neither the pattern nor additionalProperties" {
            Expect.isError (PatternPropertiesObject.Parse("""{"S_x": "ok", "other": 1}""")) "\"other\" doesn't match S_ or additionalProperties"
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
              exclusiveMinimumBoundaryIsRejected
              exclusiveMaximumBoundaryIsRejected
              insideExclusiveRangeIsAccepted
              nonMultipleIsRejected
              multipleIsAccepted
              validationErrorShouldBeDetectedByCreate
              validationErrorShouldBeDetectedByParse
              tooShortStringIsRejected
              tooLongStringIsRejected
              stringWithinLengthRangeIsAccepted
              stringAtMaxLengthIsAccepted
              invalidEmailFormatIsRejected
              validEmailFormatIsAccepted
              withoutFlagMinItemsSchemaYieldsPlainList
              min2CreateProducesCorrectTuple
              min2ParseProducesCorrectTuple
              min2ExactlyMinimumGivesEmptyRest
              min1CreateProducesCorrectPair
              min1ParseProducesCorrectPair
              min1ExactlyMinimumGivesEmptyRest
              minMaxCreateAtMinimumGivesNone
              minMaxCreateAtMaximumGivesSome
              minMaxParseAtMaximumGivesSome
              maxOnlyEmptyArrayGivesNone
              maxOnlyOneElementGivesPartialSome
              maxOnlyAtLimitGivesFullSome
              maxOnlyParseAtLimit
              maxOneEmptyArrayGivesNone
              maxOneSingleElementGivesSome
              maxOneParseSingleElement
              exactWithoutFlagCreateProducesTuple
              exactWithoutFlagParseProducesTuple
              exactWithFlagCreateProducesTuple
              exactWithFlagParseProducesTuple
              duplicateItemsAreRejected
              allUniqueItemsAreAccepted
              atMaxItemsIsAccepted
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
