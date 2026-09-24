namespace JsonSchemaProvider.Tests

// string keywords: pattern, minLength, maxLength, format. Same point as NumberKeywordTests:
// validation runs via NJsonSchema's own schema.Validate against the real schema text, so it
// enforces minLength/maxLength/format even though SchemaConversion.fs's FSharpString model
// captures them as inert data only, never acting on them at the type level.
module StringKeywordTests =
    open Expecto
    open JsonSchemaProvider

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

    type PatternSchema = JsonSchemaProvider<schema=patternSchema>

    let validationErrorShouldBeDetectedByCreate =
        test "validation error should be detected by Create" {
            Expect.isError (PatternSchema.Create(X = "a1")) "Create should return Error for invalid pattern"
        }

    let validationErrorShouldBeDetectedByParse =
        test "validation error should be detected by Parse" {
            Expect.isError
                (PatternSchema.Parse("""{"X": "a1"}"""))
                "Parse throws validation exception"
        }

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
            Expect.isError (StringLength.Create(value = "ab")) "\"ab\" is below minLength"
        }

    let tooLongStringIsRejected =
        test "maxLength rejects a too-long string" {
            Expect.isError (StringLength.Create(value = "abcdef")) "\"abcdef\" is above maxLength"
        }

    let stringWithinLengthRangeIsAccepted =
        test "a string within [minLength, maxLength] is accepted" {
            let result = Expect.wantOk (StringLength.Create(value = "abc")) "Create should succeed"
            Expect.equal result.value "abc" "\"abc\" is within [3, 5]"
        }

    let stringAtMaxLengthIsAccepted =
        test "maxLength accepts a string at the limit" {
            let result = Expect.wantOk (StringLength.Create(value = "abcde")) "Create should succeed"
            Expect.equal result.value "abcde" "\"abcde\" is exactly 5 characters, the maxLength limit"
        }

    let invalidEmailFormatIsRejected =
        test "format=email rejects a non-email string" {
            Expect.isError
                (EmailFormat.Create(value = "not-an-email"))
                "\"not-an-email\" does not match the email format"
        }

    let validEmailFormatIsAccepted =
        test "format=email accepts a genuine email string" {
            let result = Expect.wantOk (EmailFormat.Create(value = "a@b.com")) "Create should succeed"
            Expect.equal result.value "a@b.com" "\"a@b.com\" matches the email format"
        }

    [<Tests>]
    let tests =
        testList
            "JsonSchemaProvider.Tests.StringKeywordTests"
            [ validationErrorShouldBeDetectedByCreate
              validationErrorShouldBeDetectedByParse
              tooShortStringIsRejected
              tooLongStringIsRejected
              stringWithinLengthRangeIsAccepted
              stringAtMaxLengthIsAccepted
              invalidEmailFormatIsRejected
              validEmailFormatIsAccepted ]
