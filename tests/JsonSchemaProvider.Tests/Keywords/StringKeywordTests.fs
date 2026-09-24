namespace JsonSchemaProvider.Tests

// String keywords at the schema root. Object-nested versions live in ObjectKeywordTests.fs.
module StringKeywordTests =
    open Expecto
    open JsonSchemaProvider

    [<Literal>]
    let patternSchema = """{ "type": "string", "pattern": "^[a-z]+$" }"""
    type PatternSchema = JsonSchemaProvider<schema=patternSchema>

    let validationErrorShouldBeDetectedByCreate =
        test "validation error should be detected by Create" {
            Expect.isError (PatternSchema.Create("a1")) "Create should return Error for invalid pattern"
        }

    let validationErrorShouldBeDetectedByParse =
        test "validation error should be detected by Parse" {
            Expect.isError (PatternSchema.Parse("\"a1\"")) "Parse should reject a value that doesn't match the pattern"
        }

    [<Literal>]
    let stringLengthSchema = """{ "type": "string", "minLength": 3, "maxLength": 5 }"""
    type StringLength = JsonSchemaProvider<schema=stringLengthSchema>

    let tooShortStringIsRejected =
        test "minLength rejects a too-short string" {
            Expect.isError (StringLength.Create("ab")) "\"ab\" is below minLength"
        }

    let tooLongStringIsRejected =
        test "maxLength rejects a too-long string" {
            Expect.isError (StringLength.Create("abcdef")) "\"abcdef\" is above maxLength"
        }

    let stringWithinLengthRangeIsAccepted =
        test "a string within [minLength, maxLength] is accepted" {
            let result = Expect.wantOk (StringLength.Create("abc")) "Create should succeed"
            Expect.equal result "abc" "within [3, 5]"
        }

    let stringAtMaxLengthIsAccepted =
        test "maxLength accepts a string at the limit" {
            let result = Expect.wantOk (StringLength.Create("abcde")) "Create should succeed"
            Expect.equal result "abcde" "exactly 5 characters"
        }

    [<Literal>]
    let emailFormatSchema = """{ "type": "string", "format": "email" }"""
    type EmailFormat = JsonSchemaProvider<schema=emailFormatSchema>

    let invalidEmailFormatIsRejected =
        test "format=email rejects a non-email string" {
            Expect.isError (EmailFormat.Create("not-an-email")) "does not match the email format"
        }

    let validEmailFormatIsAccepted =
        test "format=email accepts a genuine email string" {
            let result = Expect.wantOk (EmailFormat.Create("a@b.com")) "Create should succeed"
            Expect.equal result "a@b.com" "matches the email format"
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
