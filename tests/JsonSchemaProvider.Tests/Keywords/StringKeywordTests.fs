namespace JsonSchemaProvider.Tests

// String keywords at the schema root. Object-nested versions live in ObjectKeywordTests.fs.
module StringKeywordTests =
    open Xunit
    open JsonSchemaProvider

    [<Literal>]
    let patternSchema = """{ "type": "string", "pattern": "^[a-z]+$" }"""
    type PatternSchema = JsonSchemaProvider<schema=patternSchema>

    [<Fact>]
    let ``validation error should be detected by Create`` () =
        expectError (PatternSchema.Create("a1")) // Create should return Error for invalid pattern

    [<Fact>]
    let ``validation error should be detected by Parse`` () =
        expectError (PatternSchema.Parse("\"a1\"")) // Parse should reject a value that doesn't match the pattern

    [<Literal>]
    let stringLengthSchema = """{ "type": "string", "minLength": 3, "maxLength": 5 }"""
    type StringLength = JsonSchemaProvider<schema=stringLengthSchema>

    [<Fact>]
    let ``minLength rejects a too-short string`` () =
        expectError (StringLength.Create("ab")) // "ab" is below minLength

    [<Fact>]
    let ``maxLength rejects a too-long string`` () =
        expectError (StringLength.Create("abcdef")) // "abcdef" is above maxLength

    [<Fact>]
    let ``a string within [minLength, maxLength] is accepted`` () =
        let result = expectOk (StringLength.Create("abc"))
        expectEqual result "abc"

    [<Fact>]
    let ``maxLength accepts a string at the limit`` () =
        let result = expectOk (StringLength.Create("abcde"))
        expectEqual result "abcde"

    [<Literal>]
    let emailFormatSchema = """{ "type": "string", "format": "email" }"""
    type EmailFormat = JsonSchemaProvider<schema=emailFormatSchema>

    [<Fact>]
    let ``format=email rejects a non-email string`` () =
        expectError (EmailFormat.Create("not-an-email")) // does not match the email format

    [<Fact>]
    let ``format=email accepts a genuine email string`` () =
        let result = expectOk (EmailFormat.Create("a@b.com"))
        expectEqual result "a@b.com"
