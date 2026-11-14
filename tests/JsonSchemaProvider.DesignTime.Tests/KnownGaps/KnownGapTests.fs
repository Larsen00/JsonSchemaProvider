namespace JsonSchemaProvider.Tests

// Every known gap the design-time side can see, in one place: each test pins what happens today
// where the spec asks for something else. A failing test here means a gap got closed: flip the
// assertion to the spec's behaviour and move the test to the file for its keyword or mechanism.
// Gaps visible through a provided type are in the runtime project's KnownGaps/KnownGapTests.fs.
module KnownGapTests =
    open System.IO
    open Expecto
    open JsonSchemaProvider
    open JsonSchemaProvider.DesignTime.SchemaConversion

    let private oldCodeGenerationFixture name =
        File.ReadAllText(Path.Combine(__SOURCE_DIRECTORY__, "../../JsonSchemaProvider.Tests/schemas/OldCodeGenerationTests", name))

    let private errorCount (schemaText: string) (instance: string) =
        Seq.length ((SchemaCache.parseSchema schemaText).Validate instance)

    let private parseFailsWith (expectedMessage: string) (schemaText: string) =
        try
            parseJsonSchema schemaText |> ignore
            failtest "Expected parseJsonSchema to fail"
        with
        | :? AssertException -> reraise ()
        | ex -> Expect.stringContains ex.Message expectedMessage "failure message"

    // The provider can't take these schemas, so a provided type over them doesn't compile.
    let unsupportedSchemas =
        testList
            "the provider rejects the schema"
            [ test "a null root" { parseFailsWith "Unsupported JSON object type Null" (oldCodeGenerationFixture "07-null.json") }

              test "a root $ref to a $defs entry" {
                  parseFailsWith "Unsupported JSON schema type None" (oldCodeGenerationFixture "09-ref.json")
              }

              test "an array with prefixItems and no items" {
                  // schema.Item is null, which parseArray passes straight on.
                  Expect.throws
                      (fun () -> parseJsonSchema """{ "type": "array", "prefixItems": [ { "type": "integer" } ] }""" |> ignore)
                      "prefixItems without items"
              } ]

    // NJsonSchema 11 keeps these keywords as extension data but its validator never reads them,
    // so the runtime fallback accepts instances the spec rejects.
    let ignoredByValidate =
        testList
            "NJsonSchema's Validate ignores the keyword"
            [ for keyword, schemaText, invalid, valid in Draft2020KeywordTests.validateCases ->
                  test keyword {
                      Expect.equal (errorCount schemaText valid) 0 $"valid {valid}"
                      Expect.equal (errorCount schemaText invalid) 0 $"invalid {invalid} is accepted"
                  } ]

    let njsonSchemaQuirks =
        testList
            "NJsonSchema departs from 2020-12"
            [ test "items next to prefixItems is applied to every element, as in draft 4" {
                  // 2020-12 applies items only after the prefixItems positions, so [1, "x"] is valid.
                  let schemaText = """{ "type": "array", "prefixItems": [ { "type": "integer" } ], "items": { "type": "string" } }"""
                  Expect.isGreaterThan (errorCount schemaText """[1, "x"]""") 0 "valid instance rejected"
              }

              test "a boolean items schema makes parsing throw" {
                  Expect.throws
                      (fun () -> SchemaCache.parseSchema """{ "type": "array", "prefixItems": [ { "type": "integer" } ], "items": false }""" |> ignore)
                      "items: false"
              } ]

    [<Tests>]
    let tests =
        testList "KnownGapTests" [ unsupportedSchemas; ignoredByValidate; njsonSchemaQuirks ]
