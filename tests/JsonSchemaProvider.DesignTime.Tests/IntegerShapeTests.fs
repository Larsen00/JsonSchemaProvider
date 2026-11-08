namespace JsonSchemaProvider.Tests

module IntegerShapeTests =
    open Expecto
    open JsonSchemaProvider.DesignTime.SchemaConversion
    open JsonSchemaProvider.DesignTime.IntegerShape
    open JsonSchemaProvider.DesignTime.ProviderConfiguration
    open TestHelpers

    let private classifyWith (flags: CompileFlags) (schemaText: string) =
        match parseJsonSchema schemaText with
        | JsonInteger keywords -> classifyIntegerShape flags keywords
        | other -> failwithf "Expected an integer schema, got %A" other

    let private classify = classifyWith defaultFlags

    let private shapeName (shape: IntegerShape) =
        match shape with
        | UnsupportedIntegerKeywords _ -> "UnsupportedIntegerKeywords"
        | NonNegativeInteger _ -> "NonNegativeInteger"
        | AnyInteger _ -> "AnyInteger"

    // An integer schema; `keywords` is spliced in after "type", e.g. ", \"minimum\": 0".
    let private integer (keywords: string) = """{ "type": "integer" """ + keywords + " }"

    let private expectShape (schemaText: string) (expected: string) =
        Expect.equal (shapeName (classify schemaText)) expected schemaText

    [<Tests>]
    let tests =
        testList
            "IntegerShapeTests"
            [ test "no numeric keywords is AnyInteger" { expectShape (integer "") "AnyInteger" }

              test "minimum 0 is NonNegativeInteger" { expectShape (integer ", \"minimum\": 0") "NonNegativeInteger" }

              test "exclusiveMinimum -1 is NonNegativeInteger" {
                  expectShape (integer ", \"exclusiveMinimum\": -1") "NonNegativeInteger"
              }

              test "bounds that round to 0 for integers are NonNegativeInteger" {
                  expectShape (integer ", \"minimum\": -0.5") "NonNegativeInteger"
                  expectShape (integer ", \"exclusiveMinimum\": -0.5") "NonNegativeInteger"
              }

              test "minimum 0 together with a weaker exclusiveMinimum is NonNegativeInteger" {
                  expectShape (integer ", \"minimum\": 0, \"exclusiveMinimum\": -5") "NonNegativeInteger"
              }

              test "the stricter of minimum and exclusiveMinimum decides" {
                  expectShape (integer ", \"minimum\": 0, \"exclusiveMinimum\": 3") "UnsupportedIntegerKeywords"
                  expectShape (integer ", \"minimum\": 4, \"exclusiveMinimum\": -1") "UnsupportedIntegerKeywords"
              }

              test "exclusiveMinimum 0 excludes 0 and is UnsupportedIntegerKeywords" {
                  expectShape (integer ", \"exclusiveMinimum\": 0") "UnsupportedIntegerKeywords"
              }

              test "any other lower bound is UnsupportedIntegerKeywords" {
                  expectShape (integer ", \"minimum\": 1") "UnsupportedIntegerKeywords"
                  expectShape (integer ", \"minimum\": -1") "UnsupportedIntegerKeywords"
              }

              // Draft 4 writes an exclusive bound as a boolean next to minimum.
              test "draft 4 minimum 0 with exclusiveMinimum true excludes 0 and is UnsupportedIntegerKeywords" {
                  expectShape (integer ", \"minimum\": 0, \"exclusiveMinimum\": true") "UnsupportedIntegerKeywords"
              }

              test "draft 4 minimum -1 with exclusiveMinimum true is NonNegativeInteger" {
                  expectShape (integer ", \"minimum\": -1, \"exclusiveMinimum\": true") "NonNegativeInteger"
              }

              test "minimum 0 with an upper bound or multipleOf is still NonNegativeInteger" {
                  expectShape (integer ", \"minimum\": 0, \"maximum\": 10") "NonNegativeInteger"
                  expectShape (integer ", \"minimum\": 0, \"exclusiveMaximum\": 10") "NonNegativeInteger"
                  expectShape (integer ", \"minimum\": 0, \"multipleOf\": 2") "NonNegativeInteger"
              }

              test "an upper bound alone is UnsupportedIntegerKeywords" {
                  expectShape (integer ", \"maximum\": 10") "UnsupportedIntegerKeywords"
              }

              test "IgnoreSpecificKeywords turns minimum 0 into UnsupportedIntegerKeywords" {
                  let flags = { defaultFlags with IgnoreSpecificKeywords = true }
                  Expect.equal (shapeName (classifyWith flags (integer ", \"minimum\": 0"))) "UnsupportedIntegerKeywords" ""
              }

              test "IgnoreSpecificKeywords leaves an integer without numeric keywords as AnyInteger" {
                  let flags = { defaultFlags with IgnoreSpecificKeywords = true }
                  Expect.equal (shapeName (classifyWith flags (integer ""))) "AnyInteger" ""
              } ]
