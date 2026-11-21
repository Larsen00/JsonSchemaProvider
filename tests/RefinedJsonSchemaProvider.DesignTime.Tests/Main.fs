namespace RefinedJsonSchemaProvider.Tests

module Main =
    open RefinedJsonSchemaProvider.Tests
    open Expecto

    [<EntryPoint>]
    let main args =
        runTestsWithCLIArgs
            []
            args
            (testList
                "RefinedJsonSchemaProvider.DesignTime.Tests"
                [ SchemaConversionTests.tests
                  SchemaKeywordTests.tests
                  ArrayShapeTests.tests
                  IntegerShapeTests.tests
                  IntegerShapeTests.strictTests
                  NodeConversionsTests.tests
                  ConversionTests.tests
                  ExprGeneratorTests.tests
                  TypeProviderTests.tests
                  NJsonSchemaTests.tests
                  CacheEquivalenceTests.tests ])
