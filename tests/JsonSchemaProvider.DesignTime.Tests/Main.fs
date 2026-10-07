namespace JsonSchemaProvider.Tests

module Main =
    open JsonSchemaProvider.Tests
    open Expecto

    [<EntryPoint>]
    let main args =
        runTestsWithCLIArgs
            []
            args
            (testList
                "JsonSchemaProvider.DesignTime.Tests"
                [ SchemaConversionTests.tests
                  SchemaKeywordTests.tests
                  ArrayShapeTests.tests
                  NodeConversionsTests.tests
                  ConversionTests.tests
                  ExprGeneratorTests.tests
                  TypeProviderTests.tests
                  NJsonSchemaTests.tests ])
