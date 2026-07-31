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
                [ SchemaConversionTests.tests; TypeLevelConversionTests.tests; NJsonSchemaTests.tests ])
