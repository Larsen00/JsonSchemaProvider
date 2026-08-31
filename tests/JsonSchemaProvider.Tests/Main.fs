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
                "JsonSchemaProvider.Tests"
                [ JsonSchemaProviderTests.tests
                  JsonSchemaProviderTestsWithConstrains.tests
                  ArrayTests.tests
                  OneOfTests.tests
                  RootTypeTests.tests ])