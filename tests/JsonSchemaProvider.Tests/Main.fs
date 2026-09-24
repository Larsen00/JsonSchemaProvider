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
                [ NumberKeywordTests.tests
                  StringKeywordTests.tests
                  ArrayKeywordTests.tests
                  ObjectKeywordTests.tests
                  OneOfTests.tests
                  OneOfObjectBranchTests.tests
                  RootTypeTests.tests
                  SchemaFileTests.tests
                  SkipRuntimeValidationTests.tests
                  NestedValidationTests.tests
                  OldCodeGenerationTests.tests ])
