namespace JsonSchemaProvider.Tests

module TypeLevelConversionTests =
    open Expecto
    open JsonSchemaProvider.DesignTime.SchemaConversion
    open JsonSchemaProvider.DesignTime.TypeLevelConversion
    open JsonSchemaProvider.DesignTime.ProviderConfiguration

    
    let commonKeywords : JsonSchemaProvider.Common.Keywords = { Path = "#" }

    let jsonIntegerNoneKeywords : JsonSchemaProvider.JsonNumber.Keywords =
      { common = commonKeywords
        specific =
          { minimum = None
            maximum = None
            exclusiveMinimum = None
            exclusiveMaximum = None
            multipleOf = None } }

    let jsonStringKeywords : JsonSchemaProvider.JsonString.Keywords =
      { common = commonKeywords
        specific = { minLength = None; maxLength = None; pattern = None; format = None } }

    let jsonBooleanKeywords : JsonSchemaProvider.JsonBoolean.Keywords = { common = commonKeywords }


    let noFlags = { CompileMinItems = false }

    let private dummyContext : GenerationContext =
        { Assembly = System.Reflection.Assembly.GetExecutingAssembly()
          NamespaceName = "Test"
          RuntimeType = typeof<obj>
          SchemaHashCode = 0
          SchemaString = "{}"
          CompileFlags = noFlags }

    let toCompileTimeType (fSharpType: FSharpType) =
        (convert dummyContext Map.empty fSharpType).CompileTimeType

    let oneOfSingleBranchYieldsPlainType =
        test "oneOf with a single branch yields the branch type directly" {
            let actual = toCompileTimeType (FSharpOneOf (commonKeywords, FSharpInt jsonIntegerNoneKeywords, []))
            Expect.equal actual typeof<int> "single-branch oneOf should not be wrapped in Choice"
        }

    let oneOfTwoBranchesYieldsChoice =
        test "oneOf with two branches yields Choice<T1,T2>" {
            let actual = toCompileTimeType (FSharpOneOf (commonKeywords, FSharpInt jsonIntegerNoneKeywords, [FSharpString jsonStringKeywords ]))
            Expect.equal actual typeof<Choice<int, string>> "two-branch oneOf should be Choice<int,string>"
        }

    let oneOfThreeBranchesYieldsNestedChoice =
        test "oneOf with three branches yields Choice<T1, Choice<T2,T3>>" {
            let actual = toCompileTimeType (FSharpOneOf (commonKeywords, FSharpInt jsonIntegerNoneKeywords, [FSharpString jsonStringKeywords; FSharpBool jsonBooleanKeywords ]))
            Expect.equal actual typeof<Choice<int, Choice<string, bool>>> "three-branch oneOf should nest"
        }

    let jsonArrayKeywords (minItems: int option) (maxItems: int option) : JsonSchemaProvider.JsonArray.Keywords =
        { common = commonKeywords
          specific = { MinItems = minItems; MaxItems = maxItems } }

    // This can only be tested here, not via ArrayTests.fs: a schema with maxItems < minItems makes
    // buildArrayConversion fail while the type provider is generating types, i.e. it would fail to
    // *compile* a `type Bad = JsonSchemaProvider<schema=...>` declaration rather than raise
    // something Expect.throws could wrap around a running Create/Parse call.
    let maxItemsLessThanMinItemsThrows =
        test "buildArrayConversion rejects a schema where maxItems < minItems" {
            let arrayType = FSharpList(FSharpInt jsonIntegerNoneKeywords, jsonArrayKeywords (Some 3) (Some 2))
            Expect.throws (fun () -> toCompileTimeType arrayType |> ignore) "maxItems < minItems should fail fast, not silently produce a type"
        }

    [<Tests>]
    let tests =
        testList
            "JsonSchemaProvider.Tests.TypeLevelConversionTests"
            [ oneOfSingleBranchYieldsPlainType
              oneOfTwoBranchesYieldsChoice
              oneOfThreeBranchesYieldsNestedChoice
              maxItemsLessThanMinItemsThrows
              ]
