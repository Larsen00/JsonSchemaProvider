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

    let toCompileTimeType (fSharpType: FSharpType) =
        fSharpTypeToCompileTimeType Map.empty fSharpType noFlags

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

    [<Tests>]
    let tests =
        testList
            "JsonSchemaProvider.Tests.TypeLevelConversionTests"
            [ oneOfSingleBranchYieldsPlainType
              oneOfTwoBranchesYieldsChoice
              oneOfThreeBranchesYieldsNestedChoice
              ]
