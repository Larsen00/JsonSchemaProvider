namespace JsonSchemaProvider.Tests

module TypeLevelConversionTests =
    open Expecto
    open JsonSchemaProvider.DesignTime.SchemaConversion
    open JsonSchemaProvider.DesignTime.TypeLevelConversion
    open JsonSchemaProvider.DesignTime.ProviderConfiguration

    let noFlags = { CompileMinItems = false }

    let toCompileTimeType (fSharpType: FSharpType) =
        fSharpTypeToCompileTimeType Map.empty fSharpType noFlags

    let oneOfSingleBranchYieldsPlainType =
        test "oneOf with a single branch yields the branch type directly" {
            let actual = toCompileTimeType (FSharpOneOf [ FSharpInt ])
            Expect.equal actual typeof<int> "single-branch oneOf should not be wrapped in Choice"
        }

    let oneOfTwoBranchesYieldsChoice =
        test "oneOf with two branches yields Choice<T1,T2>" {
            let actual = toCompileTimeType (FSharpOneOf [ FSharpInt; FSharpString ])
            Expect.equal actual typeof<Choice<int, string>> "two-branch oneOf should be Choice<int,string>"
        }

    let oneOfThreeBranchesYieldsNestedChoice =
        test "oneOf with three branches yields Choice<T1, Choice<T2,T3>>" {
            let actual = toCompileTimeType (FSharpOneOf [ FSharpInt; FSharpString; FSharpBool ])
            Expect.equal actual typeof<Choice<int, Choice<string, bool>>> "three-branch oneOf should nest"
        }

    let oneOfEmptyThrows =
        test "oneOf with no branches throws" {
            Expect.throws (fun () -> toCompileTimeType (FSharpOneOf []) |> ignore) "empty oneOf should throw"
        }

    [<Tests>]
    let tests =
        testList
            "JsonSchemaProvider.Tests.TypeLevelConversionTests"
            [ oneOfSingleBranchYieldsPlainType
              oneOfTwoBranchesYieldsChoice
              oneOfThreeBranchesYieldsNestedChoice
              oneOfEmptyThrows ]
