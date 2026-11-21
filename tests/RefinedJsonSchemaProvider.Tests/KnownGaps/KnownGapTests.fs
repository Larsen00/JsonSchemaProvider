namespace RefinedJsonSchemaProvider.Tests

// Every known gap, in one place: each test pins what the provider does today where the spec
// asks for something else. A failing test here means a gap got closed: flip the assertion to
// the spec's behaviour and move the test to the file for its keyword or mechanism.
// Gaps that stop the provider from compiling a schema at all are in the design-time project's
// KnownGaps/KnownGapTests.fs.
module KnownGapTests =
    open Xunit
    open RefinedJsonSchemaProvider
    open Draft2020KeywordTests

    // -- const: NJsonSchema 11's Validate ignores it. Workaround: enum with one value. --

    [<Literal>]
    let stringConstPath = __SOURCE_DIRECTORY__ + "/../schemas/OldCodeGenerationTests/03-string-const.json"
    type StringConst = RefinedJsonSchemaProvider<schemaFile=stringConstPath>

    [<Fact>]
    let ``const: Create accepts a non-matching value`` () =
        expectEqual (expectOk (StringConst.Create "Robert Jordan")) "Robert Jordan"

    // -- 2019-09/2020-12 keywords: NJsonSchema 11's Validate ignores them --

    [<Fact>]
    let ``contains: Create accepts an array without a match`` () = expectOk (ContainsArray.Create [ 2; 3 ]) |> ignore

    [<Fact>]
    let ``contains: Parse accepts an array without a match`` () = expectOk (ContainsArray.Parse "[2, 3]") |> ignore

    [<Fact>]
    let ``minContains: Create accepts too few matches`` () = expectOk (MinContainsArray.Create [ 1; 2 ]) |> ignore

    [<Fact>]
    let ``maxContains: Create accepts too many matches`` () = expectOk (MaxContainsArray.Create [ 1; 1 ]) |> ignore

    [<Fact>]
    let ``prefixItems: Create accepts a wrong first element`` () = expectOk (PrefixItemsArray.Create [ 2; 3 ]) |> ignore

    [<Fact>]
    let ``dependentRequired: Create accepts a without b`` () = expectOk (DependentRequiredObject.Create(a = "x")) |> ignore

    [<Fact>]
    let ``dependentSchemas: Create accepts a without b`` () = expectOk (DependentSchemasObject.Create(a = "x")) |> ignore

    [<Fact>]
    let ``propertyNames: Create accepts a too long property name`` () = expectOk (PropertyNamesObject.Create(abc = "x")) |> ignore

    [<Fact>]
    let ``unevaluatedProperties: Parse accepts an extra property`` () =
        expectOk (UnevaluatedPropertiesObject.Parse """{ "a": "x", "z": 1 }""") |> ignore
