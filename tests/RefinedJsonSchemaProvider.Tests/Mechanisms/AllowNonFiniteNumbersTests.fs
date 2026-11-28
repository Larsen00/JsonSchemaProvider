namespace RefinedJsonSchemaProvider.Tests

// A float can be nan, infinity or -infinity, which no JSON number matches; FSharp.Data writes
// them as JSON null. By default a number node is therefore validated, and Create gives Error for
// them. With allowNonFiniteNumbers=true a number node counts as fully compilable: Create gives the
// value directly and the non-finite value is written as JSON null.
module AllowNonFiniteNumbersTests =
    open Xunit
    open FSharp.Data
    open RefinedJsonSchemaProvider

    [<Literal>]
    let numberRootSchema = """{ "type": "number" }"""
    type NumberRoot = RefinedJsonSchemaProvider<schema=numberRootSchema>
    type UncheckedNumberRoot = RefinedJsonSchemaProvider<schema=numberRootSchema, allowNonFiniteNumbers=true>

    [<Fact>]
    let ``number root Create gives Ok for a finite float`` () =
        expectEqual (expectOk (NumberRoot.Create 1.5)) 1.5

    [<Fact>]
    let ``number root Create gives Error for nan and the infinities`` () =
        expectError (NumberRoot.Create nan)
        expectError (NumberRoot.Create infinity)
        expectError (NumberRoot.Create -infinity)

    [<Fact>]
    let ``with allowNonFiniteNumbers number root Create gives the value directly`` () =
        let v: float = UncheckedNumberRoot.Create 1.5
        expectEqual v 1.5

    [<Literal>]
    let requiredNumberSchema =
        """{ "type": "object", "properties": { "amount": {"type": "number"} }, "required": ["amount"] }"""
    type RequiredNumber = RefinedJsonSchemaProvider<schema=requiredNumberSchema>
    type UncheckedRequiredNumber = RefinedJsonSchemaProvider<schema=requiredNumberSchema, allowNonFiniteNumbers=true>

    [<Fact>]
    let ``object Create gives Error for nan in a required number`` () =
        expectError (RequiredNumber.Create(amount = nan))

    [<Fact>]
    let ``with allowNonFiniteNumbers object Create writes nan as JSON null`` () =
        let v: UncheckedRequiredNumber = UncheckedRequiredNumber.Create(amount = nan)
        expectEqual (v.JsonVal.ToString(JsonSaveOptions.DisableFormatting)) """{"amount":null}"""

    [<Literal>]
    let numberArraySchema = """{ "type": "array", "items": { "type": "number" }, "minItems": 1 }"""
    type NonEmptyNumbers = RefinedJsonSchemaProvider<schema=numberArraySchema>
    type UncheckedNonEmptyNumbers = RefinedJsonSchemaProvider<schema=numberArraySchema, allowNonFiniteNumbers=true>

    [<Fact>]
    let ``an array of numbers gives Error for an infinite item`` () =
        expectError (NonEmptyNumbers.Create((1.0, [ infinity ])))
        expectEqual (expectOk (NonEmptyNumbers.Create((1.0, [ 2.0 ])))) (1.0, [ 2.0 ])

    [<Fact>]
    let ``with allowNonFiniteNumbers an array of numbers keeps a plain Create`` () =
        let v: float * float list = UncheckedNonEmptyNumbers.Create((1.0, [ 2.0 ]))
        expectEqual v (1.0, [ 2.0 ])

    // A keyword number can't encode still needs validation, so Create keeps its Result.
    [<Literal>]
    let minimumSchema = """{ "type": "number", "minimum": 0 }"""
    type Minimum = RefinedJsonSchemaProvider<schema=minimumSchema, allowNonFiniteNumbers=true>

    [<Fact>]
    let ``a number with minimum still gives a Result`` () =
        expectError (Minimum.Create -1.0)
        expectOk (Minimum.Create 1.0) |> ignore
