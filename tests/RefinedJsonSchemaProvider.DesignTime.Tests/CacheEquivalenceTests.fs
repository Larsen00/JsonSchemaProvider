namespace RefinedJsonSchemaProvider.Tests

// The conversion cache is only an optimisation: with and without it the provider must generate the
// same types, signatures and invoke code. Checked as a property over random schemas.
module CacheEquivalenceTests =
    open System
    open Expecto
    open CacheSupport

    // Ok with the rendering of the generated types, or Error with the message if the provider fails.
    let private outcome (useCache: bool) (schemaText: string) : Result<string, string> =
        try
            Ok(describe (provide (flagsWithCache useCache) schemaText))
        with ex ->
            Error ex.Message

    // The first line where the two renderings differ, with a few lines of context, since a full
    // rendering runs to thousands of lines.
    let private firstDifference (withCache: Result<string, string>) (withoutCache: Result<string, string>) =
        let text r = match r with Ok s -> "Ok\n" + s | Error e -> "Error\n" + e
        let a = (text withCache).Split '\n'
        let b = (text withoutCache).Split '\n'
        let i = Seq.zip a b |> Seq.tryFindIndex (fun (x, y) -> x <> y) |> Option.defaultValue (min a.Length b.Length)
        let around (lines: string[]) = lines[max 0 (i - 3) .. min (lines.Length - 1) (i + 3)] |> String.concat "\n"
        $"first difference at line {i}\nwith cache:\n{around a}\nwithout cache:\n{around b}"

    let private schemaCount = 150

    [<Tests>]
    let tests =
        testList "Conversion cache" [
            test "with and without the cache the provider generates the same for random schemas" {
                let mutable generated = 0
                for seed in 0 .. schemaCount - 1 do
                    let schemaText = randomSchema (Random seed) 4
                    let withCache = outcome true schemaText
                    let withoutCache = outcome false schemaText
                    if withoutCache <> withCache then
                        failtestf "seed %d differs\n%s\nschema:\n%s" seed (firstDifference withCache withoutCache) schemaText
                    if Result.isOk withCache then generated <- generated + 1

                // Guards against a vacuous property where every random schema is rejected.
                Expect.isGreaterThan generated (schemaCount * 3 / 4) "most random schemas should generate types"
            }

            test "the rendering sees a difference in the generated types" {
                // Sanity check of the oracle: two different schemas must not render the same.
                let a = outcome true """{ "type": "object", "properties": { "x": { "type": "integer" } } }"""
                let b = outcome true """{ "type": "object", "properties": { "x": { "type": "string" } } }"""
                Expect.notEqual a b "describe should tell an int property from a string property"
            }
        ]
