namespace RefinedJsonSchemaProvider

open System
open System.Globalization
open FSharp.Data

// Instance equality from the JSON Schema spec, which JsonValue's own equality doesn't match.
module JsonEquality =
    // The exact decimal of a double via its round-trip text; None when decimal can't hold it.
    let private exactDecimal (number: double) : decimal option =
        match Decimal.TryParse(number.ToString("R", CultureInfo.InvariantCulture), NumberStyles.Float, CultureInfo.InvariantCulture) with
        | true, parsed when parsed <> 0m || number = 0.0 -> Some parsed
        | _ -> None

    // Sorts object properties by name and turns floats into decimals, so equal instances become equal JsonValues.
    let rec canonical (value: JsonValue) : JsonValue =
        match value with
        | JsonValue.Record properties ->
            properties
            |> Array.map (fun (name, propertyValue) -> name, canonical propertyValue)
            |> Array.sortBy fst
            |> JsonValue.Record
        | JsonValue.Array items -> JsonValue.Array(Array.map canonical items)
        | JsonValue.Float number ->
            match exactDecimal number with
            | Some parsed -> JsonValue.Number parsed
            | None -> value
        | other -> other

    // Drops every item equal to an earlier one, keeping each first occurrence as it was.
    let distinctItems (array: JsonValue) : JsonValue =
        match array with
        | JsonValue.Array items -> JsonValue.Array(Array.distinctBy canonical items)
        | other -> other
