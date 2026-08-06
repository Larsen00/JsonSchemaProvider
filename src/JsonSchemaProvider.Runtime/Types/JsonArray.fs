namespace JsonSchemaProvider

module JsonArray =
    open System
    open FSharp.Data
    open JsonSchemaProvider.Validation

    // Represents all the keywords that contrain the array type in JSON Schema.
    type SpecificKeywords = {
        MinItems: int option
        // ... other keywords can be added here
    }



    // Runtime validation functions for arrays based on JSON Schema type specific keywords.
    let validateMinItems (arr: Array) (arrayKeywords: SpecificKeywords) =
        match arrayKeywords.MinItems with
        | Some minItems when arr.Length < minItems ->
                let msg = sprintf "Array has %d items, but minimum is %d" arr.Length minItems
                false, [msg]
        | _ -> true, []

    let dummyValidation (arr: Array) (arrayKeywords: SpecificKeywords) =
        // Placeholder for other validations
        true, []

    let validateJsonValue (arr: JsonValue array) (arrayKeywords: SpecificKeywords) =
        let validations = [
            validateMinItems arr arrayKeywords
            dummyValidation arr arrayKeywords
        ]
        validations |> validate |> WasValid




    




