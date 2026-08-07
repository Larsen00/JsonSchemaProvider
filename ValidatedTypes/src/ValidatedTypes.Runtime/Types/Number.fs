namespace ValidatedTypes.DesignTime
open JsonSchemaProvider
open System

module Number =

    // Use a JSON schema to define the structure of the input for number validations
    [<Literal>]
    let numberValidationInputSchema =
        """
        {
            "type": "object",
            "properties": {
                "minimum": { "type": "number" },
                "maximum": { "type": "number" },
                "exclusiveMinimum": { "type": "number" },
                "exclusiveMaximum": { "type": "number" },
                "multipleOf": { "type": "number" }
            }
        }
        """


    // Define a type to represent the validations for a number
    type NumberValidations = JsonSchemaProvider<schema=numberValidationInputSchema>

    // Validate minimum
    let minimum (number: float) (validations: NumberValidations) =
        match validations.minimum with
        | None -> Ok number
        | Some min when number >= min -> Ok number
        | Some min -> Error [sprintf "Value %f is less than the minimum allowed value %f" number min]

    // Parse input JSON string into NumberValidations
    let parse (validationsJson: string) : Result<NumberValidations, list<string>> =
        try
            Ok <| NumberValidations.Parse validationsJson
        with
        | ex -> Error <| [sprintf "Invalid JSON schema for number validations. Hint (The json is validated against following schema): %s" numberValidationInputSchema; ex.Message]


    // Run the validations on a number. Deliberately takes a raw string, not
    // NumberValidations - NumberValidations is an erased provided type from
    // a different provider (JsonSchemaProvider), and must never appear in
    // the signature of anything called directly from another provider's
    // quotation (see notes/nested-type-provider-spike.md).
    let create (number: float) (validationsJson: string) : Result<float, list<string>> =
        match parse validationsJson with
        | Error errs -> Error errs
        | Ok validations -> minimum number validations

