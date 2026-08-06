namespace JsonSchemaProvider

module Validation =

    type ValidationResult = bool
    type ValidationMessage = string
    type RuntimeValidation = ValidationResult * ValidationMessage list


    let validate validations : RuntimeValidation = 

        validations 
        |> List.fold ( fun (isValid, msg) validationFunc -> 
            let valid, messages = validationFunc
            isValid && valid, messages @ msg
        ) (true, [])

    let WasValid (validationResult: RuntimeValidation) : bool =
        fst validationResult
        

    

    

            