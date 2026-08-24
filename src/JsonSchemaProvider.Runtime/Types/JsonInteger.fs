namespace JsonSchemaProvider

module JsonInteger =
    open System
    open FSharp.Data
    open JsonSchemaProvider.Validation

    // Represents all the keywords that contrain the array type in JSON Schema.
    type SpecificKeywords = {
        minimum: float option
        maximum: float option
        exclusiveMinimum: float option
        exclusiveMaximum: float option
        multipleOf: float option
    }
