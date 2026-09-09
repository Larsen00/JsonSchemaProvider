namespace JsonSchemaProvider

// Every JSON Schema "type" keyword (object/array/integer/number/string/boolean) gets its own
// module here, each exposing a `Keywords` record split into:
//   - `common`  : keywords any schema node can have regardless of `type` (currently just Path -
//                 this node's own JSON Pointer position in the root schema; room for
//                 Title/Description/Default/Enum/Const later)
//   - `specific`: keywords only that `type` supports (the table JSON Schema itself draws between
//                 "core"/"metadata" keywords and per-type "validation" keywords)
module Common =
    type Keywords = { Path: string }

module JsonObject =
    type Specific = { Required: Map<string, bool> }
    type Keywords = { common: Common.Keywords; specific: Specific }

module JsonArray =
    open System
    open FSharp.Data
    open JsonSchemaProvider.Validation

    type Specific = {
        MinItems: int option
        // ... other keywords can be added here
    }
    type Keywords = { common: Common.Keywords; specific: Specific }

    // Runtime validation functions for arrays based on JSON Schema type specific keywords.
    let validateMinItems (arr: Array) (arrayKeywords: Keywords) =
        match arrayKeywords.specific.MinItems with
        | Some minItems when arr.Length < minItems ->
                let msg = sprintf "Array has %d items, but minimum is %d" arr.Length minItems
                false, [msg]
        | _ -> true, []

    let dummyValidation (arr: Array) (arrayKeywords: Keywords) =
        // Placeholder for other validations
        true, []

    let validateJsonValue (arr: JsonValue array) (arrayKeywords: Keywords) =
        let validations = [
            validateMinItems arr arrayKeywords
            dummyValidation arr arrayKeywords
        ]
        validations |> validate |> WasValid

module JsonNumber =
    // Shared by both "integer" and "number" - the numeric range/multipleOf keywords apply the
    // same way regardless of which of the two the schema declares. FSharpInt/FSharpDouble (the
    // FSharpType cases built from this) still track that distinction themselves; it's not a
    // keyword, so it doesn't live here.
    type Specific = {
        minimum: float option
        maximum: float option
        exclusiveMinimum: float option
        exclusiveMaximum: float option
        multipleOf: float option
    }
    type Keywords = { common: Common.Keywords; specific: Specific }

module JsonString =
    type Specific = {
        minLength: int option
        maxLength: int option
        pattern: string option
        format: string option
    }
    type Keywords = { common: Common.Keywords; specific: Specific }

module JsonBoolean =
    // No boolean-specific validation keywords exist in JSON Schema - only `common` applies.
    type Keywords = { common: Common.Keywords }
