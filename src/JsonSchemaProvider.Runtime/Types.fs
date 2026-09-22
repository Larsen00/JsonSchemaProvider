namespace JsonSchemaProvider

// Every JSON Schema "type" keyword (object/array/integer/number/string/boolean) gets its own
// module here, each exposing a `Keywords` record split into:
//   - `common`  : keywords any schema node can have regardless of `type` (currently just Path -
//                 this node's own JSON Pointer position in the root schema; room for
//                 Title/Description/Default/Enum/Const later)
//   - `specific`: keywords only that `type` supports (the table JSON Schema itself draws between
//                 "core"/"metadata" keywords and per-type "validation" keywords)
module Common =
    type Keywords = {
        Path: string
        // A local promise only: true when this node's own JSON (not its children's) has nothing
        // stopping it from compiling, assuming every child also turns out compilable
        CanBeCompiled: bool
    }

module JsonObject =
    type Specific = {
        Required: Map<string, bool>
        MinProperties: int option
        MaxProperties: int option
        HasPatternProperties: bool
        AllowAdditionalProperties: bool
        HasAdditionalPropertiesSchema: bool
    }
    type Keywords = { common: Common.Keywords; specific: Specific }

module JsonArray =
    type Specific = {
        MinItems: int option
        MaxItems: int option
        UniqueItems: bool
        AllowAdditionalItems: bool
        HasAdditionalItemsSchema: bool
    }
    type Keywords = { common: Common.Keywords; specific: Specific }

module JsonNumber =
    // Shared by both "integer" and "number" - the numeric range/multipleOf keywords apply the
    // same way regardless of which of the two the schema declares. FSharpInt/FSharpDouble (the
    // FSharpType cases built from this) still track that distinction themselves; it's not a
    // keyword, so it doesn't live here.
    type Specific = {
        Minimum: float option
        Maximum: float option
        ExclusiveMinimum: float option
        ExclusiveMaximum: float option
        MultipleOf: float option
    }
    type Keywords = { common: Common.Keywords; specific: Specific }

module JsonString =
    type Specific = {
        MinLength: int option
        MaxLength: int option
        Pattern: string option
        Format: string option
    }
    type Keywords = { common: Common.Keywords; specific: Specific }

module JsonBoolean =
    // No boolean-specific validation keywords exist in JSON Schema - only `common` applies.
    type Keywords = { common: Common.Keywords }
