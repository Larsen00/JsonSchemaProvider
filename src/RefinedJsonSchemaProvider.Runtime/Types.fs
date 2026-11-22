namespace RefinedJsonSchemaProvider

// One module per JSON type, each with a Keywords record of common and type-specific keywords.
// #region common-keywords
module Common =
    type Keywords = {
        // #elide-start
        Path: string
        // True when the node's own keywords, not its children's, need no runtime validation.
        // #elide-end
        CanBeCompiled: bool
    }
// #endregion

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
    // Shared by integer and number nodes; decimal, as in NJsonSchema, keeps 64-bit bounds exact.
    type Specific = {
        Minimum: decimal option
        Maximum: decimal option
        ExclusiveMinimum: decimal option
        ExclusiveMaximum: decimal option
        MultipleOf: decimal option
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
    // JSON Schema has no boolean-specific keywords, so only common applies.
    type Keywords = { common: Common.Keywords }
