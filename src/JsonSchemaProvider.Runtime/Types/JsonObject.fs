namespace JsonSchemaProvider

module JsonObject =

    type SpecificKeywords = { 
        Required: Map<string, bool>
        Path: string
    }