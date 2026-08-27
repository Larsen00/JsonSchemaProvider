namespace JsonSchemaProvider.DesignTime

module JsonIntegerProvidedType =
    open System
    open JsonSchemaProvider

    let canBeCompiled (keywords: JsonInteger.SpecificKeywords) =
        keywords.minimum.IsNone && 
        keywords.maximum.IsNone &&
        keywords.exclusiveMaximum.IsNone &&
        keywords.exclusiveMinimum.IsNone &&
        keywords.multipleOf.IsNone

        
