namespace JsonSchemaProvider.DesignTime

module JsonIntegerProvidedType =
    open System
    open JsonSchemaProvider

    let canBeCompiled (keywords: JsonNumber.Keywords) =
        keywords.specific.minimum.IsNone &&
        keywords.specific.maximum.IsNone &&
        keywords.specific.exclusiveMaximum.IsNone &&
        keywords.specific.exclusiveMinimum.IsNone &&
        keywords.specific.multipleOf.IsNone

        
