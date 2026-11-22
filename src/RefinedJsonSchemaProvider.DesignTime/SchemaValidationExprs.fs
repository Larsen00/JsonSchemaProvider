namespace RefinedJsonSchemaProvider.DesignTime

// Builds quotations that call the runtime validation in SchemaValidation (runtime assembly).
module SchemaValidationExprs =
    open FSharp.Quotations
    open FSharp.Data
    open RefinedJsonSchemaProvider.SchemaValidation

    // A quotation that is true when the JsonValue is valid against the subschema at path.
    let validateJsonSchemaExpr (jsonValExpr: Expr) schemaHashCode schemaSource path =
        <@@ validateJsonSchema path (%%jsonValExpr: JsonValue) schemaHashCode schemaSource |> Result.isOk @@>
