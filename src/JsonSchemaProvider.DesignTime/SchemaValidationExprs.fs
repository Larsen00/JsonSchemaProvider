namespace JsonSchemaProvider.DesignTime

// Builds quotations that call the runtime validation in SchemaValidation (runtime assembly).
module SchemaValidationExprs =
    open FSharp.Quotations
    open FSharp.Data
    open JsonSchemaProvider.SchemaValidation

    // Returns an F# quotation expression that validates a JsonValue against the schema and returns a boolean indicating success.
    let validateJsonSchemaExpr (jsonValExpr: Expr) schemaHashCode schemaSource path =
        <@@ validateJsonSchema path (%%jsonValExpr: JsonValue) schemaHashCode schemaSource |> Result.isOk @@>
