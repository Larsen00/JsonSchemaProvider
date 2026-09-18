namespace JsonSchemaProvider.DesignTime

// Schema validation helpers shared by every "does this JsonValue satisfy this schema node"
// check - the Create() validation path and oneOf's own branch-matching.
module SchemaValidationExprs =
    open FSharp.Quotations
    open FSharp.Data
    open JsonSchemaProvider

    // Returns the schema errors for jsonText, empty when it's valid.MethodAccessException.
    let collectValidationErrors (path: string) (jsonText: string) (schemaHashCode: int32) (schemaSource: string) : string list =
        let rootschema = SchemaCache.retrieveSchema schemaHashCode schemaSource

        // This allow us to validate a nested class on .create if the path is '#' then we are at the root.
        let subschema =
            if path = "#" then rootschema
            else SchemaCache.resolveByPath rootschema path

        subschema.Validate jsonText
        |> Seq.map (fun validationError -> validationError.ToString())
        |> Seq.toList

    // Validates a JsonValue against the schema and returns a Result indicating success or failure.
    let validateJsonSchema path record (schemaHashCode: int32) (schemaSource: string) =
        match collectValidationErrors path (record.ToString()) schemaHashCode schemaSource with
        | [] -> Ok record
        | errors -> Error errors

    // Returns an F# quotation expression that validates a JsonValue against the schema and returns a boolean indicating success.
    let validateJsonSchemaExpr (jsonValExpr: Expr) schemaHashCode schemaSource path =
        <@@ validateJsonSchema path (%%jsonValExpr: JsonValue) schemaHashCode schemaSource |> Result.isOk @@>
