namespace JsonSchemaProvider

// Validation called by the generated code at runtime, so it must live in the runtime assembly.
module SchemaValidation =
    open FSharp.Data

    // The first integer in jsonText outside int64. Newtonsoft reads one as a BigInteger, which NJsonSchema's Validate throws on.
    let private tryFindIntegerOutsideInt64 (jsonText: string) : System.Numerics.BigInteger option =
        use reader = new Newtonsoft.Json.JsonTextReader(new System.IO.StringReader(jsonText))
        reader.DateParseHandling <- Newtonsoft.Json.DateParseHandling.None

        let rec scan () =
            if not (reader.Read()) then None
            else
                match reader.Value with
                | :? System.Numerics.BigInteger as number -> Some number
                | _ -> scan ()

        scan ()

    // Reported instead of NJsonSchema's exception; a number node with an integer literal this large hits it too.
    let private outsideInt64Message (number: System.Numerics.BigInteger) : string =
        $"The JSON contains the integer %s{number.ToString()}, which is outside the int64 range (-9223372036854775808 to 9223372036854775807). "
        + "NJsonSchema can't validate integers that large, so the JSON can't be checked against the schema.\n"
        + "Pick ONE of these fixes:\n"
        + "  A) Build the value with Create on a node that needs no validation: an integer whose bounds are exactly its type's range "
        + "(e.g. \"minimum\": 0, \"maximum\": 18446744073709551615 for uint64), or a number without validation keywords. "
        + "Parse always validates, so it can't take this number.\n"
        + "  B) Or set skipRuntimeValidation=true. Warning: then no JSON is validated against the schema, "
        + "and JSON of the wrong shape can raise when it is converted."

    // Returns the schema errors for jsonText, empty when it's valid.
    let collectValidationErrors (path: string) (jsonText: string) (schemaHashCode: int32) (schemaSource: string) : string list =
        let rootschema = SchemaCache.retrieveSchema schemaHashCode schemaSource

        // This allow us to validate a nested class on .create if the path is '#' then we are at the root.
        let subschema =
            if path = "#" then rootschema
            else SchemaCache.resolveByPath rootschema path

        // Only a real throw is reported: a large integer the schema doesn't describe (an extra property) validates fine.
        try
            subschema.Validate jsonText
            |> Seq.map (fun validationError -> validationError.ToString())
            |> Seq.toList
        with :? System.InvalidCastException ->
            match tryFindIntegerOutsideInt64 jsonText with
            | Some number -> [ outsideInt64Message number ]
            | None -> reraise ()

    // Validates a JsonValue against the schema and returns a Result indicating success or failure.
    let validateJsonSchema path record (schemaHashCode: int32) (schemaSource: string) =
        match collectValidationErrors path (record.ToString()) schemaHashCode schemaSource with
        | [] -> Ok record
        | errors -> Error errors

    // Parse's entry point under SkipRuntimeValidation: syntax check only, no schema validation.
    // Malformed JSON is still an Error, but JSON of the wrong shape passes through and can raise
    // later, when converted to the typed value.
    let parseOnly (jsonText: string) : Result<JsonValue, string list> =
        match JsonValue.TryParse jsonText with
        | None -> Error [ "Invalid JSON: " + jsonText ]
        | Some jsonValue -> Ok jsonValue

    // Parse's default entry point: syntactically parses jsonText, then validates it against the
    // sub-schema at path ("#" for the root). Malformed JSON is reported as an Error too, so Parse never raises.
    let parseAndValidate (path: string) (jsonText: string) (schemaHashCode: int32) (schemaSource: string) : Result<JsonValue, string list> =
        match parseOnly jsonText with
        | Error errors -> Error errors
        | Ok jsonValue ->
            match collectValidationErrors path jsonText schemaHashCode schemaSource with
            | [] -> Ok jsonValue
            | errors -> Error errors
