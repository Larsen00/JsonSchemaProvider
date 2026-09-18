namespace JsonSchemaProvider.DesignTime

module SchemaConversion =
    open NJsonSchema
    open JsonSchemaProvider
    open System
    open FSharp.Data


    // Jsonobject name
    type PropertyName = string

    // type Name = string
    type JsonSchemaType =
        | JsonObject of JsonObject.Keywords * List<PropertyName * JsonSchemaType>
        | JsonArray of JsonSchemaType * JsonArray.Keywords
        | JsonBoolean of JsonBoolean.Keywords
        | JsonInteger of JsonNumber.Keywords
        | JsonNumber of JsonNumber.Keywords
        | JsonString of JsonString.Keywords
        // TODO: None is missing from the specification
        | JsonOneOf of Common.Keywords * JsonSchemaType * JsonSchemaType list // Oneof has atleat one element


    // Annotation keywords, legal anywhere, never affecting validation - plus "$schema", always
    // injected by NJsonSchema's ToJson() regardless of source.
    let private alwaysIgnoredKeywords =
        set [ "title"; "description"; "default"; "examples"; "$comment"; "deprecated"; "$schema" ]

    // Keywords allowed without disqualifying a node from CanBeCompiled: structural ones, plus -
    // for arrays only - minItems/maxItems/uniqueItems/additionalItems, whose compilability is
    // combination-dependent and decided in TypeLevelConversion.fs instead. Every other type's
    // validation keywords (minimum, pattern, minProperties, ...) are never compiled at all, so
    // their mere presence disqualifies right here.
    let private knownKeywordsFor (schemaType: JsonObjectType) : Set<string> =
        match schemaType with
        | JsonObjectType.Array ->
            set [ "type"; "items"; "minItems"; "maxItems"; "uniqueItems"; "additionalItems" ]
        | JsonObjectType.Object -> set [ "type"; "properties"; "required" ]
        | JsonObjectType.String -> set [ "type" ]
        | JsonObjectType.Integer
        | JsonObjectType.Number -> set [ "type" ]
        | JsonObjectType.Boolean -> set [ "type" ]
        | JsonObjectType.None -> set [ "oneOf" ] // parseOneOf is the only caller that hits this case
        | _ -> Set.empty

    // True when this node's own JSON has no keyword outside knownKeywordsFor. Diffs ToJson()'s
    // top-level keys rather than reflecting over JsonSchema's ~75 .NET properties, since ToJson()
    // only round-trips keywords actually present - none of NJsonSchema's derived properties leak in.
    let private canBeCompiled (schema: JsonSchema) : bool =
        match JsonValue.Parse(schema.ToJson()) with
        | JsonValue.Record fields ->
            let known = knownKeywordsFor schema.Type
            fields |> Array.forall (fun (key, _) -> Set.contains key known || Set.contains key alwaysIgnoredKeywords)
        | _ -> false

    let rec private parseObject (rootSchema: JsonSchema) (schema: JsonSchema) (common: Common.Keywords) : JsonSchemaType =
        let isRequired (name: string) =
            schema.RequiredProperties.Contains name

        let initialSpecific : JsonObject.Specific =
            { Required = Map.empty
              // When minProperties/maxProperties are omitted, NJsonSchema defaults them to 0, same
              // as MinItems/MaxItems above.
              MinProperties = if schema.MinProperties > 0 then Some schema.MinProperties else None
              MaxProperties = if schema.MaxProperties > 0 then Some schema.MaxProperties else None
              HasPatternProperties = schema.PatternProperties.Count > 0
              AllowAdditionalProperties = schema.AllowAdditionalProperties
              HasAdditionalPropertiesSchema = not (isNull schema.AdditionalPropertiesSchema) }

        Seq.foldBack (fun (KeyValue(name, propertySchema)) (specific : JsonObject.Specific, properties') ->

            let propertyType : JsonSchemaType = parseJsonSchemaStructured rootSchema propertySchema

            { specific with Required = Map.add name (isRequired name) specific.Required },
            (name, propertyType) :: properties'


        ) schema.Properties (initialSpecific, [])
        |> fun (specific, properties) -> JsonObject({ common = common; specific = specific }, properties)

    and private parseArray (rootSchema: JsonSchema) (schema: JsonSchema) (common: Common.Keywords) : JsonSchemaType =
        let specific : JsonArray.Specific = {
            // When minItems is omitted, it defaults to 0 according to the JSON Schema specification
            MinItems = if schema.MinItems > 0 then Some schema.MinItems else None
            // When maxItems is omitted, NJsonSchema defaults it to 0, same as MinItems above
            MaxItems = if schema.MaxItems > 0 then Some schema.MaxItems else None
            UniqueItems = schema.UniqueItems
            // additionalItems only has meaning for tuple-form `items` (an array of schemas), which
            // this codebase doesn't model at all yet (schema.Item is always a single schema) - still
            // captured here so a schema author using it doesn't silently get treated as fully covered.
            AllowAdditionalItems = schema.AllowAdditionalItems
            HasAdditionalItemsSchema = not (isNull schema.AdditionalItemsSchema)
         }
        JsonArray(parseJsonSchemaStructured rootSchema schema.Item, { common = common; specific = specific })

    and private parseObjectType (rootSchema: JsonSchema) (schema: JsonSchema) : JsonSchemaType =
        // Path identifies this node's own position in rootSchema. We use it as a unique id in the
        // classMap and also to find this node's own subschema to validate against it.
        let common : Common.Keywords =
            { Path = JsonPathUtilities.GetJsonPath(rootSchema, schema)
              CanBeCompiled = canBeCompiled schema }

        let m x = x |> Option.ofNullable |> Option.map float

        let numericKeywords () : JsonNumber.Keywords =
            { common = common
              specific =
                { minimum = schema.Minimum |> m
                  maximum = schema.Maximum |> m
                  exclusiveMinimum = schema.ExclusiveMinimum |> m
                  exclusiveMaximum = schema.ExclusiveMaximum |> m
                  multipleOf = schema.MultipleOf |> m } }

        match schema.Type with
        | JsonObjectType.Array -> parseArray rootSchema schema common
        | JsonObjectType.Boolean -> JsonBoolean { common = common }
        | JsonObjectType.Integer -> JsonInteger(numericKeywords ())
        | JsonObjectType.Number -> JsonNumber(numericKeywords ())
        | JsonObjectType.Object -> parseObject rootSchema schema common
        | JsonObjectType.String ->
            let specific : JsonString.Specific = {
                minLength = schema.MinLength |> Option.ofNullable
                maxLength = schema.MaxLength |> Option.ofNullable
                pattern = schema.Pattern |> Option.ofObj
                format = schema.Format |> Option.ofObj
            }
            JsonString { common = common; specific = specific }
        | _ -> failwithf "Unsupported JSON object type %A." schema.Type

    and private parseOneOf (rootSchema: JsonSchema) (schema: JsonSchema) : JsonSchemaType =
        // Path identifies the oneOf node itself (not any one branch) - lets a nested oneOf be
        // validated as a whole via its own subschema, same as every other case.
        let common : Common.Keywords =
            { Path = JsonPathUtilities.GetJsonPath(rootSchema, schema)
              CanBeCompiled = canBeCompiled schema }
        schema.OneOf |> List.ofSeq |> List.map (parseJsonSchemaStructured rootSchema) |> fun l -> JsonOneOf (common, List.head l, List.tail l)

    and parseJsonSchemaStructured (rootSchema: JsonSchema) (schema: JsonSchema) : JsonSchemaType =

        match schema.Type with
        | JsonObjectType.None ->
            if schema.OneOf.Count > 0 then
                parseOneOf rootSchema schema
            else
                failwith "Unsupported JSON schema type None"

        | _ -> parseObjectType rootSchema schema

    let parseJsonSchema (input: string) : JsonSchemaType =
        let schema = SchemaCache.parseSchema input
        parseObjectType schema schema

    // Fsharp match types to the JsonProperty and JsonSchemaType types.
    type FSharpType =
        | FSharpClass of JsonObject.Keywords * List<PropertyName * FSharpType>
        | FSharpList of FSharpType * JsonArray.Keywords
        | FSharpDouble of JsonNumber.Keywords
        | FSharpInt of JsonNumber.Keywords
        | FSharpString of JsonString.Keywords
        | FSharpBool of JsonBoolean.Keywords
        | FSharpOneOf of Common.Keywords * FSharpType * FSharpType list

    // Conversion from the JsonSchemaType into a the eqalevant FSharpType
    let rec jsonSchemaTypeToFSharpType (jsonSchemaType: JsonSchemaType) : FSharpType =
        match jsonSchemaType with
        | JsonBoolean keywords -> FSharpBool keywords
        | JsonInteger keywords -> FSharpInt keywords
        | JsonNumber keywords -> FSharpDouble keywords
        | JsonString keywords -> FSharpString keywords
        | JsonObject (keywords, properties) ->
            // Convert the jsonshematype inside the properties into a fsharptype
            let properties' = List.map (fun (name, jsonSchemaType') -> name, jsonSchemaTypeToFSharpType jsonSchemaType') properties
            FSharpClass (keywords, properties')

        | JsonArray(innerType, keywords) ->
            let innerFSharpType = jsonSchemaTypeToFSharpType  innerType
            FSharpList(innerFSharpType, keywords)

        | JsonOneOf (keywords, head, tail) ->
            List.map jsonSchemaTypeToFSharpType (head :: tail)
            |> fun l -> FSharpOneOf (keywords, List.head l, List.tail l)

