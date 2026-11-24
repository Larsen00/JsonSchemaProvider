namespace RefinedJsonSchemaProvider.DesignTime

module SchemaConversion =
    open NJsonSchema
    open RefinedJsonSchemaProvider
    open System
    open FSharp.Data


    // The name of a property in a JSON object.
    type PropertyName = string

    // #region json-oneof
    // The intermediate representation of a schema node, one constructor per kind of node.
    type JsonSchemaNode =
        // #elide-start json-oneof
        | JsonObject of JsonObject.Keywords * List<PropertyName * JsonSchemaNode>
        | JsonArray of JsonSchemaNode * JsonArray.Keywords
        | JsonBoolean of JsonBoolean.Keywords
        | JsonInteger of JsonNumber.Keywords
        | JsonNumber of JsonNumber.Keywords
        | JsonString of JsonString.Keywords
        // #elide-end json-oneof
        // #omit-start PropertyName+JsonSchemaNode
        // TODO: None is missing from the specification
        | JsonOneOf of Common.Keywords * JsonSchemaNode * JsonSchemaNode list // head and tail: oneOf has at least one branch
        // #omit-end PropertyName+JsonSchemaNode
    // #endregion json-oneof


    // Annotations, which never affect validation, plus "$schema", which NJsonSchema's ToJson always adds.
    let private alwaysIgnoredKeywords =
        set [ "title"; "description"; "default"; "examples"; "$comment"; "deprecated"; "$schema" ]

    // The keywords a compiled node may have; array sizes and integer bounds are judged later by their shape.
    let private knownKeywordsFor (schemaType: JsonObjectType) : Set<string> =
        match schemaType with
        | JsonObjectType.Array ->
            set [ "type"; "items"; "minItems"; "maxItems"; "uniqueItems"; "additionalItems" ]
        | JsonObjectType.Object -> set [ "type"; "properties"; "required" ]
        | JsonObjectType.String -> set [ "type" ]
        | JsonObjectType.Integer -> set [ "type"; "minimum"; "exclusiveMinimum"; "maximum"; "exclusiveMaximum" ]
        | JsonObjectType.Number -> set [ "type" ]
        | JsonObjectType.Boolean -> set [ "type" ]
        | JsonObjectType.None -> set [ "oneOf" ] // only a oneOf node has no type here
        | _ -> Set.empty

    // True when the node's own JSON has no keyword outside knownKeywordsFor and the annotations.
    let private canBeCompiled (schema: JsonSchema) : bool =
        match JsonValue.Parse(schema.ToJson()) with
        | JsonValue.Record fields ->
            let known = knownKeywordsFor schema.Type
            fields |> Array.forall (fun (key, _) -> Set.contains key known || Set.contains key alwaysIgnoredKeywords)
        | _ -> false

    // One contract resolver for every GetJsonPath call. Without it NJsonSchema builds a new one per
    let private contractResolver = Newtonsoft.Json.Serialization.DefaultContractResolver()

    // Parses an object node, its properties and which of them are required.
    let rec private parseObject (rootSchema: JsonSchema) (schema: JsonSchema) (common: Common.Keywords) : JsonSchemaNode =
        let isRequired (name: string) =
            schema.RequiredProperties.Contains name

        let initialSpecific : JsonObject.Specific =
            { Required = Map.empty
              // NJsonSchema stores an omitted minProperties/maxProperties as 0.
              MinProperties = if schema.MinProperties > 0 then Some schema.MinProperties else None
              MaxProperties = if schema.MaxProperties > 0 then Some schema.MaxProperties else None
              HasPatternProperties = schema.PatternProperties.Count > 0
              AllowAdditionalProperties = schema.AllowAdditionalProperties
              HasAdditionalPropertiesSchema = not (isNull schema.AdditionalPropertiesSchema) }

        Seq.foldBack (fun (KeyValue(name, propertySchema)) (specific : JsonObject.Specific, properties') ->

            let propertyNode : JsonSchemaNode = parseJsonSchemaStructured rootSchema propertySchema

            { specific with Required = Map.add name (isRequired name) specific.Required },
            (name, propertyNode) :: properties'


        ) schema.Properties (initialSpecific, [])
        |> fun (specific, properties) -> JsonObject({ common = common; specific = specific }, properties)

    // Parses an array node, its item schema and its size keywords.
    and private parseArray (rootSchema: JsonSchema) (schema: JsonSchema) (common: Common.Keywords) : JsonSchemaNode =
        let specific : JsonArray.Specific = {
            // An omitted minItems is 0 by the specification, which allows every length.
            MinItems = if schema.MinItems > 0 then Some schema.MinItems else None
            // NJsonSchema stores an omitted maxItems as 0.
            MaxItems = if schema.MaxItems > 0 then Some schema.MaxItems else None
            UniqueItems = schema.UniqueItems
            // Only meaningful for tuple-form items, which isn't supported, so it keeps the node from compiling.
            AllowAdditionalItems = schema.AllowAdditionalItems
            HasAdditionalItemsSchema = not (isNull schema.AdditionalItemsSchema)
         }
        JsonArray(parseJsonSchemaStructured rootSchema schema.Item, { common = common; specific = specific })

    // Parses a node by its type keyword.
    and private parseObjectType (rootSchema: JsonSchema) (schema: JsonSchema) : JsonSchemaNode =
        // The Path keys the type map and finds the node's subschema for validation.
        let common : Common.Keywords =
            { Path = JsonPathUtilities.GetJsonPath(rootSchema, schema, contractResolver)
              CanBeCompiled = canBeCompiled schema }

        let m x = x |> Option.ofNullable

        // Draft 4's boolean exclusiveMinimum/exclusiveMaximum is rewritten to the draft 6 numeric form.
        let numericKeywords () : JsonNumber.Keywords =
            { common = common
              specific =
                { Minimum = if schema.IsExclusiveMinimum then None else schema.Minimum |> m
                  Maximum = if schema.IsExclusiveMaximum then None else schema.Maximum |> m
                  ExclusiveMinimum = if schema.IsExclusiveMinimum then schema.Minimum |> m else schema.ExclusiveMinimum |> m
                  ExclusiveMaximum = if schema.IsExclusiveMaximum then schema.Maximum |> m else schema.ExclusiveMaximum |> m
                  MultipleOf = schema.MultipleOf |> m } }

        match schema.Type with
        | JsonObjectType.Array -> parseArray rootSchema schema common
        | JsonObjectType.Boolean -> JsonBoolean { common = common }
        | JsonObjectType.Integer -> JsonInteger(numericKeywords ())
        | JsonObjectType.Number -> JsonNumber(numericKeywords ())
        | JsonObjectType.Object -> parseObject rootSchema schema common
        | JsonObjectType.String ->
            let specific : JsonString.Specific = {
                MinLength = schema.MinLength |> Option.ofNullable
                MaxLength = schema.MaxLength |> Option.ofNullable
                Pattern = schema.Pattern |> Option.ofObj
                Format = schema.Format |> Option.ofObj
            }
            JsonString { common = common; specific = specific }
        | _ -> failwithf "Unsupported JSON object type %A." schema.Type

    // Parses a oneOf node and its branches.
    and private parseOneOf (rootSchema: JsonSchema) (schema: JsonSchema) : JsonSchemaNode =
        // The Path of the oneOf node itself, so a nested oneOf is validated as a whole.
        let common : Common.Keywords =
            { Path = JsonPathUtilities.GetJsonPath(rootSchema, schema, contractResolver)
              CanBeCompiled = canBeCompiled schema }
        schema.OneOf |> List.ofSeq |> List.map (parseJsonSchemaStructured rootSchema) |> fun l -> JsonOneOf (common, List.head l, List.tail l)

    // Parses a schema node; a node without a type must be a oneOf.
    and parseJsonSchemaStructured (rootSchema: JsonSchema) (schema: JsonSchema) : JsonSchemaNode =

        match schema.Type with
        | JsonObjectType.None ->
            if schema.OneOf.Count > 0 then
                parseOneOf rootSchema schema
            else
                failwith "Unsupported JSON schema type None"

        | _ -> parseObjectType rootSchema schema

    // Parses schema text into the intermediate representation.
    let parseJsonSchema (input: string) : JsonSchemaNode =
        let schema = SchemaCache.parseSchema input
        parseJsonSchemaStructured schema schema
