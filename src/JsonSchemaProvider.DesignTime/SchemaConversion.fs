namespace JsonSchemaProvider.DesignTime

module SchemaConversion =
    open NJsonSchema
    open JsonSchemaProvider
    open System


    // Jsonobject name
    type PropertyName = string

    // type Name = string
    type JsonSchemaType =
        | JsonObject of JsonObject.SpecificKeywords * List<PropertyName * JsonSchemaType>
        | JsonArray of JsonSchemaType * JsonArray.SpecificKeywords
        | JsonBoolean
        | JsonInteger of JsonInteger.SpecificKeywords
        | JsonNumber
        | JsonString
        // TODO: None is missing from the specification
        | JsonOneOf of JsonSchemaType list 


    let rec private parseObject (rootSchema: JsonSchema) (schema: JsonSchema) : JsonSchemaType =
        let isRequired (name: string) =
            schema.RequiredProperties.Contains name

        // Path identifies this object's own position in rootSchema 
        // We will use is a a unique id in the classMap and also to find the objs sub schema to validate against it.
        let path = JsonPathUtilities.GetJsonPath(rootSchema, schema)

        
        Seq.foldBack (fun (KeyValue(name, propertySchema)) (keywords : JsonObject.SpecificKeywords, properties') ->

            let propertyType : JsonSchemaType = parseJsonSchemaStructured rootSchema propertySchema

            { keywords with Required = Map.add name (isRequired name) keywords.Required },
            (name, propertyType) :: properties'


        ) schema.Properties ({ Required = Map.empty; Path = path }, [])
        |> JsonObject

    and private parseArray (rootSchema: JsonSchema) (schema: JsonSchema) : JsonSchemaType =
        let keywords : JsonArray.SpecificKeywords = {
            // When minItems is omitted, it defaults to 0 according to the JSON Schema specification
            MinItems = if schema.MinItems > 0 then Some schema.MinItems else None
         }
        JsonArray(parseJsonSchemaStructured rootSchema schema.Item, keywords)

    and private parseObjectType (rootSchema: JsonSchema) (schema: JsonSchema) : JsonSchemaType =
        match schema.Type with
        | JsonObjectType.Array -> parseArray rootSchema schema
        | JsonObjectType.Boolean -> JsonBoolean
        | JsonObjectType.Integer ->
            let m x = x |> Option.ofNullable |> Option.map float
            JsonInteger {
                minimum = schema.Minimum |> m
                maximum = schema.Maximum |> m
                exclusiveMinimum = schema.ExclusiveMinimum |> m
                exclusiveMaximum = schema.ExclusiveMaximum |> m
                multipleOf = schema.MultipleOf |> m
            }
        | JsonObjectType.Number -> JsonNumber
        | JsonObjectType.Object -> parseObject rootSchema schema
        | JsonObjectType.String -> JsonString
        | _ -> failwithf "Unsupported JSON object type %A." schema.Type

    and private parseOneOf (rootSchema: JsonSchema) (schema: JsonSchema) : JsonSchemaType =
        schema.OneOf |> List.ofSeq |> List.map (parseJsonSchemaStructured rootSchema) |> JsonOneOf

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
        | FSharpClass of JsonObject.SpecificKeywords * List<PropertyName * FSharpType> 
        | FSharpList of FSharpType * JsonArray.SpecificKeywords
        | FSharpDouble
        | FSharpInt of JsonInteger.SpecificKeywords
        | FSharpString
        | FSharpBool
        | FSharpOneOf of FSharpType list

    // Conversion from the JsonSchemaType into a the eqalevant FSharpType
    let rec jsonSchemaTypeToFSharpType (jsonSchemaType: JsonSchemaType) : FSharpType =
        match jsonSchemaType with
        | JsonBoolean -> FSharpBool
        | JsonInteger keywords -> FSharpInt keywords
        | JsonNumber -> FSharpDouble
        | JsonString -> FSharpString
        | JsonObject (keywords, properties) -> 
            // Convert the jsonshematype inside the properties into a fsharptype
            let properties' = List.map (fun (name, jsonSchemaType') -> name, jsonSchemaTypeToFSharpType jsonSchemaType') properties
            FSharpClass (keywords, properties')

        | JsonArray(innerType, keywords) ->
            let innerFSharpType = jsonSchemaTypeToFSharpType  innerType
            FSharpList(innerFSharpType, keywords)
            
        | JsonOneOf types -> 
            FSharpOneOf <| List.map jsonSchemaTypeToFSharpType types
            