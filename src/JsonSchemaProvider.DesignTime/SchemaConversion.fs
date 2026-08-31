namespace JsonSchemaProvider.DesignTime

module SchemaConversion =
    open NJsonSchema
    open JsonSchemaProvider
    open System


    // Jsonobject name
    type PropertyName = string

    // type Name = string
    type JsonSchemaType =
        | JsonObject of List<PropertyName * JsonObject.SpecificKeywords * JsonSchemaType>
        | JsonArray of JsonSchemaType * JsonArray.SpecificKeywords
        | JsonBoolean
        | JsonInteger of JsonInteger.SpecificKeywords
        | JsonNumber
        | JsonString
        // TODO: None is missing from the specification
        | JsonOneOf of JsonSchemaType list 


    let rec private parseObject (schema: JsonSchema) : JsonSchemaType =
        let isRequired (name: string) =
            schema.RequiredProperties.Contains name

        schema.Properties
        |> Seq.map (fun pair ->

            let name : PropertyName = pair.Key
            let keywords : JsonObject.SpecificKeywords = {
                Required = isRequired name
            }
            let propertyType = parseJsonSchemaStructured pair.Value
            
            name, keywords, propertyType
        )
        |> Seq.toList
        |> JsonObject

    and private parseArray (schema: JsonSchema) : JsonSchemaType = 
        let keywords : JsonArray.SpecificKeywords = {
            // When minItems is omitted, it defaults to 0 according to the JSON Schema specification 
            MinItems = if schema.MinItems > 0 then Some schema.MinItems else None
         }
        JsonArray(parseJsonSchemaStructured schema.Item, keywords)

    and private parseObjectType (schema: JsonSchema) : JsonSchemaType =
        match schema.Type with
        | JsonObjectType.Array -> parseArray schema
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
        | JsonObjectType.Object -> parseObject schema
        | JsonObjectType.String -> JsonString
        | _ -> failwithf "Unsupported JSON object type %A." schema.Type

    and private parseOneOf (schema: JsonSchema) : JsonSchemaType =
        schema.OneOf |> List.ofSeq |> List.map parseJsonSchemaStructured |> JsonOneOf

    and parseJsonSchemaStructured (schema: JsonSchema) : JsonSchemaType =

        match schema.Type with
        | JsonObjectType.None -> 
            if schema.OneOf.Count > 0 then
                parseOneOf schema
            else
                failwith "Unsupported JSON schema type None"

        | _ -> parseObjectType schema

    let parseJsonSchema (input: string) : JsonSchemaType =
        let schema = SchemaCache.parseSchema input
        parseObjectType schema




    type ClassID = Guid

    // Fsharp match types to the JsonProperty and JsonSchemaType types.

    type FSharpType = 
        | FSharpClass of ClassID * List<PropertyName * JsonObject.SpecificKeywords * FSharpType> 
        | FSharpList of FSharpType * JsonArray.SpecificKeywords
        | FSharpDouble
        | FSharpInt of JsonInteger.SpecificKeywords
        | FSharpString
        | FSharpBool
        | FSharpOneOf of FSharpType list

    // Conversion from the JsonSchemaType into a the eqalevant FSharpType
    let rec jsonSchemaTypeToFSharpType
        (jsonSchemaType: JsonSchemaType)
        : FSharpType =
        match jsonSchemaType with
        | JsonBoolean -> FSharpBool
        | JsonInteger keywords -> FSharpInt keywords
        | JsonNumber -> FSharpDouble
        | JsonString -> FSharpString
        | JsonObject properties -> 
            // Convert the jsonshematype inside the properties into a fsharptype
            let properties' = List.map (fun (name, keywords, jsonSchemaType') -> name, keywords, jsonSchemaTypeToFSharpType jsonSchemaType') properties
            FSharpClass (Guid.NewGuid(), properties')

        | JsonArray(innerType, keywords) ->
            let innerFSharpType = jsonSchemaTypeToFSharpType  innerType
            FSharpList(innerFSharpType, keywords)
            
        | JsonOneOf types -> 
            FSharpOneOf <| List.map jsonSchemaTypeToFSharpType types
            