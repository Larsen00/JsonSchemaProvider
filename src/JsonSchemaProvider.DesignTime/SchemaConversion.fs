namespace JsonSchemaProvider.DesignTime

module SchemaConversion =
    open NJsonSchema
    open JsonSchemaProvider
    open System

    type JsonProperty = { 
        Name: string
        Optional: bool
        PropertyType: JsonSchemaType 
    }

    // type Name = string
    and JsonSchemaType =
        | JsonObject of JsonProperty list
        // | JsonObject of List<Name * JsonObject.SpecificKeywords * JsonSchemaType> // TODO
        | JsonArray of JsonSchemaType * JsonArray.SpecificKeywords
        | JsonBoolean
        | JsonInteger
        | JsonNumber
        | JsonString
        // TODO: None is missing from the specification
        | JsonOneOf of JsonSchemaType list 

    let rec private parseObject (schema: JsonSchema) : JsonSchemaType =
        let isOptional (name: string) =
            not (schema.RequiredProperties.Contains(name))

        schema.Properties
        |> Seq.map (fun pair ->
            { Name = pair.Key
              Optional = isOptional (pair.Key)
              PropertyType = parseJsonSchemaStructured pair.Value })
        |> Seq.toList
        |> JsonObject

    and private parseArray (schema: JsonSchema) : JsonSchemaType = 
        let keywords : JsonArray.SpecificKeywords = {
            // When minItems is omitted, it defaults to 0 according to the JSON Schema specification 
            MinItems = if schema.MinItems > 0 then Some schema.MinItems else None
         }
        JsonArray(parseJsonSchemaStructured schema.Item, keywords)

    and private parseType (schema: JsonSchema) : JsonSchemaType =
        match schema.Type with
        | JsonObjectType.Array -> parseArray schema
        | JsonObjectType.Boolean -> JsonBoolean
        | JsonObjectType.Integer -> JsonInteger
        | JsonObjectType.Number -> JsonNumber
        | JsonObjectType.Object -> parseObject schema
        | JsonObjectType.String -> JsonString
        | _ -> failwithf "Unsupported JSON object type %A." schema.Type

    and private parseOneOf (schema: JsonSchema) : JsonSchemaType =
        schema.OneOf |> List.ofSeq |> List.map parseJsonSchemaStructured |> JsonOneOf

    and parseJsonSchemaStructured (schema: JsonSchema) : JsonSchemaType =
        // TODO: Support other types than object at the root level

        match schema.Type with
        | JsonObjectType.None -> 
            if schema.OneOf.Count > 0 then
                parseOneOf schema
            else
                failwith "Unsupported JSON schema type None"

        | _ -> 
            parseType schema

    let parseJsonSchema (input: string) : JsonSchemaType =
        let schema = SchemaCache.parseSchema input
        parseJsonSchemaStructured schema

    // Fsharp match types to the JsonProperty and JsonSchemaType types. // DEV_NOTE: I removed the orginal FSharpClassTree type since it had and extra layer of complexity that was not needed. This way the types now look more like the original JSON schema types and are easier to reason about.
    type FSharpProperty =
        { Name: string
          Optional: bool
          FSharpType: FSharpType }
    
    and FSharpType =
        | FSharpClass of string * FSharpProperty list // Json object and the name // TODO: is this misleading? Since F# doesn't have classes 
        | FSharpList of FSharpType * JsonArray.SpecificKeywords
        | FSharpDouble
        | FSharpInt
        | FSharpString
        | FSharpBool
        | FSharpOneOf of FSharpType list


// '    and FSharpClassTree =
//         { Name: string
//           Properties: FSharpProperty list }
//           NestedClasses: FSharpClassTree list }'

    let rec private jsonPropertyToFSharpProperty
        ({ Name = propertyName
           Optional = optional
           PropertyType = propertyType }: JsonProperty)
        : FSharpProperty  =
        let fSharpType = jsonSchemaTypeToFSharpType propertyName propertyType

        { 
            Name = propertyName
            Optional = optional
            FSharpType = fSharpType 
        }

    and private jsonSchemaTypeToFSharpType
        (lhsName: string)
        (jsonSchemaType: JsonSchemaType)
        : FSharpType =
        match jsonSchemaType with
        | JsonBoolean -> FSharpBool
        | JsonObject properties -> FSharpClass (lhsName, List.map jsonPropertyToFSharpProperty properties)
        | JsonArray(innerType, keywords) ->
            let innerFSharpType = jsonSchemaTypeToFSharpType lhsName innerType
            FSharpList(innerFSharpType, keywords)
            
        | JsonInteger -> FSharpInt
        | JsonNumber -> FSharpDouble
        | JsonString -> FSharpString
        | JsonOneOf types -> FSharpOneOf <| List.map (jsonSchemaTypeToFSharpType lhsName) types
            

    let jsonObjectToFSharpClass (lhsName: string) (jsonSchemaType: JsonSchemaType) : FSharpType =
        match jsonSchemaType with
        | JsonObject _ -> jsonSchemaTypeToFSharpType lhsName jsonSchemaType
        | _ -> failwith "Only object type supported"
