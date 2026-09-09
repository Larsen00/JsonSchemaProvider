namespace JsonSchemaProvider.DesignTime

module SchemaConversion =
    open NJsonSchema
    open JsonSchemaProvider
    open System


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
        | JsonOneOf of JsonSchemaType * JsonSchemaType list // Oneof has atleat one element


    let rec private parseObject (rootSchema: JsonSchema) (schema: JsonSchema) (common: Common.Keywords) : JsonSchemaType =
        let isRequired (name: string) =
            schema.RequiredProperties.Contains name

        Seq.foldBack (fun (KeyValue(name, propertySchema)) (specific : JsonObject.Specific, properties') ->

            let propertyType : JsonSchemaType = parseJsonSchemaStructured rootSchema propertySchema

            { specific with Required = Map.add name (isRequired name) specific.Required },
            (name, propertyType) :: properties'


        ) schema.Properties ({ Required = Map.empty }, [])
        |> fun (specific, properties) -> JsonObject({ common = common; specific = specific }, properties)

    and private parseArray (rootSchema: JsonSchema) (schema: JsonSchema) (common: Common.Keywords) : JsonSchemaType =
        let specific : JsonArray.Specific = {
            // When minItems is omitted, it defaults to 0 according to the JSON Schema specification
            MinItems = if schema.MinItems > 0 then Some schema.MinItems else None
         }
        JsonArray(parseJsonSchemaStructured rootSchema schema.Item, { common = common; specific = specific })

    and private parseObjectType (rootSchema: JsonSchema) (schema: JsonSchema) : JsonSchemaType =
        // Path identifies this node's own position in rootSchema. We use it as a unique id in the
        // classMap and also to find this node's own subschema to validate against it.
        let common : Common.Keywords = { Path = JsonPathUtilities.GetJsonPath(rootSchema, schema) }

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
        schema.OneOf |> List.ofSeq |> List.map (parseJsonSchemaStructured rootSchema) |> fun l ->  JsonOneOf (List.head l, List.tail l)

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
        | FSharpOneOf of FSharpType * FSharpType list

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

        | JsonOneOf (head, tail) ->
            List.map jsonSchemaTypeToFSharpType (head :: tail)
            |> fun l -> FSharpOneOf (List.head l, List.tail l)

