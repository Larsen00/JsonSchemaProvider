// Copyright (c) 2024 Florian Lorenzen

// Permission is hereby granted, free of charge, to any person
// obtaining a copy of this software and associated documentation
// files (the “Software”), to deal in the Software without
// restriction, including without limitation the rights to use,
// copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the
// Software is furnished to do so, subject to the following
// conditions:

// The above copyright notice and this permission notice shall be
// included in all copies or substantial portions of the Software.

// THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
// OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
// HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
// WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
// OTHER DEALINGS IN THE SOFTWARE.

namespace JsonSchemaProvider.DesignTime

module SchemaConversion =
    open NJsonSchema
    open JsonSchemaProvider

    // JSON Schema offers a variety of keywords to validate data against specific types.
    // In the following types, we wont to store these keywords
    type ArrayKeywords = {
        MinItems: int option
    }

    type JsonProperty = { 
        Name: string
        Optional: bool
        PropertyType: JsonSchemaType 
    }

    and JsonSchemaType =
        | JsonObject of JsonProperty list
        | JsonArray of JsonSchemaType * ArrayKeywords
        | JsonBoolean
        | JsonInteger
        | JsonNumber
        | JsonString
        // TODO: None is missing from the specification

    let rec private parseObject (schema: JsonSchema) : JsonSchemaType =
        let isOptional (name: string) =
            not (schema.RequiredProperties.Contains(name))

        schema.Properties
        |> Seq.map (fun pair ->
            { Name = pair.Key
              Optional = isOptional (pair.Key)
              PropertyType = parseType pair.Value })
        |> Seq.toList
        |> JsonObject

    and private parseArray (schema: JsonSchema) : JsonSchemaType = 
        let keywords = {
            // When minItems is omitted, it defaults to 0 according to the JSON Schema specification 
            MinItems = if schema.MinItems > 0 then Some schema.MinItems else None
         }
        JsonArray(parseType schema.Item, keywords)

    and private parseType (schema: JsonSchema) : JsonSchemaType =
        match schema.Type with
        | JsonObjectType.Array -> parseArray schema
        | JsonObjectType.Boolean -> JsonBoolean
        | JsonObjectType.Integer -> JsonInteger
        | JsonObjectType.Number -> JsonNumber
        | JsonObjectType.Object -> parseObject schema
        | JsonObjectType.String -> JsonString
        | _ -> failwithf "Unsupported JSON object type %A." schema.Type

    let parseJsonSchemaStructured (schema: JsonSchema) : JsonSchemaType =
        // TODO: Support other types than object at the root level
        if schema.Type <> JsonObjectType.Object then
            failwith "Only object supported."

        parseType schema

    let parseJsonSchema (input: string) : JsonSchemaType =
        let schema = SchemaCache.parseSchema input
        parseJsonSchemaStructured schema

    type FSharpType =
        | FSharpClass of string // Json object and the name // TODO: is this misleading? Since F# doesn't have classes 
        | FSharpList of FSharpType * ArrayKeywords
        | FSharpDouble
        | FSharpInt
        | FSharpString
        | FSharpBool

    type FSharpProperty =
        { Name: string
          Optional: bool
          FSharpType: FSharpType }

    type FSharpClassTree =
        { Name: string
          Properties: FSharpProperty list
          NestedClasses: FSharpClassTree list }

    let rec private jsonPropertyToFSharpPropertyAndNestedClass
        ({ Name = propertyName
           Optional = optional
           PropertyType = propertyType }: JsonProperty)
        : FSharpProperty * FSharpClassTree option =
        let (fSharpType, maybeClass) =
            jsonSchemaTypeToFSharpTypeAndNestedClass propertyName propertyType

        ({ Name = propertyName
           Optional = optional
           FSharpType = fSharpType },
         maybeClass)

    and private jsonSchemaTypeToFSharpTypeAndNestedClass
        (lhsName: string)
        (jsonSchemaType: JsonSchemaType)
        : FSharpType * FSharpClassTree option =
        match jsonSchemaType with
        | JsonBoolean -> (FSharpBool, None)
        | JsonObject(properties) -> (FSharpClass(lhsName), Some(jsonPropertyListToFSharpClassTree lhsName properties))
        | JsonArray(innerType, keywords) ->
            let (innerFSharpType, maybeClass) =
                jsonSchemaTypeToFSharpTypeAndNestedClass lhsName innerType

            FSharpList(innerFSharpType, keywords), maybeClass
        | JsonInteger -> (FSharpInt, None)
        | JsonNumber -> (FSharpDouble, None)
        | JsonString -> (FSharpString, None)

    and private jsonPropertyListToFSharpClassTree
        (lhsName: string)
        (jsonProperties: JsonProperty list)
        : FSharpClassTree =
        let (fSharpProperties, maybeNestedClasses) =
            jsonProperties
            |> List.map jsonPropertyToFSharpPropertyAndNestedClass
            |> List.unzip

        let nestedClasses = maybeNestedClasses |> List.map Option.toList |> List.concat

        { Name = lhsName
          Properties = fSharpProperties
          NestedClasses = nestedClasses }

    let jsonObjectToFSharpClassTree (lhsName: string) (jsonSchemaType: JsonSchemaType) : FSharpClassTree =
        match jsonSchemaType with
        | JsonObject(properties) -> jsonPropertyListToFSharpClassTree lhsName properties
        | _ -> failwith "Only object type supported"
