namespace RefinedJsonSchemaProvider

open System.Collections.Concurrent
open NJsonSchema
open FSharp.Data
open System
open Newtonsoft.Json.Serialization

// The runtime type of every object node, a JsonValue that may be null.
[<AllowNullLiteral>]
type NullableJsonValue(jsonVal: JsonValue) =
    member val JsonVal = jsonVal
    override this.ToString() : string = this.JsonVal.ToString()


// Avoids re-parsing the same schema text every time Parse/Create runs.
module SchemaCache =
    // The hash code of the schema text mapped to the parsed schema.
    let private cache = ConcurrentDictionary<int, JsonSchema>()

    // Parses schema text into an NJsonSchema object (no caching).
    let parseSchema (schemaSource: string) =
        JsonSchema.FromJsonAsync(schemaSource)
        |> Async.AwaitTask
        |> Async.RunSynchronously

    // The cached schema, parsed and cached on first use.
    let retrieveSchema (hashCode: int) (schemaSource: string) =
        cache.GetOrAdd(hashCode, (fun _ -> parseSchema schemaSource))

    // The subschema at a JSON Pointer path, e.g. "#/properties/name".
    let resolveByPath (root: JsonSchema) (path: string) : JsonSchema =
        let appender = JsonSchemaAppender(root, DefaultTypeNameGenerator())
        let resolver = JsonReferenceResolver appender
        resolver.ResolveDocumentReference(root, path, typeof<JsonSchema>, DefaultContractResolver()) :?> JsonSchema

#if !IS_DESIGNTIME
[<assembly: FSharp.Core.CompilerServices.TypeProviderAssembly("RefinedJsonSchemaProvider.DesignTime")>]
do ()
#endif
