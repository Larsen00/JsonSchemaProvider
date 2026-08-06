namespace JsonSchemaProvider

open System.Collections.Concurrent
open NJsonSchema
open FSharp.Data
open System

// Wraps a raw JsonValue so generated properties can return "this or null"
// (nullable = how optional object/list properties are represented).
[<AllowNullLiteral>]
type NullableJsonValue(jsonVal: JsonValue) =
    member val JsonVal = jsonVal
    override this.ToString() : string = this.JsonVal.ToString()


// Avoids re-parsing the same schema text every time Parse/Create runs.
module SchemaCache =
    // hashCode of the schema text -> already-parsed NJsonSchema object.
    let private cache = ConcurrentDictionary<int, JsonSchema>()

    // Parses schema text into an NJsonSchema object (no caching).
    let parseSchema (schemaSource: string) =
        JsonSchema.FromJsonAsync(schemaSource)
        |> Async.AwaitTask
        |> Async.RunSynchronously

    // Parses and stores a schema in the cache up front.
    let cacheSchema (schemaSource: string) =
        let hashCode = schemaSource.GetHashCode()
        let schema = parseSchema schemaSource
        cache[hashCode] = schema

    //  the cached schema if present, else parses it once and caches it.
    let retrieveSchema (hashCode: int) (schemaSource: string) =
        cache.GetOrAdd(hashCode, (fun _ -> parseSchema schemaSource))

#if !IS_DESIGNTIME
[<assembly: FSharp.Core.CompilerServices.TypeProviderAssembly("JsonSchemaProvider.DesignTime")>]
do ()
#endif
