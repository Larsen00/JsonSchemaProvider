namespace JsonSchemaProvider

open System.Collections.Concurrent
open NJsonSchema
open FSharp.Data

[<AllowNullLiteral>]
type NullableJsonValue(jsonVal: JsonValue) =
    member val JsonVal = jsonVal
    override this.ToString() : string = this.JsonVal.ToString()


module SchemaCache =
    let private cache = ConcurrentDictionary<int, JsonSchema>()

    let parseSchema (schemaSource: string) =
        JsonSchema.FromJsonAsync(schemaSource)
        |> Async.AwaitTask
        |> Async.RunSynchronously

    let cacheSchema (schemaSource: string) =
        let hashCode = schemaSource.GetHashCode()
        let schema = parseSchema schemaSource
        cache[hashCode] = schema

    let retrieveSchema (hashCode: int) (schemaSource: string) =
        cache.GetOrAdd(hashCode, (fun _ -> parseSchema schemaSource))

#if !IS_DESIGNTIME
[<assembly: FSharp.Core.CompilerServices.TypeProviderAssembly("JsonSchemaProvider.DesignTime")>]
do ()
#endif
