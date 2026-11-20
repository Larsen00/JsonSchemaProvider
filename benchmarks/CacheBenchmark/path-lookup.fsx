// Cost of NJsonSchema's GetJsonPath as the provider calls it, against a shared contract resolver and
// the batch GetJsonPaths (notes/cache-benchmark.md, section 5). Run: dotnet fsi path-lookup.fsx
// Needs a Release build of the benchmark first: dotnet build -c Release (in this folder).
#r "bin/Release/net10.0/Newtonsoft.Json.dll"
#r "bin/Release/net10.0/Namotion.Reflection.dll"
#r "bin/Release/net10.0/NJsonSchema.Annotations.dll"
#r "bin/Release/net10.0/NJsonSchema.dll"
#r "bin/Release/net10.0/FSharp.Data.Json.Core.dll"
#r "bin/Release/net10.0/FSharp.Data.Runtime.Utilities.dll"
#r "bin/Release/net10.0/FSharp.Data.Http.dll"
#r "bin/Release/net10.0/FSharp.TypeProviders.SDK.dll"
#r "bin/Release/net10.0/JsonSchemaProvider.DesignTime.dll"
#r "bin/Release/net10.0/CacheBenchmark.dll"
open System.Diagnostics
open JsonSchemaProvider
open JsonSchemaProvider.DesignTime
open JsonSchemaProvider.Benchmarks
open JsonSchemaProvider.Tests.CacheSupport
let time name f =
    let sw = Stopwatch.StartNew()
    let r = f ()
    printfn "%-28s %8.1f ms" name sw.Elapsed.TotalMilliseconds
    r
let resolver = Newtonsoft.Json.Serialization.DefaultContractResolver()
for objects in [ 10; 40; 150 ] do
  let text = Schemas.objectTree objects
  let schema = SchemaCache.parseSchema text
  let rec subs (s: NJsonSchema.JsonSchema) : NJsonSchema.JsonSchema list =
      s :: ([ for p in s.Properties.Values -> (p :> NJsonSchema.JsonSchema) ] @ (if isNull s.Item then [] else [ s.Item ]) |> List.collect subs)
  let all = subs schema
  printfn "== %d nodes" all.Length
  let sample = all |> List.take 10
  let viaDefault = time "GetJsonPath x10, as now" (fun () -> sample |> List.map (fun s -> NJsonSchema.JsonPathUtilities.GetJsonPath(schema, s)))
  let viaShared = time "GetJsonPath x10, shared resolver" (fun () -> sample |> List.map (fun s -> NJsonSchema.JsonPathUtilities.GetJsonPath(schema, s, resolver)))
  let allShared = time (sprintf "GetJsonPath x%d, shared resolver" all.Length) (fun () -> all |> List.map (fun s -> NJsonSchema.JsonPathUtilities.GetJsonPath(schema, s, resolver)))
  let batch = time (sprintf "GetJsonPaths batch x%d" all.Length) (fun () -> NJsonSchema.JsonPathUtilities.GetJsonPaths(schema, all |> Seq.cast<obj>, resolver))
  printfn "same paths: %b %b" (viaDefault = viaShared) (all |> List.forall2 (fun p s -> batch[s :> obj] = p) allShared)
