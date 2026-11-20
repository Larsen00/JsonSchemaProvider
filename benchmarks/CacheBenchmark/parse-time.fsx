// Time to parse each benchmark schema alone (NJsonSchema + parseJsonSchemaStructured), written to
// results/parse-time.csv. Run: dotnet fsi parse-time.fsx
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
let families = [ "objectTree", [ 1; 5; 10; 20; 30; 40; 60; 80; 100; 120; 150 ], Schemas.objectTree
                 "refinedArrays", [ 1; 2; 3; 4; 6; 8; 12; 16; 20; 24; 32; 40; 48; 64 ], Schemas.refinedArrays
                 "deepChain", [ 1; 2; 4; 6; 8; 10; 12; 14; 16; 18; 20 ], Schemas.deepChain ]
let lines = System.Collections.Generic.List<string>([ "family,size,nodes,runs,parseMs" ])
let warm = SchemaCache.parseSchema (Schemas.objectTree 3) in SchemaConversion.parseJsonSchemaStructured warm warm |> ignore
for name, sizes, schemaOf in families do
  for size in sizes do
    let text = schemaOf size
    let one () =
        let sw = Stopwatch.StartNew()
        let schema = SchemaCache.parseSchema text
        SchemaConversion.parseJsonSchemaStructured schema schema |> ignore
        sw.Elapsed.TotalMilliseconds
    let first = one ()
    let runs = if first > 5000.0 then [ first ] else [ first; one (); one () ]
    let med = (List.sort runs).[runs.Length / 2]
    let line = sprintf "%s,%d,%d,%d,%.2f" name size (Schemas.nodeCount text) runs.Length med
    lines.Add line
    printfn "%s" line
    System.IO.File.WriteAllLines((System.IO.Path.Combine(__SOURCE_DIRECTORY__, "results", "parse-time.csv")), lines)
