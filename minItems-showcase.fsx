#I "src/JsonSchemaProvider.Runtime/bin/Debug/netstandard2.0"
#r "FSharp.Data.Json.Core.dll"
#r "NJsonSchema.dll"
#r "JsonSchemaProvider.dll"
open JsonSchemaProvider

// Plain F#: [1] has type int list, which includes [], so the match warns (FS0025)
let firstPlain =
    match [1] with
    | x :: _ -> x

// Provider: minItems 1 gives int * int list, so the match covers every term and doesn't warn
[<Literal>]
let schema = """{ "type": "array", "items": { "type": "integer" }, "minItems": 1 }"""
type NonEmpty = JsonSchemaProvider<schema = schema>

let firstProvided  =
    match NonEmpty.Create((1, [])) with
    | x, _ -> x
