#I "../../src/RefinedJsonSchemaProvider.Runtime/bin/Debug/netstandard2.0"
#r "FSharp.Data.Json.Core.dll"
#r "NJsonSchema.dll"
#r "RefinedJsonSchemaProvider.dll"

open RefinedJsonSchemaProvider

[<Literal>]
let loginSchema =
    """
    {
      "type": "object",
      "properties": {
        "username": { "type": "string", "minLength": 1 },
        "password": { "type": "string", "minLength": 1 }
      },
      "required": ["username", "password"]
    }"""

[<Literal>]
let tokenSchema =
    """
    {
      "type": "object",
      "properties": {
        "username": { "type": "string" },
        "token": { "type": "string" }
      },
      "required": ["username", "token"]
    }"""

[<Literal>]
let failureSchema =
    """
    {
      "type": "object",
      "properties": {
        "messages": {
          "type": "array",
          "items": { "type": "string" },
          "minItems": 1
        }
      },
      "required": ["messages"]
    }"""

type Login = RefinedJsonSchemaProvider<schema = loginSchema>

type Token =
    RefinedJsonSchemaProvider<schema = tokenSchema, allowNullStrings = true>

type Failure =
    RefinedJsonSchemaProvider<schema = failureSchema, allowNullStrings = true>

let url = "http://localhost:8082/"
