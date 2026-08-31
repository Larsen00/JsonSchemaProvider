#I "../../src/JsonSchemaProvider.Runtime/bin/Debug/netstandard2.0"

#r "FSharp.Data.Json.Core.dll"
#r "NJsonSchema.dll"
#r "JsonSchemaProvider.dll"

#r "nuget: Expecto"

// If you only need quick provider debugging, avoid loading test source:
// #load "JsonSchemaProviderTests.fs"

open JsonSchemaProvider

[<Literal>]
let ageSchema =
    """
    {
      "type": "object",
      "properties": {
        "age": { "type": "integer", "minimum": 5, "maximum": 10 },
        "person": {
          "type": "object",
          "properties": {
            "name": { "type": "string", "maxLength": 5, "pattern": "^[A-Z].*" }
          },
          "required": ["name"]
        }
      },
      "required": ["age"]
    }"""

[<Literal>]
let personSchema2 =
    """
    {
      "type": "object",
      "properties": {
        "age": { "type": "integer", "minimum": 5, "maximum": 10 },
        "person": {
          "type": "object",
          "properties": {
            "name": { "type": "string" }
          },
          "required": ["name"]
        }
      },
      "required": ["age"]
    }"""

let personSchema3 =
    """
    {
      "type": "object",
      "properties": {
        "age": { "type": "integer", "minimum": 5, "maximum": 10 },
        "person": {
          "type": "object",
          "properties": {
            "name": { "type": "string" }
          },
          "required": ["name"]
        }
      },
      "required": ["age"]
    }"""

type Person = JsonSchemaProvider<schema=ageSchema>

let t = Person.Create(age = 6)
printfn "this is t: %A" t

try
    let x = Person.Parse(
        """
        {
        "age": 15,
        "person" : {
            "name": "tolongname"
            }
        }
        """
    )
    printfn "this is x: %A" x
with ex ->
    printfn "this is x (threw): %s" ex.Message

// Person2: person.name has NO constraint at all - so per the bottom-up skip rule discussed in
// notes/, `person` is fully compiled (nothing in its subtree can fail) and only `age` is a
// genuine failure source. Today, before any of that design is implemented, this still goes
// through the same schema.Validate mechanism as Person - but since `name` has nothing to
// violate, the error should mention ONLY age, not both. Contrast with `x` above, where BOTH
// age and person.name can fail and schema.Validate reports both together in one message.
type Person2 = JsonSchemaProvider<schema=personSchema2>

try
    let y = Person2.Parse(
        """
        {
        "age": 15,
        "person" : {
            "name": "this name has no constraint so any length is fine"
            }
        }
        """
    )
    printfn "this is y: %A" y
with ex ->
    printfn "this is y (threw): %s" ex.Message

// Can be compiled
type Adress = 
    {
        City: string
        Stree: string
    }

type Personx = {
    Age: int // cant be compiled
    Name: string // cant be compiled
    Adress: Adress
}

type RootPersonx =
    Result<Personx, string list>

type PersonxAdress = Adress
type PersonxAge = Result<int, string list>
type PersonxName = Result<string, string list>



// if someone called:
// let age = Peronsx.Age.create(age = )
// let name = Personx.Name.create(name =)
// let adress = Perosnx.Adress.create(adress =)
// and then a function call to the below the user could reconsruct the perosnx type
let createPerosnxFromNestedClasse adress (age: PersonxAge) (name: PersonxName) : Personx =
    match age, name with
    | Ok a, Ok n -> {
        Age = a
        Name = n
        Adress = adress
        }
    | _ -> failwith "error"

// but if the user then called the create on the root they would get result perosnx, string list then need to decontruct
// let p = Perosnx.create(_) => Ok (Personx)
