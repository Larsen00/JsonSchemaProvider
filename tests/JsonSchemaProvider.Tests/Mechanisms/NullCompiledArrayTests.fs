namespace JsonSchemaProvider.Tests

// F# tuples and lists are .NET reference types, so Unchecked.defaultof gives a null the compiler
// accepts. These tests record what the provider does with one; they document behaviour, not intent.
// A root array has no public ToJson, so serialisation is observed through an object property.
module NullCompiledArrayTests =
    open System
    open Xunit
    open JsonSchemaProvider

    let expectNullReference (f: unit -> unit) =
        Assert.Throws<NullReferenceException>(Action f) |> ignore

    // -- minItems = maxItems = 3: int * (int * int), fully compilable --

    [<Literal>]
    let intTuple3Schema = """{ "type": "array", "items": { "type": "integer" }, "minItems": 3, "maxItems": 3 }"""
    type IntTuple3 = JsonSchemaProvider<schema = intTuple3Schema>

    [<Fact>]
    let ``fully compilable tuple Create is plain`` () =
        let v: int * (int * int) = IntTuple3.Create((1, (2, 3)))
        expectEqual v (1, (2, 3))

    // Create round-trips through ToJson, so the null is hit inside Create itself.
    [<Fact>]
    let ``tuple Create throws NullReferenceException for a null outer pair`` () =
        expectNullReference (fun () -> IntTuple3.Create(Unchecked.defaultof<int * (int * int)>) |> ignore)

    [<Fact>]
    let ``tuple Create throws NullReferenceException for a null inner pair`` () =
        expectNullReference (fun () -> IntTuple3.Create((1, Unchecked.defaultof<int * int>)) |> ignore)

    [<Fact>]
    let ``tuple ToList throws NullReferenceException for a null outer pair`` () =
        expectNullReference (fun () -> IntTuple3.ToList(Unchecked.defaultof<int * (int * int)>) |> ignore)

    [<Fact>]
    let ``tuple ToList throws NullReferenceException for a null inner pair`` () =
        expectNullReference (fun () -> IntTuple3.ToList((1, Unchecked.defaultof<int * int>)) |> ignore)

    [<Literal>]
    let compiledArraysObjectSchema =
        """{ "type": "object", "properties": { "t": { "type": "array", "items": { "type": "integer" }, "minItems": 3, "maxItems": 3 }, "l": { "type": "array", "items": { "type": "integer" } } }, "required": ["t", "l"] }"""
    type CompiledArraysObject = JsonSchemaProvider<schema = compiledArraysObjectSchema>

    [<Fact>]
    let ``object of fully compilable arrays has a plain Create and serialises normally`` () =
        let v: CompiledArraysObject = CompiledArraysObject.Create(t = (1, (2, 3)), l = [ 4 ])
        expectEqual (v.JsonVal.ToString(FSharp.Data.JsonSaveOptions.DisableFormatting)) """{"t":[1,2,3],"l":[4]}"""

    [<Fact>]
    let ``object Create throws NullReferenceException for a null outer pair property`` () =
        expectNullReference (fun () ->
            CompiledArraysObject.Create(t = Unchecked.defaultof<int * (int * int)>, l = []) |> ignore)

    [<Fact>]
    let ``object Create throws NullReferenceException for a null inner pair property`` () =
        expectNullReference (fun () ->
            CompiledArraysObject.Create(t = (1, Unchecked.defaultof<int * int>), l = []) |> ignore)

    // -- control: minItems = maxItems = 1 is a plain int, nothing can be null --

    [<Literal>]
    let intSingleSchema = """{ "type": "array", "items": { "type": "integer" }, "minItems": 1, "maxItems": 1 }"""
    type IntSingle = JsonSchemaProvider<schema = intSingleSchema>

    [<Fact>]
    let ``single-item Create of Unchecked.defaultof is just 0`` () =
        let v: int = IntSingle.Create(Unchecked.defaultof<int>)
        expectEqual v 0

    // -- plain int list, fully compilable --

    [<Literal>]
    let intListSchema = """{ "type": "array", "items": { "type": "integer" } }"""
    type IntList = JsonSchemaProvider<schema = intListSchema>

    [<Fact>]
    let ``list Create throws NullReferenceException for a null list`` () =
        expectNullReference (fun () -> IntList.Create(Unchecked.defaultof<int list>) |> ignore)

    [<Fact>]
    let ``object Create throws NullReferenceException for a null list property`` () =
        expectNullReference (fun () ->
            CompiledArraysObject.Create(t = (1, (2, 3)), l = Unchecked.defaultof<int list>) |> ignore)

    // -- comparison: string items, so Create validates --

    [<Literal>]
    let stringTuple2Schema = """{ "type": "array", "items": { "type": "string" }, "minItems": 2, "maxItems": 2 }"""
    type StringTuple2 = JsonSchemaProvider<schema = stringTuple2Schema>

    [<Fact>]
    let ``validated tuple Create gives Error for a null string element`` () =
        expectError (StringTuple2.Create(("a", null)))

    // Validation runs on the JSON, and ToJson hits the null tuple before there is any JSON.
    [<Fact>]
    let ``validated tuple Create still throws NullReferenceException for a null pair`` () =
        expectNullReference (fun () -> StringTuple2.Create(Unchecked.defaultof<string * string>) |> ignore)

    [<Literal>]
    let stringListSchema = """{ "type": "array", "items": { "type": "string" } }"""
    type StringList = JsonSchemaProvider<schema = stringListSchema>

    [<Fact>]
    let ``validated list Create still throws NullReferenceException for a null list`` () =
        expectNullReference (fun () -> StringList.Create(Unchecked.defaultof<string list>) |> ignore)
