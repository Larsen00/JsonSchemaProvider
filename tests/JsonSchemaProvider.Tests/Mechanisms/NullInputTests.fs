namespace JsonSchemaProvider.Tests

// A .NET string or provided object can be null and F# can't rule that out for provided types, so
// wherever one is passed in, Create gives a Result and a null becomes Error via validation.
// Nothing throws. Optional properties are the exception: there null means "leave it out".
module NullInputTests =
    open Xunit
    open FSharp.Data
    open JsonSchemaProvider

    [<Literal>]
    let requiredStringSchema =
        """{ "type": "object", "properties": { "name": {"type": "string"} }, "required": ["name"] }"""
    type RequiredString = JsonSchemaProvider<schema=requiredStringSchema>

    [<Fact>]
    let ``object Create gives Error for null in a required string`` () =
        expectError (RequiredString.Create(name = null))

    [<Fact>]
    let ``object Create gives Ok for a non-null required string`` () =
        let v = expectOk (RequiredString.Create(name = "x"))
        expectEqual v.name "x"

    [<Literal>]
    let optionalStringSchema =
        """{ "type": "object", "properties": { "name": {"type": "string"} } }"""
    type OptionalString = JsonSchemaProvider<schema=optionalStringSchema>

    [<Fact>]
    let ``optional string Create with null still leaves the property out`` () =
        let v = expectOk (OptionalString.Create(name = null))
        expectEqual v.name None

    [<Literal>]
    let stringListSchema =
        """{ "type": "object", "properties": { "tags": {"type": "array", "items": {"type": "string"}} }, "required": ["tags"] }"""
    type StringList = JsonSchemaProvider<schema=stringListSchema>

    [<Fact>]
    let ``object Create gives Error for null as a list element`` () =
        expectError (StringList.Create(tags = [ "a"; null ]))

    [<Literal>]
    let stringRootSchema = """{ "type": "string" }"""
    type StringRoot = JsonSchemaProvider<schema=stringRootSchema>

    [<Fact>]
    let ``string root Create gives Error for null`` () =
        expectError (StringRoot.Create null)

    [<Literal>]
    let stringListRootSchema = """{ "type": "array", "items": {"type": "string"} }"""
    type StringListRoot = JsonSchemaProvider<schema=stringListRootSchema>

    [<Fact>]
    let ``list root Create gives Error for a null element`` () =
        expectError (StringListRoot.Create [ "a"; null ])

    // SkipRuntimeValidation skips the check that would catch the null: nothing throws, the null
    // is stored as JSON null and the getter reads it back as "" (FSharp.Data's AsString).
    type UnsafeRequiredString = JsonSchemaProvider<schema=requiredStringSchema, skipRuntimeValidation=true>

    [<Fact>]
    let ``with SkipRuntimeValidation Create accepts null in a required string without throwing`` () =
        let v = UnsafeRequiredString.Create(name = null)
        expectEqual v.JsonVal (JsonValue.Record [| "name", JsonValue.Null |])
        expectEqual v.name ""

    [<Literal>]
    let nestedObjectSchema =
        """{ "type": "object", "properties": { "inner": {"type": "object", "properties": { "x": {"type": "integer"} }} }, "required": ["inner"] }"""
    type NestedObject = JsonSchemaProvider<schema=nestedObjectSchema>

    [<Fact>]
    let ``object Create gives Error for null in a required nested object`` () =
        expectError (NestedObject.Create(inner = null))

    [<Fact>]
    let ``object Create gives Ok for a non-null nested object`` () =
        let inner = NestedObject.innerObj.Create(x = 1)
        let v = expectOk (NestedObject.Create(inner = inner))
        expectEqual v.inner.x (Some 1)

    // The nested object's own Create only takes an integer, so it can't fail and stays plain.
    [<Fact>]
    let ``an object of only integers keeps a plain Create`` () =
        let inner: NestedObject.innerObj = NestedObject.innerObj.Create(x = 1)
        expectEqual inner.x (Some 1)

    [<Literal>]
    let objectListSchema =
        """{ "type": "array", "items": {"type": "object", "properties": { "x": {"type": "integer"} }} }"""
    type ObjectList = JsonSchemaProvider<schema=objectListSchema>

    [<Fact>]
    let ``list root Create gives Error for a null object element`` () =
        let item = ObjectList.Item.Create(x = 1)
        expectError (ObjectList.Create [ item; null ])
