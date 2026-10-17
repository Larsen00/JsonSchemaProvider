namespace JsonSchemaProvider.Tests

// The schemaFile static parameter: reading a JSON Schema from a file on disk instead of an
// inline schema string literal. Schema files live in ../schemas/.
module SchemaFileTests =
    open Xunit
    open JsonSchemaProvider

    [<Literal>]
    let flatSchemaPath = __SOURCE_DIRECTORY__ + "/../schemas/FlatSchema.json"
    type FlatFromFile = JsonSchemaProvider<schemaFile=flatSchemaPath>

    [<Fact>]
    let ``create method from an object schema loaded from file should return record`` () =
        let flat = FlatFromFile.Create()
        expectEqual flat.X None
        expectEqual flat.Y None
        expectEqual flat.Z None

    [<Literal>]
    let requiredPropertiesSchemaPath = __SOURCE_DIRECTORY__ + "/../schemas/RequiredPropertiesSchema.json"
    type RequiredPropertiesFromFile = JsonSchemaProvider<schemaFile=requiredPropertiesSchemaPath>

    [<Fact>]
    let ``required properties loaded from file should not be parsed into Option`` () =
        let v = expectOk (RequiredPropertiesFromFile.Parse("""{"X": "x", "Y": "y", "Z": 1}"""))
        expectEqual v.X "x"
        expectEqual v.Y "y"
        expectEqual v.Z 1

    [<Literal>]
    let stringListRootSchemaPath = __SOURCE_DIRECTORY__ + "/../schemas/StringListRootSchema.json"
    type StringListRootFromFile = JsonSchemaProvider<schemaFile=stringListRootSchemaPath>

    [<Fact>]
    let ``array-root schema loaded from file Create builds the list`` () =
        let result = StringListRootFromFile.Create([ "a"; "b"; "c" ])
        expectEqual result [ "a"; "b"; "c" ]
