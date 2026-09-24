namespace JsonSchemaProvider.Tests

// Covers the schemaFile static parameter (as opposed to schema, an inline string literal):
// the provider reading a JSON Schema from a file on disk instead of taking the schema text
// directly. Every schema here lives in the schemas/ subfolder so this one file is the place
// to look when checking whether file-based schema loading still works.
module SchemaFileTests =
    open Expecto
    open JsonSchemaProvider

    [<Literal>]
    let flatSchemaPath = __SOURCE_DIRECTORY__ + "/../schemas/FlatSchema.json"

    [<Literal>]
    let requiredPropertiesSchemaPath =
        __SOURCE_DIRECTORY__ + "/../schemas/RequiredPropertiesSchema.json"

    [<Literal>]
    let stringListRootSchemaPath =
        __SOURCE_DIRECTORY__ + "/../schemas/StringListRootSchema.json"

    type FlatFromFile = JsonSchemaProvider<schemaFile=flatSchemaPath>
    type RequiredPropertiesFromFile = JsonSchemaProvider<schemaFile=requiredPropertiesSchemaPath>
    type StringListRootFromFile = JsonSchemaProvider<schemaFile=stringListRootSchemaPath>

    let createMethodFromFileSchemaShouldReturnRecord =
        test "create method from an object schema loaded from file should return record" {
            let flat = FlatFromFile.Create()
            Expect.equal flat.X None "flat.X = None"
            Expect.equal flat.Y None "flat.Y = None"
            Expect.equal flat.Z None "flat.Z = None"
        }

    let requiredPropertiesFromFileShouldBeParsed =
        test "required properties loaded from file should not be parsed into Option" {
            let v =
                Expect.wantOk (RequiredPropertiesFromFile.Parse("""{"X": "x", "Y": "y", "Z": 1}""")) "Parse should succeed"

            Expect.equal v.X "x" """v.X = "x" """
            Expect.equal v.Y "y" """v.Y = "y" """
            Expect.equal v.Z 1 "v.Z = 1"
        }

    let stringListRootFromFileShouldBeCreated =
        test "array-root schema loaded from file Create builds the list" {
            let result = StringListRootFromFile.Create([ "a"; "b"; "c" ])
            Expect.equal result [ "a"; "b"; "c" ] "result = [a; b; c]"
        }

    [<Tests>]
    let tests =
        testList
            "JsonSchemaProvider.Tests.SchemaFileTests"
            [ createMethodFromFileSchemaShouldReturnRecord
              requiredPropertiesFromFileShouldBeParsed
              stringListRootFromFileShouldBeCreated ]
