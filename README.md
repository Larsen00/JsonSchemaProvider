# JsonSchemaProvider: F# type provider for JSON schema

This is a fork of [florenzen/JsonSchemaProvider](https://github.com/florenzen/JsonSchemaProvider) used for a DTU
master's thesis (see [CLAUDE.md](CLAUDE.md) for background). It is not published to NuGet and does not track the
upstream project.

The JsonSchemaProvider provides F# types from [JSON schemas](https://json-schema.org). It can be used to build
JSON values in a strongly typed way that conform to the schema or to parse JSON values into an F# value that
can be queried in a strongly typed way. Specifications like numeric ranges or string patterns that cannot be
validated at compile time are checked at runtime.

The JSON schema can either be given as an inline string literal or by a local file.

The type provider is built around [NJsonSchema](https://njsonschema.org/) for the schema parsing and validation
and uses the `JsonValue` data type from [FSharp.Data](https://fsprojects.github.io/FSharp.Data/).

## Building

The type provider requires the .NET SDK 10 or higher (see [global.json](global.json)).

```bash
dotnet build JsonSchemaProvider.sln
```

Tests are split into two projects:

- `tests/JsonSchemaProvider.DesignTime.Tests` — unit tests for the schema-conversion/type-level logic. These
  call the DesignTime code directly and never invoke the type provider itself.
- `tests/JsonSchemaProvider.Tests` — tests that actually instantiate `JsonSchemaProvider<...>`, exercising the
  type provider end to end.

```bash
dotnet test tests/JsonSchemaProvider.DesignTime.Tests
dotnet test tests/JsonSchemaProvider.Tests
```

If a build fails with a file-in-use error on `JsonSchemaProvider.DesignTime.dll`, it's because Ionide's
background compiler service (FSAC) still has it loaded from editing a file that uses the type provider. Run
"F#: Restart Language Server" from VS Code's command palette to release the lock.

## Debugging

Debugging type providers requires to run the FSharp compiler or interpreter on a source
file using the type provider since the provider's code is executed in the compilation
pipeline. See the comments in [debugUtils/debug.fsx](debugUtils/debug.fsx) how to launch the
code in the Ionide debugger.

## License

The JSON schema type provider is available under the MIT license. For more information see [license file](LICENSE).
