# JsonSchemaProvider: refined F# types from JSON Schema

An F# type provider that turns a JSON Schema into F# types, and its constraints into refined types
wherever F# can express them.

```fsharp
[<Literal>]
let schema = """{ "type": "array", "items": { "type": "integer", "minimum": 0, "maximum": 255 }, "minItems": 1 }"""
type NonEmpty = JsonSchemaProvider<schema = schema>

let numbers = NonEmpty.Create((1uy, [ 2uy; 3uy ]))   // byte * byte list: an empty array can't be built
```

This is a fork of [Florian Lorenzen's JsonSchemaProvider](https://github.com/florenzen/JsonSchemaProvider),
extended as my master's thesis at DTU. It is not the `JsonSchemaProvider` package on NuGet; that
one is the original. See [Credits](#credits).

> **Status:** research code from a master's thesis. It works and is well tested, but the API can
> change and parts of JSON Schema aren't supported yet ([Limitations](#limitations)). If you use it,
> please [open an issue](https://github.com/Larsen00/JsonSchemaProvider/issues). I'm happy to keep
> extending it if people find it useful.

## Table of Contents

- [How it works](#how-it-works)
- [Install](#install)
- [Usage](#usage)
- [Type mapping](#type-mapping)
- [Limitations](#limitations)
- [API](#api)
- [Credits](#credits)
- [Contributing](#contributing)
- [License](#license)

## How it works

Every schema node gets the most precise F# type the provider can give it:

- **Refined in the type.** Array size limits (`minItems`, `maxItems`, `uniqueItems`) and integer
  bounds become F# types that only hold valid values, such as `int * int list` for a non-empty
  array or `byte` for 0–255. `Create` then evaluates to the value directly, since nothing is left
  to check.
- **Checked at runtime.** Constraints F# can't express, like `pattern` or `format`, are validated
  when you call `Create` or `Parse`, which then evaluate to `Result<'T, string list>`. You can't get
  at the value without handling the `Error` case.

The return type of `Create` therefore tells you which one you got. `Parse` always evaluates to a
`Result`, since its input is an arbitrary string.

Schemas are parsed and validated with [NJsonSchema](https://github.com/RicoSuter/NJsonSchema) 11,
and JSON values are FSharp.Data's `JsonValue`. The provider can only follow the JSON Schema
specification as far as NJsonSchema does; see [Limitations](#limitations).

## Install

You need the .NET SDK 10. The library targets `netstandard2.0`. The fork isn't on NuGet yet, so
pack it locally, with a version that can't clash with the original package:

```bash
git clone https://github.com/Larsen00/JsonSchemaProvider.git
cd JsonSchemaProvider
dotnet pack src/JsonSchemaProvider.Runtime/JsonSchemaProvider.fsproj -c Release -p:IsNuGet=true -p:Version=0.2.0-local -o ./nupkg
```

`-p:IsNuGet=true` is required, since it bundles the design-time assembly. Then, in your project:

```bash
dotnet nuget add source /absolute/path/to/JsonSchemaProvider/nupkg -n jsonschemaprovider-local
dotnet add package JsonSchemaProvider --version 0.2.0-local
```

Alternatively, reference the projects directly:

```xml
<ProjectReference Include="..\JsonSchemaProvider\src\JsonSchemaProvider.Runtime\JsonSchemaProvider.fsproj" />
<ProjectReference Include="..\JsonSchemaProvider\src\JsonSchemaProvider.DesignTime\JsonSchemaProvider.DesignTime.fsproj"
                  ReferenceOutputAssembly="false" />
```

Or, in an F# script, after a `dotnet build`:

```fsharp
#I "path/to/JsonSchemaProvider/src/JsonSchemaProvider.Runtime/bin/Debug/netstandard2.0"
#r "FSharp.Data.Json.Core.dll"
#r "NJsonSchema.dll"
#r "JsonSchemaProvider.dll"
```

## Usage

### Objects

```fsharp
open JsonSchemaProvider

[<Literal>]
let personSchema = """
{
  "type": "object",
  "properties": {
    "name": { "type": "string" },
    "age":  { "type": "integer", "minimum": 0, "maximum": 255 },
    "tags": { "type": "array", "items": { "type": "string" } }
  },
  "required": ["name", "tags"]
}"""

type Person = JsonSchemaProvider<schema = personSchema>

match Person.Parse """{ "name": "Ada", "age": 36, "tags": ["math"] }""" with
| Ok person -> printfn "%s is %A" person.name person.age   // Ada is Some 36uy
| Error errors -> printfn "%A" errors

Person.Parse """{ "age": "old" }"""
// Error ["PropertyRequired: #/name"; "IntegerExpected: #/age"; "PropertyRequired: #/tags"]

let grace = Person.Create(name = "Grace", tags = [ "navy" ])   // Result, since a string may be null
```

Required properties have their plain type and optional ones are options (`age : byte option`).
`ToString()` on a value gives its JSON.

### Refined arrays

Add `"minItems": 1` to `tags` and `"maxItems": 2` to a `phones` array, and the types change:

| Property | F# type | Example |
|---|---|---|
| `tags` | `string * string list` | `("admin", [])` |
| `phones` | `(string * string option) option` | `Some ("+45 1234", None)` |

Passing `tags = []` is now a compile error (FS0001), not a runtime error. The general rule is that
`minItems` items come first as pairs, and the room `maxItems` leaves becomes a chain of options:

| Keywords | F# type |
|---|---|
| none | `'T list` |
| `minItems: 2` | `'T * ('T * 'T list)` |
| `maxItems: 1` | `'T option` |
| `maxItems: 2` | `('T * 'T option) option` |
| `minItems: 1, maxItems: 2` | `'T * 'T option` |
| `minItems: 1, maxItems: 1` | `'T` |
| `minItems: 3, maxItems: 3` | `'T * ('T * 'T)` |

Every array type has a `ToList` helper, and fixed sizes also have `ToTuple`:

```fsharp
[<Literal>]
let rgbSchema = """{ "type": "array", "items": { "type": "integer", "minimum": 0, "maximum": 255 }, "minItems": 3, "maxItems": 3 }"""
type Rgb = JsonSchemaProvider<schema = rgbSchema>

let colour = Rgb.Create((255uy, (128uy, 0uy)))   // no Result needed
Rgb.ToTuple colour                               // (255uy, 128uy, 0uy)
Rgb.ToList colour                                // [255uy; 128uy; 0uy]
```

With `uniqueItems`, `Create` drops duplicates and `Parse` rejects them. This applies to primitive
items only; arrays of objects, and `uniqueItems` combined with `minItems`, are checked at runtime.

### Refined integers

A JSON integer can be any size, but an F# integer type can't. So an integer needs a lower bound
(`minimum` or `exclusiveMinimum`) and an upper bound (`maximum` or `exclusiveMaximum`). The bounds
pick the smallest type that holds them, unsigned if the lower bound is at least 0. If the bounds
match the type's range exactly, `Create` needs no `Result`:

| Bounds | F# type | `Create` evaluates to |
|---|---|---|
| 0 to 255 | `byte` | `byte` |
| -2147483648 to 2147483647 | `int` | `int` |
| 0 to 100 | `byte` | `Result<byte, string list>` |
| 1 to 65535 | `uint16` | `Result<uint16, string list>` |
| none, or only one side | compile error | |

An integer without both bounds is a compile error that says what to add:

```
The integer at 'properties/age' has no upper bound, so valid numbers can overflow its F# type (uint32).
Pick ONE of these fixes:
  A) Add "maximum" or "exclusiveMaximum" to it.
     For uint32: "maximum": 4294967295
  B) Or set allowUnboundedIntegers=true. This turns the check off for every integer in the schema,
     and numbers outside the type then throw at runtime.
```

With `allowUnboundedIntegers = true` a missing bound is allowed: no bounds gives `int`, and one
bound gives a 32-bit type (`minimum 0` gives `uint32`) or larger. A valid number outside that type
then makes `Parse` throw.

When there are several bounds on one side, the loosest one picks the type and the stricter one is
checked at runtime. So you can choose the type yourself: `minimum 0, maximum 10, exclusiveMaximum 256`
gives `byte` with `maximum 10` validated, and `exclusiveMaximum 4294967296` instead gives `uint32`.

A schema no value can satisfy (minimum above maximum, `minItems` above `maxItems`) is a compile
error.

### oneOf

A `oneOf` becomes a `Choice`, and object branches get their own types:

```fsharp
// "payment": { "oneOf": [ { object with "iban" }, { object with "cardNumber" } ] }
type Order = JsonSchemaProvider<schema = orderSchema>

match Order.Parse json with
| Ok order ->
    match order.payment with
    | Choice1Of2 bank -> printfn "Bank transfer from %s" bank.iban
    | Choice2Of2 card -> printfn "Card %s" card.cardNumber
| Error errors -> printfn "%A" errors

let bank = Order.paymentCase1.Create(iban = "DK5000400440116243")
```

The branch is found by validation, trying the branches in order. JSON that matches more than one
branch is rejected, as the spec requires.

### Schema files

```fsharp
[<Literal>]
let path = __SOURCE_DIRECTORY__ + "/schemas/config.schema.json"
type Config = JsonSchemaProvider<schemaFile = path>
```

Use `__SOURCE_DIRECTORY__`, because a relative path is resolved against the compiler's working
directory.

## Type mapping

| JSON Schema | F# type |
|---|---|
| `boolean` / `number` / `string` | `bool` / `float` / `string` |
| `integer` | a sized integer type picked from its bounds, from `sbyte` to `uint64` |
| `object` | a provided type with one property per schema property |
| `array` | `'T list`, or a pair/option shape with size keywords |
| `oneOf` | `Choice<A, B>`, nested for more branches |

| Handled by | Keywords |
|---|---|
| The type | `type`, `properties`, `required`, `items`, `oneOf`, `minItems`, `maxItems`, `uniqueItems`, integer bounds |
| Runtime validation | `pattern`, `minLength`, `maxLength`, `format`, `enum`, `multipleOf`, number bounds, `minProperties`, `maxProperties`, `additionalProperties`, ... |
| Ignored | `title`, `description`, `default`, `examples`, `$comment`, `deprecated` |

`Create` also evaluates to a `Result` when a node contains a `string` or a nested object, since
.NET allows `null` for both, or a `oneOf` with more than one branch.

## Limitations

Compile errors:

- `$ref` isn't supported.
- Every node needs a `type` or a `oneOf`, so `{}` and a bare `anyOf`/`allOf` fail.
- `"type": "null"` and type arrays like `["string", "null"]` aren't supported.
- Tuple-form `items` fails with an unhelpful null-reference error.
- An integer without both a lower and an upper bound, unless `allowUnboundedIntegers = true`.

Compiles, but beware:

- NJsonSchema 11 ignores some keywords when validating: `const`, `dependencies`, `contains`,
  `prefixItems`, `dependentRequired`, `propertyNames`, `unevaluatedProperties` and others.
- `maxItems: 0` is treated as absent.
- `Create` relabels a value passed with the wrong `Choice` constructor instead of rejecting it.
- Two `oneOf` branches that are both arrays of objects get item types with the same name.
- With `allowUnboundedIntegers = true`, `Parse` can raise on numbers too big for the integer type.
- NJsonSchema can't validate an integer outside the `int64` range, so anything that validates it
  evaluates to an `Error`, even for a valid value. That covers `Parse` and a validating `Create`, for
  the upper half of `uint64` and for an integer literal that large in a `number` node.
  `Create` on a node that needs no validation still accepts the value.

## API

**Static parameters:**

| Parameter | Default | Meaning |
|---|---|---|
| `schema` | `""` | The schema as a string literal. |
| `schemaFile` | `""` | Path to a schema file. Set exactly one of the two. |
| `skipRuntimeValidation` | `false` | No runtime validation: `Create` never returns a `Result`, and `Parse` only checks the JSON syntax. For trusted data only. |
| `ignoreSpecificKeywords` | `false` | Plain lists and `int` instead of refined types; the keywords are checked at runtime instead. Integer bounds must still fit in `int`. |
| `allowUnboundedIntegers` | `false` | Allows integers without both bounds. Their type can't hold every valid number, so `Parse` can raise on one that overflows. |

**Methods:** every provided type except the array helpers has `Create` (named arguments for objects, one `value` otherwise)
and `Parse : string -> Result<'T, string list>`. Errors are NJsonSchema messages such as
`"NumberTooBig: #/age"`.

**Nested types** are named after where they appear: `Root.addressObj` for a property, `Root.rowsItem`
for array items, `Root.paymentCase1` for a `oneOf` branch, and `Root.Item`/`Root.Case1` at the root.
Array helpers live on `Root.valuesArray`, `...ItemArray` and so on.

## Credits

The foundation is [florenzen/JsonSchemaProvider](https://github.com/florenzen/JsonSchemaProvider) by
**Florian Lorenzen**: the type provider itself, NJsonSchema and FSharp.Data underneath, the
`Create`/`Parse` API, inline and file schemas, and object types. If his version covers what you
need, use [his package](https://www.nuget.org/packages/JsonSchemaProvider/).

This fork adds refined types for arrays and integers, `oneOf`, non-object roots, `Result` instead
of exceptions, validation of every node, and the three flags. Coming from the original, expect
`Result` return types, sized integers that need both bounds, tuple-shaped arrays and `<name>Item`
instead of `<name>Obj` for array items. `ignoreSpecificKeywords = true` with
`allowUnboundedIntegers = true` brings back plain lists and `int`.

## Contributing

Issues and pull requests are welcome, especially a schema that breaks the provider. Likely next
steps are `$ref`, `allOf`/`anyOf`, nullable types and a NuGet release.

```bash
dotnet build
dotnet test
```

If an open editor locks the provider DLLs (MSB3027), `scripts/rebuild.ps1 -Mode test` stops Ionide's
language server while building.

## License

[MIT](LICENSE) © 2024 Florian Lorenzen. The fork uses the same license.
