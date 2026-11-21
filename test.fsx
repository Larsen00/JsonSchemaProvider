#I "src/RefinedJsonSchemaProvider.Runtime/bin/Debug/netstandard2.0"
#r "FSharp.Data.Json.Core.dll"
#r "NJsonSchema.dll"
#r "RefinedJsonSchemaProvider.dll"

open RefinedJsonSchemaProvider

// Compile error with requireIntegerBounds=true: no bounds at all.
// [<Literal>]
// let unboundedSchema = """{ "type": "integer" }"""
// type Unbounded = RefinedJsonSchemaProvider<schema=unboundedSchema, requireIntegerBounds=true>

// // Compile error: a nested property with only a lower bound.
// [<Literal>]
// let lowerOnlySchema = """{ "type": "object", "properties": { "age": { "type": "integer", "minimum": 0 } } }"""
// type LowerOnly = RefinedJsonSchemaProvider<schema=lowerOnlySchema, requireIntegerBounds=true>

// // Compile error: both bounds, but no integer type holds the range.
// [<Literal>]
// let tooWideSchema = """{ "type": "integer", "minimum": 0, "maximum": 18446744073709551616 }"""
// type TooWide = RefinedJsonSchemaProvider<schema=tooWideSchema, requireIntegerBounds=true>

// Fix 1: add the bounds. Compiles to int.
[<Literal>]
let boundedSchema = """{ "type": "integer", "minimum": -2147483648, "maximum": 2147483647 }"""
type Bounded = RefinedJsonSchemaProvider<schema=boundedSchema>

// Fix 1 with exclusive bounds: compiles to byte (0..255).
[<Literal>]
let exclusiveSchema = """{ "type": "integer", "exclusiveMinimum": -1, "exclusiveMaximum": 256 }"""
type Exclusive = RefinedJsonSchemaProvider<schema=exclusiveSchema>

// // Fix 2: leave requireIntegerBounds off (the default). Compiles to int, Parse can throw on overflow.
// type Allowed = RefinedJsonSchemaProvider<schema=unboundedSchema>



// type datatype<'a> =
//     | Nil
//     | Cons of 'a * datatype<'a>
// type recType<'a> =
//     | SingleTon of 'a * Nil
