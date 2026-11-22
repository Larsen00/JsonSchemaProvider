namespace RefinedJsonSchemaProvider.Tests

module ArrayShapeTests =
    open Expecto
    open RefinedJsonSchemaProvider.DesignTime.SchemaConversion
    open RefinedJsonSchemaProvider.DesignTime.ArrayShape
    open RefinedJsonSchemaProvider.DesignTime.ProviderConfiguration
    open TestHelpers

    let private classifyWith (flags: CompileFlags) (schemaText: string) =
        match parseJsonSchema schemaText with
        | JsonArray(inner, keywords) -> classifyArrayShape flags keywords
        | other -> failwithf "Expected an array schema, got %A" other

    let private classify = classifyWith defaultFlags

    let private shapeName (shape: ArrayShape) =
        match shape with
        | InvalidBounds -> "InvalidBounds"
        | UnsupportedKeywords -> "UnsupportedKeywords"
        | ExactLength n -> $"ExactLength {n}"
        | MinItemsPrefix minItems -> $"MinItemsPrefix {minItems}"
        | MinMaxItems(minItems, maxItems) -> $"MinMaxItems {minItems} {maxItems}"
        | MaxItemsSingle -> "MaxItemsSingle"
        | MaxItemsChain maxItems -> $"MaxItemsChain {maxItems}"
        | Unbounded -> "Unbounded"
        | UniqueUnbounded -> "UniqueUnbounded"
        | UniqueMaxItems maxItems -> $"UniqueMaxItems {maxItems}"

    let private expectShape (schemaText: string) (expected: string) =
        Expect.equal (shapeName (classify schemaText)) expected schemaText

    [<Tests>]
    let tests =
        testList
            "ArrayShapeTests"
            [ test "no size keywords is Unbounded" { expectShape (intArray "") "Unbounded" }

              test "explicit minItems 0 is Unbounded" { expectShape (intArray ", \"minItems\": 0") "Unbounded" }

              test "maxItems 1 alone is MaxItemsSingle" { expectShape (intArray ", \"maxItems\": 1") "MaxItemsSingle" }

              test "maxItems 3 alone is MaxItemsChain" { expectShape (intArray ", \"maxItems\": 3") "MaxItemsChain 3" }

              test "minItems = maxItems is ExactLength" {
                  expectShape (intArray ", \"minItems\": 3, \"maxItems\": 3") "ExactLength 3"
              }

              test "minItems = maxItems = 1 is ExactLength 1" {
                  expectShape (intArray ", \"minItems\": 1, \"maxItems\": 1") "ExactLength 1"
              }

              test "minItems alone is MinItemsPrefix" {
                  expectShape (intArray ", \"minItems\": 2") "MinItemsPrefix 2"
              }

              test "minItems < maxItems is MinMaxItems" {
                  expectShape (intArray ", \"minItems\": 1, \"maxItems\": 3") "MinMaxItems 1 3"
              }

              test "minItems > maxItems is InvalidBounds" {
                  expectShape (intArray ", \"minItems\": 3, \"maxItems\": 2") "InvalidBounds"
              }

              test "InvalidBounds wins over uniqueItems" {
                  expectShape (intArray ", \"minItems\": 3, \"maxItems\": 2, \"uniqueItems\": true") "InvalidBounds"
              }

              test "uniqueItems alone is UniqueUnbounded" {
                  expectShape (intArray ", \"uniqueItems\": true") "UniqueUnbounded"
                  expectShape (intArray ", \"uniqueItems\": false") "Unbounded"
              }

              test "uniqueItems with maxItems is UniqueMaxItems" {
                  expectShape (intArray ", \"maxItems\": 1, \"uniqueItems\": true") "UniqueMaxItems 1"
                  expectShape (intArray ", \"maxItems\": 3, \"uniqueItems\": true") "UniqueMaxItems 3"
              }

              test "uniqueItems on object items gets the unique shapes too" {
                  let objectItems = """{ "type": "array", "items": { "type": "object" }, "uniqueItems": true"""
                  expectShape (objectItems + " }") "UniqueUnbounded"
                  expectShape (objectItems + ", \"maxItems\": 3 }") "UniqueMaxItems 3"
              }

              test "uniqueItems with minItems falls back to UnsupportedKeywords" {
                  expectShape (intArray ", \"minItems\": 2, \"maxItems\": 2, \"uniqueItems\": true") "UnsupportedKeywords"
                  expectShape (intArray ", \"minItems\": 2, \"uniqueItems\": true") "UnsupportedKeywords"
                  expectShape (intArray ", \"minItems\": 1, \"maxItems\": 3, \"uniqueItems\": true") "UnsupportedKeywords"
              }

              test "RefinedTypes off wins over uniqueItems" {
                  let flags = { defaultFlags with RefinedTypes = false }
                  Expect.equal (shapeName (classifyWith flags (intArray ", \"uniqueItems\": true"))) "UnsupportedKeywords" ""
              }

              test "additionalItems false forces UnsupportedKeywords" {
                  expectShape (intArray ", \"additionalItems\": false") "UnsupportedKeywords"
              }

              test "an additionalItems schema forces UnsupportedKeywords" {
                  expectShape (intArray ", \"additionalItems\": { \"type\": \"string\" }") "UnsupportedKeywords"
              }

              test "RefinedTypes off turns a size-bounded array into UnsupportedKeywords" {
                  let flags = { defaultFlags with RefinedTypes = false }
                  let shape = classifyWith flags (intArray ", \"minItems\": 2")
                  Expect.equal (shapeName shape) "UnsupportedKeywords" ""
              }
 ]
