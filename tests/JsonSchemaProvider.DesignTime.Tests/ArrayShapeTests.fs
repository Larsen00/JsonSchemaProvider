namespace JsonSchemaProvider.Tests

module ArrayShapeTests =
    open Expecto
    open JsonSchemaProvider.DesignTime.SchemaConversion
    open JsonSchemaProvider.DesignTime.ArrayShape
    open JsonSchemaProvider.DesignTime.ProviderConfiguration
    open TestHelpers

    let private classifyWith (flags: CompileFlags) (schemaText: string) =
        match parseJsonSchema schemaText with
        | JsonArray(inner, keywords) -> classifyArrayShape flags inner keywords
        | other -> failwithf "Expected an array schema, got %A" other

    let private classify = classifyWith defaultFlags

    let private shapeName (shape: ArrayShape) =
        match shape with
        | InvalidBounds _ -> "InvalidBounds"
        | UnsupportedKeywords _ -> "UnsupportedKeywords"
        | ExactLength(_, _, n) -> $"ExactLength {n}"
        | MinItemsPrefix(_, _, minItems, maxItems) -> $"MinItemsPrefix {minItems} {maxItems}"
        | MaxItemsSingle _ -> "MaxItemsSingle"
        | MaxItemsChain(_, _, maxItems) -> $"MaxItemsChain {maxItems}"
        | Unbounded _ -> "Unbounded"

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

              test "minItems alone is MinItemsPrefix without an upper bound" {
                  expectShape (intArray ", \"minItems\": 2") "MinItemsPrefix 2 "
              }

              test "minItems < maxItems is MinItemsPrefix with an upper bound" {
                  expectShape (intArray ", \"minItems\": 1, \"maxItems\": 3") "MinItemsPrefix 1 Some(3)"
              }

              test "minItems > maxItems is InvalidBounds" {
                  expectShape (intArray ", \"minItems\": 3, \"maxItems\": 2") "InvalidBounds"
              }

              test "InvalidBounds wins over uniqueItems" {
                  expectShape (intArray ", \"minItems\": 3, \"maxItems\": 2, \"uniqueItems\": true") "InvalidBounds"
              }

              test "uniqueItems forces UnsupportedKeywords even with size bounds" {
                  expectShape (intArray ", \"minItems\": 2, \"maxItems\": 2, \"uniqueItems\": true") "UnsupportedKeywords"
              }

              test "additionalItems false forces UnsupportedKeywords" {
                  expectShape (intArray ", \"additionalItems\": false") "UnsupportedKeywords"
              }

              test "an additionalItems schema forces UnsupportedKeywords" {
                  expectShape (intArray ", \"additionalItems\": { \"type\": \"string\" }") "UnsupportedKeywords"
              }

              test "IgnoreSpecificKeywords turns a size-bounded array into UnsupportedKeywords" {
                  let flags = { defaultFlags with IgnoreSpecificKeywords = true }
                  let shape = classifyWith flags (intArray ", \"minItems\": 2")
                  Expect.equal (shapeName shape) "UnsupportedKeywords" ""
              }
 ]
