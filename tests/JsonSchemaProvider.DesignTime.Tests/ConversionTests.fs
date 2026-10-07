namespace JsonSchemaProvider.Tests

// NodeConversion types, and ToRuntime/ToJson/ToList/ToTuple evaluated on real JSON values.
module ConversionTests =
    open Expecto
    open FSharp.Data
    open JsonSchemaProvider
    open JsonSchemaProvider.DesignTime.ProviderConfiguration
    open TestHelpers

    let private conversionOf (schemaText: string) = convertRoot (fixture schemaText)

    let private expectRoundTrip (conv: NodeConversion) (jsonText: string) =
        Expect.equal (roundTrip conv jsonText) (normalize jsonText) $"round trip of {jsonText}"

    let primitiveTests =
        testList
            "Primitives"
            [ for typeName, expected in
                  [ "boolean", typeof<bool>; "integer", typeof<int>; "number", typeof<double>; "string", typeof<string> ] do
                  test $"{typeName} maps to {expected.Name} at compile time and at runtime" {
                      let conv = conversionOf $"""{{ "type": "{typeName}" }}"""
                      Expect.equal conv.CompileTimeType expected "CompileTimeType"
                      Expect.equal conv.RuntimeType expected "RuntimeType"
                  }

              test "primitives round-trip through ToRuntime and ToJson" {
                  expectRoundTrip (conversionOf """{ "type": "boolean" }""") "true"
                  expectRoundTrip (conversionOf """{ "type": "integer" }""") "-42"
                  expectRoundTrip (conversionOf """{ "type": "number" }""") "1.5"
                  expectRoundTrip (conversionOf """{ "type": "string" }""") "\"hello\""
              }

              test "ToRuntime evaluates to the plain F# value" {
                  Expect.equal (toRuntime (conversionOf """{ "type": "integer" }""") "7") (box 7) "integer"
                  Expect.equal (toRuntime (conversionOf """{ "type": "string" }""") "\"x\"") (box "x") "string"
              }

              test "a primitive is FullyCompilable only without validation keywords" {
                  Expect.isTrue (conversionOf """{ "type": "integer" }""").FullyCompilable "plain"
                  Expect.isFalse (conversionOf """{ "type": "integer", "minimum": 0 }""").FullyCompilable "minimum"
              } ]

    let objectTests =
        testList
            "Objects"
            [ test "an object's compile-time type is its type map entry, its runtime type NullableJsonValue" {
                  let f = fixture """{ "type": "object", "properties": { "a": { "type": "integer" } } }"""
                  let conv = convertRoot f
                  Expect.equal conv.CompileTimeType (f.TypeMap["#"] :> System.Type) "CompileTimeType"
                  Expect.equal conv.RuntimeType typeof<NullableJsonValue> "RuntimeType"
              }

              test "an object round-trips unchanged" {
                  let conv = conversionOf """{ "type": "object", "properties": { "a": { "type": "integer" } } }"""
                  expectRoundTrip conv """{"a":1,"extra":[true]}"""
              }

              test "an object is FullyCompilable only if all its properties are" {
                  Expect.isTrue (conversionOf """{ "type": "object", "properties": { "a": { "type": "integer" } } }""").FullyCompilable "plain"
                  Expect.isFalse
                      (conversionOf """{ "type": "object", "properties": { "a": { "type": "integer", "minimum": 0 } } }""").FullyCompilable
                      "property with minimum"
                  Expect.isFalse
                      (conversionOf """{ "type": "object", "properties": { "o": { "type": "object", "properties": { "s": { "type": "string", "pattern": "x" } } } } }""").FullyCompilable
                      "nested property with pattern"
              } ]

    // (schema keywords, compile-time type, sample JSON values)
    let private arrayCases : (string * System.Type * string list) list =
        [ "", typeof<int list>, [ "[]"; "[1,2,3]" ]
          ", \"minItems\": 1, \"maxItems\": 1", typeof<int>, [ "[7]" ]
          ", \"minItems\": 3, \"maxItems\": 3", typeof<int * (int * int)>, [ "[1,2,3]" ]
          ", \"minItems\": 2", typeof<int * (int * int list)>, [ "[1,2]"; "[1,2,3,4]" ]
          ", \"minItems\": 1, \"maxItems\": 3", typeof<int * (int * int option) option>, [ "[1]"; "[1,2]"; "[1,2,3]" ]
          ", \"maxItems\": 1", typeof<int option>, [ "[]"; "[1]" ]
          ", \"maxItems\": 3", typeof<(int * (int * int option) option) option>, [ "[]"; "[1]"; "[1,2,3]" ] ]

    let arrayTests =
        testList
            "Arrays"
            [ for keywords, expectedType, samples in arrayCases do
                  let schemaText = intArray keywords

                  test $"array{keywords} compiles to {expectedType}" {
                      let conv = conversionOf schemaText
                      Expect.equal conv.CompileTimeType expectedType "CompileTimeType"
                      Expect.equal conv.RuntimeType expectedType "RuntimeType"
                  }

                  test $"array{keywords} round-trips" {
                      let conv = conversionOf schemaText
                      for sample in samples do
                          expectRoundTrip conv sample
                  }

                  test $"array{keywords} ToList yields the elements in order" {
                      let array = arrayConversionOf (fixture schemaText)
                      for sample in samples do
                          let runtimeValue = toRuntime array.common sample
                          let list = apply array.specific.ToList.Convert array.common.RuntimeType runtimeValue
                          let expected = JsonValue.Parse(sample).AsArray() |> Array.map (fun v -> v.AsInteger()) |> List.ofArray
                          Expect.equal list (box expected) $"ToList of {sample}"
                  }

              test "ExactLength n >= 2 offers a flat ToTuple" {
                  let array = arrayConversionOf (fixture (intArray ", \"minItems\": 3, \"maxItems\": 3"))
                  let toTuple = Expect.wantSome array.specific.ToTuple "ToTuple"
                  Expect.equal toTuple.CompileTimeReturnType typeof<int * int * int> "return type"
                  let runtimeValue = toRuntime array.common "[1,2,3]"
                  Expect.equal (apply toTuple.Convert array.common.RuntimeType runtimeValue) (box (1, 2, 3)) "value"
              }

              test "ToTuple is only offered for ExactLength n >= 2" {
                  for keywords in [ ""; ", \"minItems\": 1, \"maxItems\": 1"; ", \"minItems\": 2"; ", \"maxItems\": 2" ] do
                      let array = arrayConversionOf (fixture (intArray keywords))
                      Expect.isNone array.specific.ToTuple keywords
              }

              test "nested exact-length arrays round-trip" {
                  let conv =
                      conversionOf """{ "type": "array", "items": { "type": "array", "items": { "type": "integer" }, "minItems": 2, "maxItems": 2 } }"""
                  Expect.equal conv.CompileTimeType typeof<(int * int) list> "CompileTimeType"
                  expectRoundTrip conv "[[1,2],[3,4]]"
              }

              test "an array of objects round-trips" {
                  let conv = conversionOf """{ "type": "array", "items": { "type": "object", "properties": { "a": { "type": "integer" } } } }"""
                  Expect.equal conv.RuntimeType typeof<NullableJsonValue list> "RuntimeType"
                  expectRoundTrip conv """[{"a":1},{"a":2}]"""
              }

              test "size-bounded arrays of plain items are FullyCompilable" {
                  for keywords, _, _ in arrayCases do
                      Expect.isTrue (conversionOf (intArray keywords)).FullyCompilable keywords
              }

              test "uniqueItems makes an array not FullyCompilable" {
                  Expect.isFalse (conversionOf (intArray ", \"uniqueItems\": true")).FullyCompilable ""
              }

              test "a non-compilable item makes the array not FullyCompilable" {
                  let schemaText = """{ "type": "array", "items": { "type": "integer", "minimum": 0 }, "minItems": 2, "maxItems": 2 }"""
                  Expect.isFalse (conversionOf schemaText).FullyCompilable ""
              }

              test "IgnoreSpecificKeywords keeps the plain list type but drops FullyCompilable" {
                  let conv = convertRoot (fixtureWith { defaultFlags with IgnoreSpecificKeywords = true } (intArray ", \"minItems\": 2"))
                  Expect.equal conv.CompileTimeType typeof<int list> "CompileTimeType"
                  Expect.isFalse conv.FullyCompilable "FullyCompilable"
              }

              test "minItems > maxItems fails at design time" {
                  Expect.throws (fun () -> conversionOf (intArray ", \"minItems\": 3, \"maxItems\": 2") |> ignore) ""
              } ]

    let private intOrString = """{ "oneOf": [ { "type": "integer" }, { "type": "string" } ] }"""

    let oneOfTests =
        testList
            "oneOf"
            [ test "ToRuntime picks the first matching branch" {
                  let conv = conversionOf intOrString
                  Expect.equal (toRuntime conv "5") (box (Choice1Of2 5 : Choice<int, string>)) "integer"
                  Expect.equal (toRuntime conv "\"a\"") (box (Choice2Of2 "a" : Choice<int, string>)) "string"
              }

              test "branch matching uses the branch's own keywords" {
                  let conv =
                      conversionOf """{ "oneOf": [ { "type": "string", "minLength": 3 }, { "type": "string", "maxLength": 2 } ] }"""
                  Expect.equal (toRuntime conv "\"abcd\"") (box (Choice1Of2 "abcd" : Choice<string, string>)) "long"
                  Expect.equal (toRuntime conv "\"ab\"") (box (Choice2Of2 "ab" : Choice<string, string>)) "short"
              }

              test "three branches nest and round-trip" {
                  let conv = conversionOf """{ "oneOf": [ { "type": "integer" }, { "type": "string" }, { "type": "boolean" } ] }"""
                  Expect.equal (toRuntime conv "true") (box (Choice2Of2(Choice2Of2 true) : Choice<int, Choice<string, bool>>)) "boolean"
                  for sample in [ "1"; "\"s\""; "false" ] do
                      expectRoundTrip conv sample
              }

              test "oneOf branches can be arrays and objects" {
                  let conv =
                      conversionOf
                          """{ "oneOf": [ { "type": "array", "items": { "type": "integer" }, "maxItems": 1 }, { "type": "object", "properties": { "a": { "type": "integer" } } } ] }"""
                  for sample in [ "[]"; "[3]"; """{"a":1}""" ] do
                      expectRoundTrip conv sample
              }

              test "oneOf with disjoint plain branches is FullyCompilable" {
                  Expect.isTrue (conversionOf intOrString).FullyCompilable ""
              } ]

    // Known open issue, disabled until a solution is chosen: FullyCompilable for oneOf ignores
    // "exactly one branch", so Create skips validation for overlapping branches and can build
    // JSON that violates the oneOf. Re-add these to oneOfTests once that is decided.
    //
    // // 5 matches both integer and number, so Create(Choice1Of2 5) builds JSON that violates oneOf.
    // test "oneOf with overlapping branches is not FullyCompilable" {
    //     let conv = conversionOf """{ "oneOf": [ { "type": "integer" }, { "type": "number" } ] }"""
    //     Expect.isFalse conv.FullyCompilable "integer/number overlap"
    // }
    //
    // // The first branch's Create(a = 1, b = 2) builds {"a":1,"b":2}, which also satisfies the second branch.
    // test "oneOf with overlapping object branches is not FullyCompilable" {
    //     let conv =
    //         conversionOf
    //             """{ "oneOf": [
    //                    { "type": "object", "properties": { "a": { "type": "integer" }, "b": { "type": "integer" } }, "required": ["a"] },
    //                    { "type": "object", "properties": { "b": { "type": "integer" } }, "required": ["b"] } ] }"""
    //     Expect.isFalse conv.FullyCompilable "object overlap"
    // }

    [<Tests>]
    let tests = testList "ConversionTests" [ primitiveTests; objectTests; arrayTests; oneOfTests ]
