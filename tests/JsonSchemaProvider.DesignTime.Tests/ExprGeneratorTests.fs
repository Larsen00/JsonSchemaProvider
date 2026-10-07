namespace JsonSchemaProvider.Tests

// Evaluates the Create/Parse/getter code ExprGenerator builds, without going through the compiler.
module ExprGeneratorTests =
    open System
    open Expecto
    open FSharp.Quotations
    open FSharp.Data
    open JsonSchemaProvider
    open JsonSchemaProvider.DesignTime.SchemaConversion
    open JsonSchemaProvider.DesignTime.NodeConversions
    open JsonSchemaProvider.DesignTime.ExprGenerator
    open TestHelpers

    let private parse (f: Fixture) (path: string) (schemaType: JsonSchemaType) (jsonText: string) : obj =
        let conv = convert f.Context f.TypeMap schemaType
        eval (generateParseInvokeCode f.Context path conv.RuntimeType conv.ToRuntime [ Expr.Value jsonText ])

    let private parseRoot (f: Fixture) (jsonText: string) = parse f "#" f.Root jsonText

    let private create (f: Fixture) (args: Expr list) : obj = eval (generateCreateInvokeCode f.Context f.TypeMap f.Root args)

    let private jsonOf (value: obj) = compact (value :?> NullableJsonValue).JsonVal

    let private okJson (result: obj) =
        match result :?> Result<NullableJsonValue, string list> with
        | Ok value -> compact value.JsonVal
        | Error errors -> failtestf "Expected Ok, got Error %A" errors

    let private isErrorResult (result: obj) =
        let case, _ = Reflection.FSharpValue.GetUnionFields(result, result.GetType())
        case.Name = "Error"

    let private nullable<'T when 'T: struct and 'T: (new: unit -> 'T) and 'T :> ValueType> (value: 'T option) =
        match value with
        | Some v -> Expr.Value(Nullable v, typeof<Nullable<'T>>)
        | None -> Expr.Value(null, typeof<Nullable<'T>>)

    let private boundedInt = """{ "type": "integer", "minimum": 5 }"""

    let private person =
        """{ "type": "object",
             "properties": {
               "name": { "type": "string" },
               "age": { "type": "integer", "minimum": 0 },
               "nick": { "type": "string" } },
             "required": ["name"] }"""

    let parseTests =
        testList
            "Parse"
            [ test "valid JSON is Ok with the converted value" {
                  Expect.equal (parseRoot (fixture boundedInt) "7") (box (Ok 7 : Result<int, string list>)) ""
              }

              test "JSON violating the schema is Error" {
                  Expect.isTrue (isErrorResult (parseRoot (fixture boundedInt) "3")) "below minimum"
                  Expect.isTrue (isErrorResult (parseRoot (fixture boundedInt) "\"7\"")) "wrong JSON type"
              }

              test "malformed JSON is Error, not an exception" {
                  Expect.isTrue (isErrorResult (parseRoot (fixture boundedInt) "{not json")) ""
              }

              test "SkipRuntimeValidation only checks syntax" {
                  let f = fixtureWith { defaultFlags with SkipRuntimeValidation = true } boundedInt
                  Expect.equal (parseRoot f "3") (box (Ok 3 : Result<int, string list>)) "below minimum accepted"
                  Expect.isTrue (isErrorResult (parseRoot f "{not json")) "malformed still rejected"
              }

              test "an object parses to its JSON" {
                  Expect.equal (okJson (parseRoot (fixture person) """{"name":"a","age":3}""")) """{"name":"a","age":3}""" ""
              }

              test "an object missing a required property is Error" {
                  Expect.isTrue (isErrorResult (parseRoot (fixture person) """{"age":3}""")) ""
              }

              test "a nested object's Parse validates against its own sub-schema only" {
                  let f =
                      fixture
                          """{ "type": "object",
                               "properties": { "inner": { "type": "object", "properties": { "x": { "type": "integer" } }, "required": ["x"] } },
                               "required": ["inner"] }"""
                  let inner = match f.Root with JsonObject(_, [ _, inner ]) -> inner | other -> failtestf "%A" other
                  Expect.equal (okJson (parse f "#/properties/inner" inner """{"x":1}""")) """{"x":1}""" "valid"
                  Expect.isTrue (isErrorResult (parse f "#/properties/inner" inner "{}")) "missing x"
              }

              test "a oneOf root parses into the matching Choice" {
                  let f = fixture """{ "oneOf": [ { "type": "integer" }, { "type": "string" } ] }"""
                  Expect.equal (parseRoot f "\"s\"") (box (Ok(Choice2Of2 "s") : Result<Choice<int, string>, string list>)) ""
              } ]

    let createTests =
        testList
            "Create"
            [ test "a compilable primitive root evaluates to the argument itself" {
                  let f = fixture """{ "type": "integer" }"""
                  Expect.equal (create f [ Expr.Value 3 ]) (box 3) ""
              }

              test "a validated primitive root evaluates to Ok or Error" {
                  let f = fixture boundedInt
                  Expect.equal (create f [ Expr.Value 7 ]) (box (Ok 7 : Result<int, string list>)) "valid"
                  Expect.isTrue (isErrorResult (create f [ Expr.Value 3 ])) "below minimum"
              }

              test "a validated array root is checked as JSON" {
                  let f = fixture """{ "type": "array", "items": { "type": "integer", "minimum": 0 } }"""
                  Expect.equal (create f [ Expr.Value [ 1; 2 ] ]) (box (Ok [ 1; 2 ] : Result<int list, string list>)) "valid"
                  Expect.isTrue (isErrorResult (create f [ Expr.Value [ -1 ] ])) "negative item"
              }

              test "an object omits absent optional properties" {
                  let f = fixture person
                  let result = create f [ Expr.Value "ann"; nullable<int> None; Expr.Value(null, typeof<string>) ]
                  Expect.equal (okJson result) """{"name":"ann"}""" ""
              }

              test "an object includes present optional properties" {
                  let f = fixture person
                  let result = create f [ Expr.Value "ann"; nullable (Some 30); Expr.Value "a" ]
                  Expect.equal (okJson result) """{"name":"ann","age":30,"nick":"a"}""" ""
              }

              test "an object violating a property keyword is Error" {
                  let f = fixture person
                  Expect.isTrue (isErrorResult (create f [ Expr.Value "ann"; nullable (Some -1); Expr.Value(null, typeof<string>) ])) ""
              }

              test "a compilable object evaluates to the value directly" {
                  let f = fixture """{ "type": "object", "properties": { "a": { "type": "integer" } }, "required": ["a"] }"""
                  Expect.equal (jsonOf (create f [ Expr.Value 1 ])) """{"a":1}""" ""
              }

              // An exact-length-1 array compiles to its item type, so an optional one is Nullable<int>.
              test "an optional exact-length-1 array property is omitted when null and wrapped when set" {
                  let f =
                      fixture
                          """{ "type": "object", "properties": { "one": { "type": "array", "items": { "type": "integer" }, "minItems": 1, "maxItems": 1 } } }"""
                  Expect.equal (jsonOf (create f [ nullable<int> None ])) "{}" "absent"
                  Expect.equal (jsonOf (create f [ nullable (Some 4) ])) """{"one":[4]}""" "present"
              }

              test "an optional single-branch oneOf integer property is omitted when null" {
                  let f = fixture """{ "type": "object", "properties": { "n": { "oneOf": [ { "type": "integer" } ] } } }"""
                  Expect.equal (jsonOf (create f [ nullable<int> None ])) "{}" "absent"
                  Expect.equal (jsonOf (create f [ nullable (Some 2) ])) """{"n":2}""" "present"
              } ]

    let getterTests =
        testList
            "Property getters"
            [ let f = fixture person
              let keywords, properties = match f.Root with JsonObject(k, p) -> k, p | other -> failwithf "%A" other
              let instance = Expr.Value(NullableJsonValue(JsonValue.Parse """{"name":"ann","age":30}"""))
              let getter name = generatePropertyGetter f.Context f.TypeMap keywords (properties |> List.find (fst >> (=) name))

              test "a required property evaluates to its value" {
                  Expect.equal (eval (getter "name" [ instance ])) (box "ann") ""
              }

              test "a present optional property evaluates to Some" {
                  Expect.equal (eval (getter "age" [ instance ])) (box (Some 30)) ""
              }

              test "an absent optional property evaluates to None" {
                  Expect.isNull (eval (getter "nick" [ instance ])) "None is null at runtime"
              } ]

    [<Tests>]
    let tests = testList "ExprGeneratorTests" [ parseTests; createTests; getterTests ]
