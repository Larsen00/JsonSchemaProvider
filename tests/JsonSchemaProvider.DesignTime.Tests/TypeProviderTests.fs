namespace JsonSchemaProvider.Tests

// The provided type surface TypeProvider.run builds: members, signatures and nested types.
module TypeProviderTests =
    open System
    open System.Reflection
    open Expecto
    open ProviderImplementation.ProvidedTypes
    open JsonSchemaProvider
    open JsonSchemaProvider.DesignTime
    open JsonSchemaProvider.DesignTime.ProviderConfiguration
    open TestHelpers

    let private provideWith (flags: CompileFlags) (schemaText: string) : ProvidedTypeDefinition =
        let schema = SchemaCache.parseSchema schemaText
        TypeProvider.run schema (schemaText.GetHashCode()) (Assembly.GetExecutingAssembly()) "Test" "Root" typeof<NullableJsonValue> flags

    let private provide = provideWith defaultFlags

    let private method (t: Type) (name: string) =
        match t.GetMethod(name, BindingFlags.Public ||| BindingFlags.Static) with
        | null -> failtestf "%s has no static method %s" t.Name name
        | m -> m

    let private hasMethod (t: Type) (name: string) =
        not (isNull (t.GetMethod(name, BindingFlags.Public ||| BindingFlags.Static)))

    let private nested (t: Type) (name: string) =
        match t.GetNestedType(name, BindingFlags.Public) with
        | null ->
            let names = t.GetNestedTypes() |> Array.map (fun n -> n.Name)
            failtestf "%s has no nested type %s (has %A)" t.Name name names
        | n -> n

    let private nestedObject =
        """{ "type": "object",
             "properties": {
               "id": { "type": "integer" },
               "label": { "type": "string" },
               "inner": { "type": "object", "properties": { "x": { "type": "integer" } }, "required": ["x"] } },
             "required": ["id", "inner"] }"""

    let objectRootTests =
        testList
            "Object root"
            [ test "required properties have their plain type, optional ones an option" {
                  let root = provide nestedObject
                  Expect.equal (root.GetProperty("id").PropertyType) typeof<int> "id"
                  Expect.equal (root.GetProperty("label").PropertyType) typeof<string option> "label"
              }

              test "a nested object becomes a nested provided type used by its property" {
                  let root = provide nestedObject
                  let inner = nested root "innerObj"
                  Expect.equal (root.GetProperty("inner").PropertyType) inner "inner property type"
                  Expect.equal (inner.GetProperty("x").PropertyType) typeof<int> "x"
              }

              test "Create takes one parameter per property, optional ones marked optional" {
                  let parameters = (method (provide nestedObject) "Create").GetParameters()
                  Expect.equal (parameters |> Array.map (fun p -> p.Name)) [| "id"; "label"; "inner" |] "names"
                  Expect.equal (parameters |> Array.map (fun p -> p.IsOptional)) [| false; true; false |] "optional"
              }

              test "an optional integer parameter is Nullable<int>" {
                  let root = provide """{ "type": "object", "properties": { "n": { "type": "integer" } } }"""
                  Expect.equal ((method root "Create").GetParameters().[0].ParameterType) typeof<Nullable<int>> ""
              }

              test "Create evaluates to the provided type itself when the object is fully compilable" {
                  let root = provide nestedObject
                  Expect.equal (method root "Create").ReturnType (root :> Type) ""
              }

              test "Create evaluates to a Result when validation is needed" {
                  let root = provide """{ "type": "object", "properties": { "n": { "type": "integer", "minimum": 0 } } }"""
                  Expect.isTrue (isResultOf root (method root "Create").ReturnType) ""
              }

              test "SkipRuntimeValidation drops the Result from Create but not from Parse" {
                  let root = provideWith { defaultFlags with SkipRuntimeValidation = true } """{ "type": "object", "properties": { "n": { "type": "integer", "minimum": 0 } } }"""
                  Expect.equal (method root "Create").ReturnType (root :> Type) "Create"
                  Expect.isTrue (isResultOf root (method root "Parse").ReturnType) "Parse"
              }

              test "the root and every nested object get Parse returning a Result of themselves" {
                  let root = provide nestedObject
                  let inner = nested root "innerObj"
                  Expect.isTrue (isResultOf root (method root "Parse").ReturnType) "root"
                  Expect.isTrue (isResultOf inner (method inner "Parse").ReturnType) "inner"
              }

              test "a nested object's Create depends on its own compilability only" {
                  let root =
                      provide
                          """{ "type": "object",
                               "properties": {
                                 "plain": { "type": "object", "properties": { "a": { "type": "integer" } } },
                                 "checked": { "type": "object", "properties": { "a": { "type": "integer", "minimum": 0 } } } } }"""
                  let plain = nested root "plainObj"
                  let checkedType = nested root "checkedObj"
                  Expect.equal (method plain "Create").ReturnType plain "plain"
                  Expect.isTrue (isResultOf checkedType (method checkedType "Create").ReturnType) "checked"
                  Expect.isTrue (isResultOf root (method root "Create").ReturnType) "root inherits the nested check"
              }

              test "array items that are objects become an Item type" {
                  let root = provide """{ "type": "object", "properties": { "values": { "type": "array", "items": { "type": "object", "properties": { "a": { "type": "integer" } } } } } }"""
                  let item = nested root "valuesItem"
                  let propertyType = root.GetProperty("values").PropertyType
                  Expect.equal (propertyType.GetGenericTypeDefinition()) typedefof<_ option> "option"
                  let listType = propertyType.GetGenericArguments().[0]
                  Expect.equal (listType.GetGenericTypeDefinition()) typedefof<_ list> "list"
                  Expect.equal (listType.GetGenericArguments().[0]) item "item"
              }

              test "an array property gets a helper type with ToList" {
                  let root = provide """{ "type": "object", "properties": { "values": { "type": "array", "items": { "type": "integer" }, "minItems": 2, "maxItems": 2 } } }"""
                  let helper = nested root "valuesArray"
                  Expect.equal (method helper "ToList").ReturnType typeof<int list> "ToList"
                  Expect.equal (method helper "ToTuple").ReturnType typeof<int * int> "ToTuple"
              }

              test "nested arrays get nested helper types" {
                  let root =
                      provide
                          """{ "type": "object", "properties": { "grid": { "type": "array", "items": { "type": "array", "items": { "type": "integer" }, "maxItems": 2 } } } }"""
                  let helper = nested root "gridArray"
                  let itemHelper = nested helper "ItemArray"
                  Expect.equal (method itemHelper "ToList").ReturnType typeof<int list> ""
              } ]

    let otherRootTests =
        testList
            "Non-object roots"
            [ test "a primitive root has Create(value) and Parse" {
                  let root = provide """{ "type": "string" }"""
                  let create = method root "Create"
                  Expect.equal (create.GetParameters() |> Array.map (fun p -> p.Name, p.ParameterType)) [| "value", typeof<string> |] "Create parameters"
                  Expect.equal create.ReturnType typeof<string> "Create return type"
                  Expect.isTrue (isResultOf typeof<string> (method root "Parse").ReturnType) "Parse"
              }

              test "a validated primitive root's Create evaluates to a Result" {
                  let root = provide """{ "type": "integer", "maximum": 3 }"""
                  Expect.isTrue (isResultOf typeof<int> (method root "Create").ReturnType) ""
              }

              test "an array root carries ToList/ToTuple directly" {
                  let root = provide (intArray ", \"minItems\": 3, \"maxItems\": 3")
                  Expect.equal (method root "Create").ReturnType typeof<int * (int * int)> "Create"
                  Expect.equal (method root "ToList").ReturnType typeof<int list> "ToList"
                  Expect.equal (method root "ToTuple").ReturnType typeof<int * int * int> "ToTuple"
              }

              test "an unbounded array root has no ToTuple" {
                  Expect.isFalse (hasMethod (provide (intArray "")) "ToTuple") ""
              }

              test "a oneOf root takes a Choice" {
                  let root = provide """{ "oneOf": [ { "type": "integer" }, { "type": "string" } ] }"""
                  let parameterType = (method root "Create").GetParameters().[0].ParameterType
                  Expect.equal parameterType typeof<Choice<int, string>> ""
              }

              test "object branches of a root oneOf become Case types" {
                  let root =
                      provide
                          """{ "oneOf": [ { "type": "object", "properties": { "a": { "type": "integer" } }, "required": ["a"] }, { "type": "integer" } ] }"""
                  let case1 = root.GetNestedTypes() |> Array.tryFind (fun t -> t.Name.EndsWith "Case1")
                  Expect.isSome case1 "a Case1 type for the object branch"
              }

              test "an array of objects at the root exposes the item type" {
                  let root = provide """{ "type": "array", "items": { "type": "object", "properties": { "a": { "type": "integer" } } } }"""
                  Expect.isNonEmpty (root.GetNestedTypes()) "the item object type"
              } ]

    [<Tests>]
    let tests = testList "TypeProviderTests" [ objectRootTests; otherRootTests ]
