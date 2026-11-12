namespace JsonSchemaProvider.Tests

// 2019-09/2020-12 keywords the provider has no encoding for. A node using one gets a plain type
// and a Result from Create/Parse. The skipped facts are what the spec requires: NJsonSchema 11's
// Validate ignores these keywords, so the runtime fallback currently accepts the invalid instance.
module Draft2020KeywordTests =
    open Xunit
    open JsonSchemaProvider

    [<Literal>]
    let private ignored = "NJsonSchema 11 Validate ignores this keyword"

    // -- contains: plain list despite minItems, since contains has no compiled shape --

    [<Literal>]
    let containsSchema = """{ "type": "array", "items": { "type": "integer" }, "contains": { "const": 1 }, "minItems": 2 }"""
    type ContainsArray = JsonSchemaProvider<schema = containsSchema, allowUnboundedIntegers=true>

    [<Fact>]
    let ``contains with minItems gives a plain list, not a tuple`` () =
        let v: int list = expectOk (ContainsArray.Create [ 2; 1 ])
        expectEqual v [ 2; 1 ]

    [<Fact>]
    let ``contains still checks minItems at runtime`` () = expectError (ContainsArray.Create [ 1 ])

    [<Fact>]
    let ``contains: an array without a match is wrongly accepted`` () = expectOk (ContainsArray.Create [ 2; 3 ]) |> ignore

    [<Fact(Skip = ignored)>]
    let ``contains: Create rejects an array without a match`` () = expectError (ContainsArray.Create [ 2; 3 ])

    [<Fact(Skip = ignored)>]
    let ``contains: Parse rejects an array without a match`` () = expectError (ContainsArray.Parse "[2, 3]")

    // -- minContains / maxContains --

    [<Literal>]
    let minContainsSchema = """{ "type": "array", "items": { "type": "integer" }, "contains": { "const": 1 }, "minContains": 2, "maxItems": 3 }"""
    type MinContainsArray = JsonSchemaProvider<schema = minContainsSchema, allowUnboundedIntegers=true>

    [<Fact>]
    let ``minContains with maxItems gives a plain list and checks maxItems at runtime`` () =
        let v: int list = expectOk (MinContainsArray.Create [ 1; 1 ])
        expectEqual v [ 1; 1 ]
        expectError (MinContainsArray.Create [ 1; 1; 1; 1 ])

    [<Fact(Skip = ignored)>]
    let ``minContains: Create rejects too few matches`` () = expectError (MinContainsArray.Create [ 1; 2 ])

    [<Literal>]
    let maxContainsSchema = """{ "type": "array", "items": { "type": "integer" }, "contains": { "const": 1 }, "maxContains": 1 }"""
    type MaxContainsArray = JsonSchemaProvider<schema = maxContainsSchema, allowUnboundedIntegers=true>

    [<Fact(Skip = ignored)>]
    let ``maxContains: Create rejects too many matches`` () = expectError (MaxContainsArray.Create [ 1; 1 ])

    // -- prefixItems --

    [<Literal>]
    let prefixItemsSchema = """{ "type": "array", "items": { "type": "integer" }, "prefixItems": [ { "const": 1 } ], "minItems": 2 }"""
    type PrefixItemsArray = JsonSchemaProvider<schema = prefixItemsSchema, allowUnboundedIntegers=true>

    [<Fact>]
    let ``prefixItems with minItems gives a plain list`` () =
        let v: int list = expectOk (PrefixItemsArray.Create [ 1; 3 ])
        expectEqual v [ 1; 3 ]

    [<Fact(Skip = ignored)>]
    let ``prefixItems: Create rejects a wrong first element`` () = expectError (PrefixItemsArray.Create [ 2; 3 ])

    // -- dependentRequired / dependentSchemas / propertyNames / unevaluatedProperties --

    [<Literal>]
    let dependentRequiredSchema =
        """{ "type": "object", "properties": { "a": { "type": "string" }, "b": { "type": "string" } }, "dependentRequired": { "a": ["b"] } }"""
    type DependentRequiredObject = JsonSchemaProvider<schema = dependentRequiredSchema>

    [<Fact>]
    let ``dependentRequired: Create gives a Result and accepts a valid object`` () =
        let v = expectOk (DependentRequiredObject.Create(a = "x", b = "y"))
        expectEqual (v.a, v.b) (Some "x", Some "y")

    [<Fact(Skip = ignored)>]
    let ``dependentRequired: Create rejects a without b`` () = expectError (DependentRequiredObject.Create(a = "x"))

    [<Literal>]
    let dependentSchemasSchema =
        """{ "type": "object", "properties": { "a": { "type": "string" }, "b": { "type": "string" } }, "dependentSchemas": { "a": { "required": ["b"] } } }"""
    type DependentSchemasObject = JsonSchemaProvider<schema = dependentSchemasSchema>

    [<Fact(Skip = ignored)>]
    let ``dependentSchemas: Create rejects a without b`` () = expectError (DependentSchemasObject.Create(a = "x"))

    [<Literal>]
    let propertyNamesSchema = """{ "type": "object", "properties": { "abc": { "type": "string" } }, "propertyNames": { "maxLength": 2 } }"""
    type PropertyNamesObject = JsonSchemaProvider<schema = propertyNamesSchema>

    [<Fact>]
    let ``propertyNames: an object breaking it is wrongly accepted`` () =
        expectOk (PropertyNamesObject.Create(abc = "x")) |> ignore

    [<Fact(Skip = ignored)>]
    let ``propertyNames: Create rejects a too long property name`` () = expectError (PropertyNamesObject.Create(abc = "x"))

    [<Literal>]
    let unevaluatedPropertiesSchema =
        """{ "type": "object", "properties": { "a": { "type": "string" } }, "unevaluatedProperties": false }"""
    type UnevaluatedPropertiesObject = JsonSchemaProvider<schema = unevaluatedPropertiesSchema>

    [<Fact(Skip = ignored)>]
    let ``unevaluatedProperties: Parse rejects an extra property`` () =
        expectError (UnevaluatedPropertiesObject.Parse """{ "a": "x", "z": 1 }""")
