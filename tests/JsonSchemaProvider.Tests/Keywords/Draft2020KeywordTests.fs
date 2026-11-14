namespace JsonSchemaProvider.Tests

// 2019-09/2020-12 keywords the provider has no encoding for. A node using one gets a plain type
// and a Result from Create/Parse. That NJsonSchema 11's Validate ignores the keywords themselves
// is pinned in KnownGaps/KnownGapTests.fs.
module Draft2020KeywordTests =
    open Xunit
    open JsonSchemaProvider

    // -- contains: plain list despite minItems, since contains has no compiled shape --

    [<Literal>]
    let containsSchema = """{ "type": "array", "items": { "type": "integer" }, "contains": { "const": 1 }, "minItems": 2 }"""
    type ContainsArray = JsonSchemaProvider<schema = containsSchema>

    [<Fact>]
    let ``contains with minItems gives a plain list, not a tuple`` () =
        let v: int list = expectOk (ContainsArray.Create [ 2; 1 ])
        expectEqual v [ 2; 1 ]

    [<Fact>]
    let ``contains still checks minItems at runtime`` () = expectError (ContainsArray.Create [ 1 ])

    // -- minContains / maxContains --

    [<Literal>]
    let minContainsSchema = """{ "type": "array", "items": { "type": "integer" }, "contains": { "const": 1 }, "minContains": 2, "maxItems": 3 }"""
    type MinContainsArray = JsonSchemaProvider<schema = minContainsSchema>

    [<Fact>]
    let ``minContains with maxItems gives a plain list and checks maxItems at runtime`` () =
        let v: int list = expectOk (MinContainsArray.Create [ 1; 1 ])
        expectEqual v [ 1; 1 ]
        expectError (MinContainsArray.Create [ 1; 1; 1; 1 ])

    [<Literal>]
    let maxContainsSchema = """{ "type": "array", "items": { "type": "integer" }, "contains": { "const": 1 }, "maxContains": 1 }"""
    type MaxContainsArray = JsonSchemaProvider<schema = maxContainsSchema>

    // -- prefixItems --

    [<Literal>]
    let prefixItemsSchema = """{ "type": "array", "items": { "type": "integer" }, "prefixItems": [ { "const": 1 } ], "minItems": 2 }"""
    type PrefixItemsArray = JsonSchemaProvider<schema = prefixItemsSchema>

    [<Fact>]
    let ``prefixItems with minItems gives a plain list`` () =
        let v: int list = expectOk (PrefixItemsArray.Create [ 1; 3 ])
        expectEqual v [ 1; 3 ]

    // -- dependentRequired / dependentSchemas / propertyNames / unevaluatedProperties --

    [<Literal>]
    let dependentRequiredSchema =
        """{ "type": "object", "properties": { "a": { "type": "string" }, "b": { "type": "string" } }, "dependentRequired": { "a": ["b"] } }"""
    type DependentRequiredObject = JsonSchemaProvider<schema = dependentRequiredSchema>

    [<Fact>]
    let ``dependentRequired: Create gives a Result and accepts a valid object`` () =
        let v = expectOk (DependentRequiredObject.Create(a = "x", b = "y"))
        expectEqual (v.a, v.b) (Some "x", Some "y")

    [<Literal>]
    let dependentSchemasSchema =
        """{ "type": "object", "properties": { "a": { "type": "string" }, "b": { "type": "string" } }, "dependentSchemas": { "a": { "required": ["b"] } } }"""
    type DependentSchemasObject = JsonSchemaProvider<schema = dependentSchemasSchema>

    [<Literal>]
    let propertyNamesSchema = """{ "type": "object", "properties": { "abc": { "type": "string" } }, "propertyNames": { "maxLength": 2 } }"""
    type PropertyNamesObject = JsonSchemaProvider<schema = propertyNamesSchema>

    [<Literal>]
    let unevaluatedPropertiesSchema =
        """{ "type": "object", "properties": { "a": { "type": "string" } }, "unevaluatedProperties": false }"""
    type UnevaluatedPropertiesObject = JsonSchemaProvider<schema = unevaluatedPropertiesSchema>
