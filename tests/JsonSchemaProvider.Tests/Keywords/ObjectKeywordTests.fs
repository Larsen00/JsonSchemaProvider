namespace JsonSchemaProvider.Tests

// Object-nesting coverage: properties/required/optional round-tripping, object's own keywords
// (minProperties/maxProperties/additionalProperties/patternProperties), and every keyword from
// NumberKeywordTests/StringKeywordTests/ArrayKeywordTests tested again nested in a property
// instead of at the schema root, to prove the nesting itself doesn't break the keyword.
module ObjectKeywordTests =
    open Xunit
    open JsonSchemaProvider

    [<Literal>]
    let flatSchema =
        """{ "type": "object", "properties": { "X": {"type": "string"}, "Y": {"type": "string"}, "Z": {"type": "integer"} } }"""
    type Flat = JsonSchemaProvider<schema=flatSchema>

    [<Fact>]
    let ``valid record should be parsed`` () =
        let flat = expectOk (Flat.Parse("""{"X": "x", "Z": 1}"""))
        expectEqual flat.X (Some "x")
        expectEqual flat.Y None
        expectEqual flat.Z (Some 1)

    [<Fact>]
    let ``create method should return record`` () =
        let flat = Flat.Create(X = "x", Z = 1)
        expectEqual flat.X (Some "x")
        expectEqual flat.Y None
        expectEqual flat.Z (Some 1)

    // flatSchema's optional Z:integer covers the JsonInteger branch; this covers the JsonBoolean/
    // JsonNumber optional-property branches, which nothing else in this file exercises.
    [<Literal>]
    let optionalPrimitivesSchema =
        """{ "type": "object", "properties": { "flag": {"type": "boolean"}, "amount": {"type": "number"} } }"""
    type OptionalPrimitives = JsonSchemaProvider<schema=optionalPrimitivesSchema>

    [<Fact>]
    let ``optional boolean and number properties round-trip when present`` () =
        let v = OptionalPrimitives.Create(flag = true, amount = 1.5)
        expectEqual v.flag (Some true)
        expectEqual v.amount (Some 1.5)

    [<Fact>]
    let ``optional boolean and number properties give None when absent`` () =
        let v = OptionalPrimitives.Create()
        expectEqual v.flag None
        expectEqual v.amount None

    [<Literal>]
    let requiredPropertiesSchema =
        """{ "type": "object", "properties": { "X": {"type": "string"}, "Y": {"type": "string"}, "Z": {"type": "integer"} }, "required": ["X", "Y", "Z"] }"""
    type RequiredProperties = JsonSchemaProvider<schema=requiredPropertiesSchema>

    [<Fact>]
    let ``required properties should not be parsed into Option`` () =
        let v = expectOk (RequiredProperties.Parse("""{"X": "x", "Y": "y", "Z": 1}"""))
        expectEqual v.X "x"
        expectEqual v.Y "y"
        expectEqual v.Z 1

    [<Literal>]
    let cityPosition =
        """
        {
          "type": "object",
          "properties": {
            "city": {"type": "string"},
            "globalPosition": {
              "type": "object",
              "properties": { "lat": {"type": "number"}, "lon": {"type": "number"} },
              "required": ["lat", "lon"]
            }
          },
          "required": ["city", "globalPosition"]
        }"""
    type CityPosition = JsonSchemaProvider<schema=cityPosition>

    [<Fact>]
    let ``value from nested objects should be created`` () =
        let globalPosition = CityPosition.globalPositionObj.Create(lat = 52.520007, lon = 13.404954)
        let created = CityPosition.Create(city = "Berlin", globalPosition = globalPosition)
        expectEqual created.globalPosition.lat 52.520007

    // -- number keyword nested in a property (root-level versions: NumberKeywordTests.fs) --

    [<Literal>]
    let exclusiveRangeSchema =
        """{ "type": "object", "properties": { "value": { "type": "number", "exclusiveMinimum": 0, "exclusiveMaximum": 10 } }, "required": ["value"] }"""
    type ExclusiveRange = JsonSchemaProvider<schema=exclusiveRangeSchema>

    [<Fact>]
    let ``object property: exclusiveMinimum rejects the boundary value itself`` () =
        expectError (ExclusiveRange.Create(value = 0.0)) // 0 is excluded by exclusiveMinimum

    [<Fact>]
    let ``object property: exclusiveMaximum rejects the boundary value itself`` () =
        expectError (ExclusiveRange.Create(value = 10.0)) // 10 is excluded by exclusiveMaximum

    [<Fact>]
    let ``object property: a value strictly inside an exclusive range is accepted`` () =
        let result = expectOk (ExclusiveRange.Create(value = 5.0))
        expectEqual result.value 5.0

    [<Literal>]
    let multipleOfSchema =
        """{ "type": "object", "properties": { "value": { "type": "integer", "multipleOf": 5 } }, "required": ["value"] }"""
    type MultipleOf = JsonSchemaProvider<schema=multipleOfSchema>

    [<Fact>]
    let ``object property: multipleOf rejects a non-multiple value`` () =
        expectError (MultipleOf.Create(value = 7)) // 7 is not a multiple of 5

    [<Fact>]
    let ``object property: multipleOf accepts a genuine multiple`` () =
        let result = expectOk (MultipleOf.Create(value = 10))
        expectEqual result.value 10

    // -- string keyword nested in a property (root-level versions: StringKeywordTests.fs) --

    [<Literal>]
    let patternSchema =
        """{ "type": "object", "properties": { "X": { "type": "string", "pattern": "^[a-z]+$" } } }"""
    type PatternSchema = JsonSchemaProvider<schema=patternSchema>

    [<Fact>]
    let ``object property: validation error should be detected by Create`` () =
        expectError (PatternSchema.Create(X = "a1")) // Create should return Error for invalid pattern

    [<Fact>]
    let ``object property: validation error should be detected by Parse`` () =
        expectError (PatternSchema.Parse("""{"X": "a1"}""")) // Parse throws validation exception

    [<Literal>]
    let stringLengthSchema =
        """{ "type": "object", "properties": { "value": { "type": "string", "minLength": 3, "maxLength": 5 } }, "required": ["value"] }"""
    type StringLength = JsonSchemaProvider<schema=stringLengthSchema>

    [<Fact>]
    let ``object property: minLength rejects a too-short string`` () =
        expectError (StringLength.Create(value = "ab")) // "ab" is below minLength

    [<Fact>]
    let ``object property: maxLength rejects a too-long string`` () =
        expectError (StringLength.Create(value = "abcdef")) // "abcdef" is above maxLength

    [<Fact>]
    let ``object property: a string within [minLength, maxLength] is accepted`` () =
        let result = expectOk (StringLength.Create(value = "abc"))
        expectEqual result.value "abc"

    [<Fact>]
    let ``object property: maxLength accepts a string at the limit`` () =
        let result = expectOk (StringLength.Create(value = "abcde"))
        expectEqual result.value "abcde"

    [<Literal>]
    let emailFormatSchema =
        """{ "type": "object", "properties": { "value": { "type": "string", "format": "email" } }, "required": ["value"] }"""
    type EmailFormat = JsonSchemaProvider<schema=emailFormatSchema>

    [<Fact>]
    let ``object property: format=email rejects a non-email string`` () =
        expectError (EmailFormat.Create(value = "not-an-email")) // does not match the email format

    [<Fact>]
    let ``object property: format=email accepts a genuine email string`` () =
        let result = expectOk (EmailFormat.Create(value = "a@b.com"))
        expectEqual result.value "a@b.com"

    // -- array keyword nested in a property (root-level versions: ArrayKeywordTests.fs) --

    [<Literal>]
    let stringArrayMin2Schema =
        """{ "type": "object", "properties": { "tags": { "type": "array", "items": {"type": "string"}, "minItems": 2 } }, "required": ["tags"] }"""
    type PlainStringArray = JsonSchemaProvider<schema = stringArrayMin2Schema, ignoreSpecificKeywords = true>
    type StringArrayMin2 = JsonSchemaProvider<schema = stringArrayMin2Schema>

    [<Fact>]
    let ``object property: minItems schema without compileMinItems flag yields plain list`` () =
        let v = expectOk (PlainStringArray.Parse("""{"tags": ["a", "b", "c"]}"""))
        expectEqual v.tags [ "a"; "b"; "c" ]

    [<Fact>]
    let ``object property: minItems=2 Create produces correct tuple`` () =
        let (h1, (h2, rest)) = (StringArrayMin2.Create(tags = ("a", ("b", [ "c"; "d" ])))).tags
        expectEqual h1 "a"
        expectEqual h2 "b"
        expectEqual rest [ "c"; "d" ]

    [<Fact>]
    let ``object property: minItems=2 Parse produces correct tuple`` () =
        let v = expectOk (StringArrayMin2.Parse("""{"tags": ["x", "y", "z"]}"""))
        let (h1, (h2, rest)) = v.tags
        expectEqual h1 "x"
        expectEqual h2 "y"
        expectEqual rest [ "z" ]

    [<Fact>]
    let ``object property: minItems=2 with exactly 2 elements gives empty rest`` () =
        let v = expectOk (StringArrayMin2.Parse("""{"tags": ["a", "b"]}"""))
        let (_, (_, rest)) = v.tags
        expectEqual rest []

    [<Literal>]
    let intArrayMin1Schema =
        """{ "type": "object", "properties": { "values": { "type": "array", "items": {"type": "integer"}, "minItems": 1 } }, "required": ["values"] }"""
    type IntArrayMin1 = JsonSchemaProvider<schema = intArrayMin1Schema>

    [<Fact>]
    let ``object property: minItems=1 Create produces correct pair`` () =
        let (head, rest) = (IntArrayMin1.Create(values = (42, [ 1; 2; 3 ]))).values
        expectEqual head 42
        expectEqual rest [ 1; 2; 3 ]

    [<Fact>]
    let ``object property: minItems=1 Parse produces correct pair`` () =
        let v = expectOk (IntArrayMin1.Parse("""{"values": [10, 20, 30]}"""))
        let (head, rest) = v.values
        expectEqual head 10
        expectEqual rest [ 20; 30 ]

    [<Fact>]
    let ``object property: minItems=1 with exactly 1 element gives empty rest`` () =
        let v = expectOk (IntArrayMin1.Parse("""{"values": [99]}"""))
        let (head, rest) = v.values
        expectEqual head 99
        expectEqual rest []

    [<Literal>]
    let intArrayMin1Max2Schema =
        """{ "type": "object", "properties": { "values": { "type": "array", "items": {"type": "integer"}, "minItems": 1, "maxItems": 2 } }, "required": ["values"] }"""
    type IntArrayMin1Max2 = JsonSchemaProvider<schema = intArrayMin1Max2Schema>

    [<Fact>]
    let ``object property: minItems=1, maxItems=2: exactly minItems elements gives None for the rest`` () =
        let (head, rest) = (IntArrayMin1Max2.Create(values = (42, None))).values
        expectEqual head 42
        expectEqual rest None

    [<Fact>]
    let ``object property: minItems=1, maxItems=2: maxItems elements gives Some for the rest`` () =
        let (head, rest) = (IntArrayMin1Max2.Create(values = (42, Some 7))).values
        expectEqual head 42
        expectEqual rest (Some 7)

    [<Fact>]
    let ``object property: minItems=1, maxItems=2: Parse with maxItems elements gives Some for the rest`` () =
        let v = expectOk (IntArrayMin1Max2.Parse("""{"values": [1, 2]}"""))
        expectEqual v.values (1, Some 2)

    [<Literal>]
    let intArrayMax2Schema =
        """{ "type": "object", "properties": { "values": { "type": "array", "items": {"type": "integer"}, "maxItems": 2 } }, "required": ["values"] }"""
    type IntArrayMax2 = JsonSchemaProvider<schema = intArrayMax2Schema>

    [<Fact>]
    let ``object property: maxItems=2, no minItems: no elements gives None`` () =
        expectEqual (IntArrayMax2.Create(values = None)).values None

    [<Fact>]
    let ``object property: maxItems=2, no minItems: one element gives Some(head, None)`` () =
        expectEqual (IntArrayMax2.Create(values = Some(1, None))).values (Some(1, None))

    [<Fact>]
    let ``object property: maxItems=2, no minItems: at the limit gives Some(head, Some tail)`` () =
        expectEqual (IntArrayMax2.Create(values = Some(1, Some 2))).values (Some(1, Some 2))

    [<Fact>]
    let ``object property: maxItems=2, no minItems: Parse at the limit round-trips both elements`` () =
        let v = expectOk (IntArrayMax2.Parse("""{"values": [5, 6]}"""))
        expectEqual v.values (Some(5, Some 6))

    [<Literal>]
    let intArrayMax1Schema =
        """{ "type": "object", "properties": { "values": { "type": "array", "items": {"type": "integer"}, "maxItems": 1 } }, "required": ["values"] }"""
    type IntArrayMax1 = JsonSchemaProvider<schema = intArrayMax1Schema>

    [<Fact>]
    let ``object property: maxItems=1: no elements gives None`` () =
        expectEqual (IntArrayMax1.Create(values = None)).values None

    [<Fact>]
    let ``object property: maxItems=1: one element gives Some`` () =
        expectEqual (IntArrayMax1.Create(values = Some 42)).values (Some 42)

    [<Fact>]
    let ``object property: maxItems=1: Parse with one element round-trips it`` () =
        let v = expectOk (IntArrayMax1.Parse("""{"values": [7]}"""))
        expectEqual v.values (Some 7)

    // See ArrayKeywordTests.fs's matching block: IntArrayExact2WithFlag is declared identically
    // to IntArrayExact2 (neither passes ignoreSpecificKeywords), inherited as-is.
    [<Literal>]
    let intArrayExact2Schema =
        """{ "type": "object", "properties": { "values": { "type": "array", "items": {"type": "integer"}, "minItems": 2, "maxItems": 2 } }, "required": ["values"] }"""
    type IntArrayExact2 = JsonSchemaProvider<schema = intArrayExact2Schema>
    type IntArrayExact2WithFlag = JsonSchemaProvider<schema = intArrayExact2Schema>

    [<Fact>]
    let ``object property: minItems=maxItems=2, no flag: Create produces the exact tuple`` () =
        expectEqual (IntArrayExact2.Create(values = (1, 2))).values (1, 2)

    [<Fact>]
    let ``object property: minItems=maxItems=2, no flag: Parse produces the exact tuple`` () =
        let v = expectOk (IntArrayExact2.Parse("""{"values": [3, 4]}"""))
        expectEqual v.values (3, 4)

    [<Fact>]
    let ``object property: minItems=maxItems=2, second declaration: Create produces the same exact tuple`` () =
        expectEqual (IntArrayExact2WithFlag.Create(values = (5, 6))).values (5, 6)

    [<Fact>]
    let ``object property: minItems=maxItems=2, second declaration: Parse produces the same exact tuple`` () =
        let v = expectOk (IntArrayExact2WithFlag.Parse("""{"values": [7, 8]}"""))
        expectEqual v.values (7, 8)

    // -- ToList/ToTuple on array properties: helpers on a "<PropertyName>Array" type --

    [<Fact>]
    let ``object property: minItems=maxItems=2: valuesArray.ToList flattens (h, tail)`` () =
        expectEqual (IntArrayExact2.valuesArray.ToList((1, 2))) [ 1; 2 ]

    [<Fact>]
    let ``object property: minItems=maxItems=2: valuesArray.ToTuple rebuilds the flat tuple`` () =
        expectEqual (IntArrayExact2.valuesArray.ToTuple((1, 2))) (1, 2)

    [<Fact>]
    let ``object property: minItems=2, no maxItems: tagsArray.ToList flattens the prefix plus the open tail`` () =
        expectEqual (StringArrayMin2.tagsArray.ToList(("a", ("b", [ "c"; "d" ])))) [ "a"; "b"; "c"; "d" ]

    // -- ToList/ToTuple inside nested classes: helpers sit on the nested type, e.g. metaObj.valuesArray --

    [<Literal>]
    let nestedClassArraySchema =
        """{
          "type": "object",
          "properties": {
            "meta": {
              "type": "object",
              "properties": {
                "values": { "type": "array", "items": {"type": "integer"}, "minItems": 3, "maxItems": 3 },
                "inner": {
                  "type": "object",
                  "properties": { "tags": { "type": "array", "items": {"type": "string"}, "maxItems": 2 } },
                  "required": ["tags"]
                }
              },
              "required": ["values", "inner"]
            },
            "rows": {
              "type": "array",
              "items": {
                "type": "object",
                "properties": { "cells": { "type": "array", "items": {"type": "integer"}, "minItems": 1 } },
                "required": ["cells"]
              }
            }
          },
          "required": ["meta", "rows"]
        }"""
    type NestedClassArray = JsonSchemaProvider<schema = nestedClassArraySchema>

    [<Fact>]
    let ``nested class: minItems=maxItems=3: metaObj.valuesArray.ToTuple rebuilds the flat tuple`` () =
        expectEqual (NestedClassArray.metaObj.valuesArray.ToTuple((1, (2, 3)))) (1, 2, 3)

    [<Fact>]
    let ``nested class: Parse then metaObj.valuesArray.ToList flattens the parsed property`` () =
        let v =
            expectOk (NestedClassArray.Parse("""{"meta": {"values": [4, 5, 6], "inner": {"tags": ["x"]}}, "rows": []}"""))
        expectEqual (NestedClassArray.metaObj.valuesArray.ToList(v.meta.values)) [ 4; 5; 6 ]

    [<Fact>]
    let ``two nested classes deep: metaObj.innerObj.tagsArray.ToList flattens the maxItems chain`` () =
        expectEqual (NestedClassArray.metaObj.innerObj.tagsArray.ToList(Some("a", Some "b"))) [ "a"; "b" ]

    [<Fact>]
    let ``array item class: rowsItem.cellsArray.ToList flattens each row's (h, tail list)`` () =
        let v =
            expectOk (NestedClassArray.Parse("""{"meta": {"values": [1, 2, 3], "inner": {"tags": []}}, "rows": [{"cells": [1, 2]}, {"cells": [3]}]}"""))
        let flattened = v.rows |> List.map (fun row -> NestedClassArray.rowsItem.cellsArray.ToList(row.cells))
        expectEqual flattened [ [ 1; 2 ]; [ 3 ] ]

    // -- Property-level array-of-arrays / array-of-objects helper cases --

    [<Literal>]
    let propertyArrayOfArraysSchema =
        """{
          "type": "object",
          "properties": {
            "values": { "type": "array", "items": { "type": "array", "items": {"type": "integer"}, "minItems": 3, "maxItems": 3 } },
            "points": { "type": "array", "minItems": 2, "maxItems": 2, "items": { "type": "object", "properties": { "x": {"type": "integer"} }, "required": ["x"] } }
          },
          "required": ["values", "points"]
        }"""
    type PropertyArrayOfArrays = JsonSchemaProvider<schema = propertyArrayOfArraysSchema>

    [<Fact>]
    let ``object property: array of exact-3 arrays: valuesArray.ItemArray.ToTuple flattens each inner array`` () =
        let v =
            expectOk (PropertyArrayOfArrays.Parse("""{"values": [[1, 2, 3], [4, 5, 6]], "points": [{"x": 1}, {"x": 2}]}"""))
        expectEqual (v.values |> List.map PropertyArrayOfArrays.valuesArray.ItemArray.ToTuple) [ (1, 2, 3); (4, 5, 6) ]

    [<Fact>]
    let ``object property: exact-2 array of objects: pointsArray.ToTuple returns provided-class items`` () =
        let v =
            expectOk (PropertyArrayOfArrays.Parse("""{"values": [], "points": [{"x": 1}, {"x": 2}]}"""))
        let (first, second) = PropertyArrayOfArrays.pointsArray.ToTuple(v.points)
        expectEqual (first.x, second.x) (1, 2)

    // Optional array property: the property is `shape option`, so ToList is applied with Option.map.
    [<Literal>]
    let optionalMin2ArraySchema =
        """{ "type": "object", "properties": { "tags": { "type": "array", "items": {"type": "string"}, "minItems": 2 } } }"""
    type OptionalMin2Array = JsonSchemaProvider<schema = optionalMin2ArraySchema>

    [<Fact>]
    let ``optional array property, present: Option.map tagsArray.ToList gives Some list`` () =
        let v = expectOk (OptionalMin2Array.Parse("""{"tags": ["a", "b", "c"]}"""))
        expectEqual (v.tags |> Option.map OptionalMin2Array.tagsArray.ToList) (Some [ "a"; "b"; "c" ])

    [<Fact>]
    let ``optional array property, absent: Option.map tagsArray.ToList gives None`` () =
        let v = expectOk (OptionalMin2Array.Parse("""{}"""))
        expectEqual (v.tags |> Option.map OptionalMin2Array.tagsArray.ToList) None

    [<Literal>]
    let uniqueItemsSchema =
        """{ "type": "object", "properties": { "values": { "type": "array", "items": { "type": "integer" }, "uniqueItems": true } }, "required": ["values"] }"""
    type UniqueItemsArray = JsonSchemaProvider<schema=uniqueItemsSchema>

    [<Fact>]
    let ``object property: uniqueItems rejects an array with a duplicate`` () =
        expectError (UniqueItemsArray.Create(values = [ 1; 2; 2 ])) // [1;2;2] has a duplicate

    [<Fact>]
    let ``object property: uniqueItems accepts an array with no duplicates`` () =
        let result = expectOk (UniqueItemsArray.Create(values = [ 1; 2; 3 ]))
        expectEqual result.values [ 1; 2; 3 ]

    [<Literal>]
    let maxItemsSchema =
        """{ "type": "object", "properties": { "values": { "type": "array", "items": { "type": "integer" }, "maxItems": 2 } }, "required": ["values"] }"""
    type MaxItemsArray = JsonSchemaProvider<schema=maxItemsSchema>

    [<Fact>]
    let ``object property: maxItems accepts an array at the limit`` () =
        expectEqual (MaxItemsArray.Create(values = Some(1, Some 2))).values (Some(1, Some 2))

    [<Fact>]
    let ``object property: maxItems accepts an array within the limit`` () =
        expectEqual (MaxItemsArray.Create(values = Some(1, None))).values (Some(1, None))

    [<Literal>]
    let minItemsRuntimeSchema =
        """{ "type": "object", "properties": { "tags": { "type": "array", "items": { "type": "string" }, "minItems": 2 } }, "required": ["tags"] }"""
    type MinItemsRuntime = JsonSchemaProvider<schema=minItemsRuntimeSchema, ignoreSpecificKeywords=true>

    [<Fact>]
    let ``object property: minItems is enforced at runtime by Create, not just as a compile-time tuple shape`` () =
        expectError (MinItemsRuntime.Create(tags = [ "a" ])) // 1 element is below minItems=2

    [<Fact>]
    let ``object property: minItems is satisfied at runtime when there are enough elements`` () =
        let result = expectOk (MinItemsRuntime.Create(tags = [ "a"; "b" ]))
        expectEqual result.tags [ "a"; "b" ]

    [<Literal>]
    let arrayItemConstraintSchema =
        """{ "type": "object", "properties": { "values": { "type": "array", "items": { "type": "integer", "minimum": 0 } } }, "required": ["values"] }"""
    type ArrayItemConstraint = JsonSchemaProvider<schema=arrayItemConstraintSchema>

    [<Fact>]
    let ``object property: an item schema's own constraint is enforced against every element`` () =
        expectError (ArrayItemConstraint.Create(values = [ 1; -1; 3 ])) // -1 violates the item schema's minimum

    [<Fact>]
    let ``object property: an array whose every element satisfies the item schema is accepted`` () =
        let result = expectOk (ArrayItemConstraint.Create(values = [ 1; 2; 3 ]))
        expectEqual result.values [ 1; 2; 3 ]

    // -- object's own keywords: minProperties / maxProperties / additionalProperties / patternProperties --

    [<Literal>]
    let minPropertiesSchema =
        """{ "type": "object", "properties": { "a": {"type": "integer"}, "b": {"type": "integer"}, "c": {"type": "integer"} }, "minProperties": 2 }"""
    type MinPropertiesObject = JsonSchemaProvider<schema=minPropertiesSchema>

    [<Fact>]
    let ``minProperties rejects an object built with too few properties set`` () =
        expectError (MinPropertiesObject.Create(a = 1)) // only 1 property set is below minProperties=2

    [<Fact>]
    let ``minProperties accepts an object with enough properties set`` () =
        let result = expectOk (MinPropertiesObject.Create(a = 1, b = 2))
        expectEqual (result.a, result.b) (Some 1, Some 2)

    [<Literal>]
    let maxPropertiesSchema =
        """{ "type": "object", "properties": { "a": {"type": "integer"}, "b": {"type": "integer"}, "c": {"type": "integer"} }, "maxProperties": 2 }"""
    type MaxPropertiesObject = JsonSchemaProvider<schema=maxPropertiesSchema>

    [<Fact>]
    let ``maxProperties rejects an object built with too many properties set`` () =
        expectError (MaxPropertiesObject.Create(a = 1, b = 2, c = 3)) // 3 properties set exceeds maxProperties=2

    [<Fact>]
    let ``maxProperties accepts an object within the limit`` () =
        let result = expectOk (MaxPropertiesObject.Create(a = 1, b = 2))
        expectEqual (result.a, result.b) (Some 1, Some 2)

    // additionalProperties/patternProperties can only be violated through Parse - Create's
    // parameter list is fixed by the schema's own declared properties.
    [<Literal>]
    let additionalPropertiesSchema =
        """{ "type": "object", "properties": { "a": {"type": "integer"} }, "additionalProperties": false }"""
    type AdditionalPropertiesObject = JsonSchemaProvider<schema=additionalPropertiesSchema>

    [<Fact>]
    let ``additionalProperties=false rejects an undeclared property via Parse`` () =
        expectError (AdditionalPropertiesObject.Parse("""{"a": 1, "extra": true}""")) // "extra" isn't declared

    [<Fact>]
    let ``additionalProperties=false accepts an object with only declared properties via Parse`` () =
        let result = expectOk (AdditionalPropertiesObject.Parse("""{"a": 1}"""))
        expectEqual result.a (Some 1)

    [<Literal>]
    let patternPropertiesSchema =
        """{ "type": "object", "patternProperties": { "^S_": {"type": "string"} }, "additionalProperties": false }"""
    type PatternPropertiesObject = JsonSchemaProvider<schema=patternPropertiesSchema>

    [<Fact>]
    let ``patternProperties rejects a property name that matches neither the pattern nor additionalProperties`` () =
        expectError (PatternPropertiesObject.Parse("""{"S_x": "ok", "other": 1}""")) // "other" doesn't match S_ or additionalProperties

    [<Fact>]
    let ``patternProperties accepts a property name matching the pattern`` () =
        expectOk (PatternPropertiesObject.Parse("""{"S_x": "ok"}""")) |> ignore

    // An exact-length-1 int array compiles to int, so the optional parameter is Nullable<int>.
    [<Literal>]
    let optionalExact1Schema =
        """{ "type": "object", "properties": { "one": { "type": "array", "items": {"type": "integer"}, "minItems": 1, "maxItems": 1 } } }"""
    type OptionalExact1 = JsonSchemaProvider<schema = optionalExact1Schema>

    [<Fact>]
    let ``optional exact-length-1 array property: omitted argument leaves the property out`` () =
        expectEqual (OptionalExact1.Create()).one None

    [<Fact>]
    let ``optional exact-length-1 array property: given argument is stored as a one-element array`` () =
        let created = OptionalExact1.Create(one = 4)
        expectEqual created.one (Some 4)
        expectEqual (created.ToString().Replace(" ", "").Replace("\n", "").Replace("\r", "")) """{"one":[4]}"""
