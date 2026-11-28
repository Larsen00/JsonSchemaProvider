namespace RefinedJsonSchemaProvider.Tests

// Schemas pulled from an existing JSON-Schema code generator's own test fixtures - a prior tool
// this thesis compares the type provider against. Checks whether real schemas that tool already
// handled also work through RefinedJsonSchemaProvider<schemaFile=...>. A design-time failure aborts
// compiling this whole file, so an unsupported schema has its type and test commented out below
// with a one-line reason; uncomment once the provider gains the feature it's blocked on.
module OldCodeGenerationTests =
    open Xunit
    open RefinedJsonSchemaProvider

    [<Literal>]
    let booleanPath = __SOURCE_DIRECTORY__ + "/../schemas/OldCodeGenerationTests/01-boolean.json"
    type Boolean = RefinedJsonSchemaProvider<schemaFile=booleanPath>

    [<Fact>]
    let ``boolean root loaded from file Create builds the value`` () =
        expectEqual (Boolean.Create true) true

    [<Literal>]
    let stringPath = __SOURCE_DIRECTORY__ + "/../schemas/OldCodeGenerationTests/02-string.json"
    type String = RefinedJsonSchemaProvider<schemaFile=stringPath>

    [<Fact>]
    let ``string root loaded from file Create builds the value`` () =
        expectEqual (expectOk (String.Create "hello")) "hello"

    // A string root always gives a Result (the string could be null); const adds nothing to that.
    [<Literal>]
    let stringConstPath = __SOURCE_DIRECTORY__ + "/../schemas/OldCodeGenerationTests/03-string-const.json"
    type StringConst = RefinedJsonSchemaProvider<schemaFile=stringConstPath>

    [<Fact>]
    let ``string const root loaded from file Create builds the value`` () =
        let value = expectOk (StringConst.Create "Brian Sanderson")
        expectEqual value "Brian Sanderson"

    // const isn't enforced yet (pinned in KnownGaps/KnownGapTests.fs). Workaround for schema
    // authors: const x means the same as enum [x], and enum is enforced.
    [<Literal>]
    let stringSingleEnumSchema = """{ "type": "string", "enum": [ "Brian Sanderson" ] }"""
    type StringSingleEnum = RefinedJsonSchemaProvider<schema=stringSingleEnumSchema>

    [<Fact>]
    let ``const rewritten as a one-value enum: Create rejects a non-matching value`` () =
        expectError (StringSingleEnum.Create "Robert Jordan")

    [<Literal>]
    let stringEnumPath = __SOURCE_DIRECTORY__ + "/../schemas/OldCodeGenerationTests/04-string-enum.json"
    type StringEnum = RefinedJsonSchemaProvider<schemaFile=stringEnumPath>

    [<Fact>]
    let ``string enum root loaded from file Create builds the value`` () =
        let value = expectOk (StringEnum.Create "Robert Jordan")
        expectEqual value "Robert Jordan"

    [<Fact>]
    let ``string enum root loaded from file Create rejects a non-member value`` () =
        expectError (StringEnum.Create "Someone Else") // outside the enum

    [<Literal>]
    let integerPath = __SOURCE_DIRECTORY__ + "/../schemas/OldCodeGenerationTests/05-integer.json"
    type Integer = RefinedJsonSchemaProvider<schemaFile=integerPath>

    [<Fact>]
    let ``integer root loaded from file Create builds the value`` () =
        expectEqual (Integer.Create 42) 42

    [<Literal>]
    let numberPath = __SOURCE_DIRECTORY__ + "/../schemas/OldCodeGenerationTests/06-number.json"
    type Number = RefinedJsonSchemaProvider<schemaFile=numberPath>
    type UncheckedNumber = RefinedJsonSchemaProvider<schemaFile=numberPath, allowNonFiniteNumbers=true>

    [<Fact>]
    let ``number root loaded from file Create builds the value`` () =
        expectEqual (UncheckedNumber.Create 3.14) 3.14

    [<Fact>]
    let ``number root loaded from file Create gives Ok by default`` () =
        expectEqual (expectOk (Number.Create 3.14)) 3.14

    // "type": "null" isn't supported - design-time throws (parseObjectType has no Null case).
    // The failure is pinned in the design-time project's KnownGaps/KnownGapTests.fs.
    // [<Literal>]
    // let nullPath = __SOURCE_DIRECTORY__ + "/../schemas/OldCodeGenerationTests/07-null.json"
    // type Null = RefinedJsonSchemaProvider<schemaFile=nullPath>
    //
    // [<Fact>]
    // let ``null root loaded from file Create builds the value`` () =
    //     expectEqual (Null.Create()) ()

    [<Literal>]
    let stringArrayPath = __SOURCE_DIRECTORY__ + "/../schemas/OldCodeGenerationTests/08-string-array.json"
    type StringArray = RefinedJsonSchemaProvider<schemaFile=stringArrayPath>

    [<Fact>]
    let ``string array root loaded from file Create builds the value`` () =
        expectEqual (expectOk (StringArray.Create [ "a"; "b" ])) [ "a"; "b" ]

    // A root $ref isn't supported - NJsonSchema leaves the referencing node's Type = None, and
    // that case only handles oneOf, so it hits "Unsupported JSON schema type None" instead of
    // resolving to the $defs entry. Pinned in the design-time project's KnownGaps/KnownGapTests.fs.
    // [<Literal>]
    // let refPath = __SOURCE_DIRECTORY__ + "/../schemas/OldCodeGenerationTests/09-ref.json"
    // type Ref = RefinedJsonSchemaProvider<schemaFile=refPath>
    //
    // [<Fact>]
    // let ``$ref to a $defs entry loaded from file Create builds the value`` () =
    //     expectEqual (Ref.Create "hello") "hello"

    [<Literal>]
    let objectWithRequiredPath = __SOURCE_DIRECTORY__ + "/../schemas/OldCodeGenerationTests/10-object-with-required.json"
    type ObjectWithRequired = RefinedJsonSchemaProvider<schemaFile=objectWithRequiredPath>

    [<Fact>]
    let ``object with required properties loaded from file Parse builds the record`` () =
        let v = expectOk (ObjectWithRequired.Parse("""{"first_name": "Robert", "last_name": "Jordan"}"""))
        expectEqual v.first_name (Some "Robert")
        expectEqual v.last_name "Jordan"

    [<Literal>]
    let objectWithoutRequiredPath = __SOURCE_DIRECTORY__ + "/../schemas/OldCodeGenerationTests/11-object-without-required.json"
    type ObjectWithoutRequired = RefinedJsonSchemaProvider<schemaFile=objectWithoutRequiredPath>
    type UncheckedObjectWithoutRequired =
        RefinedJsonSchemaProvider<schemaFile=objectWithoutRequiredPath, allowNonFiniteNumbers=true>

    [<Fact>]
    let ``object without required properties loaded from file Create builds the record`` () =
        let v = UncheckedObjectWithoutRequired.Create()
        expectEqual v.first_name None
        expectEqual v.last_name None

    [<Fact>]
    let ``object without required properties loaded from file Create gives Ok by default`` () =
        let v = expectOk (ObjectWithoutRequired.Create())
        expectEqual v.first_name None
        expectEqual v.last_name None

    // $codegen (a vendor keyword, unlike the spec's annotation-only $comment) isn't a known or
    // ignored keyword, so its mere presence takes the whole node off the compile-time path -
    // Create returns Result even though every property here would otherwise erase to a record.
    [<Literal>]
    let objectWithCodegenMetadataPath = __SOURCE_DIRECTORY__ + "/../schemas/OldCodeGenerationTests/12-object-with-codegen-metadata.json"
    type ObjectWithCodegenMetadata = RefinedJsonSchemaProvider<schemaFile=objectWithCodegenMetadataPath>

    [<Fact>]
    let ``object with an unrecognized $codegen keyword loaded from file still builds`` () =
        let v = expectOk (ObjectWithCodegenMetadata.Create())
        expectEqual v.``use`` None

    [<Literal>]
    let storageVariantApplicationIOSPath = __SOURCE_DIRECTORY__ + "/../schemas/OldCodeGenerationTests/13-storage-variant-application-ios.json"
    type StorageVariantApplicationIOS = RefinedJsonSchemaProvider<schemaFile=storageVariantApplicationIOSPath>

    [<Fact>]
    let ``storage variant application-iOS schema loaded from file Parse builds the record`` () =
        let v = expectOk (StorageVariantApplicationIOS.Parse("""{"bundleId": "com.example.app"}"""))
        expectEqual v.bundleId "com.example.app"
        expectEqual v.teamId None

    [<Literal>]
    let storageVariantSkipPropertyPath = __SOURCE_DIRECTORY__ + "/../schemas/OldCodeGenerationTests/14-storage-variant-skip-property.json"
    type StorageVariantSkipProperty = RefinedJsonSchemaProvider<schemaFile=storageVariantSkipPropertyPath>

    [<Fact>]
    let ``storage variant skip-property schema loaded from file Create builds the record`` () =
        let v = expectOk (StorageVariantSkipProperty.Create())
        expectEqual v.teamId None
        expectEqual v.bundleId None
