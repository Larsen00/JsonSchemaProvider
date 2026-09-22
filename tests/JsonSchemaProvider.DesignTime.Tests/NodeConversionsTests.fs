namespace JsonSchemaProvider.Tests

module NodeConversionsTests =
    open Expecto
    open JsonSchemaProvider.DesignTime.SchemaConversion
    open JsonSchemaProvider.DesignTime.NodeConversions
    open JsonSchemaProvider.DesignTime.ProviderConfiguration

    
    let commonKeywords : JsonSchemaProvider.Common.Keywords = { Path = "#"; CanBeCompiled = true }

    let jsonIntegerNoneKeywords : JsonSchemaProvider.JsonNumber.Keywords =
      { common = commonKeywords
        specific =
          { Minimum = None
            Maximum = None
            ExclusiveMinimum = None
            ExclusiveMaximum = None
            MultipleOf = None } }

    let jsonStringKeywords : JsonSchemaProvider.JsonString.Keywords =
      { common = commonKeywords
        specific = { MinLength = None; MaxLength = None; Pattern = None; Format = None } }

    let jsonBooleanKeywords : JsonSchemaProvider.JsonBoolean.Keywords = { common = commonKeywords }

    // "...At" variants for building several nodes that appear *together* in one FSharpType tree
    // (e.g. oneOf branches, or an array and its own inner type). convert now caches by Path, and
    // a real schema never gives a node and its own descendant the same Path - the plain
    // (non-"At") values above are only safe for a single, standalone node with no same-tree
    // sibling/descendant also built from them.
    let keywordsAt (path: string) : JsonSchemaProvider.Common.Keywords = { Path = path; CanBeCompiled = true }

    let intKeywordsAt (path: string) : JsonSchemaProvider.JsonNumber.Keywords =
      { common = keywordsAt path
        specific =
          { Minimum = None
            Maximum = None
            ExclusiveMinimum = None
            ExclusiveMaximum = None
            MultipleOf = None } }

    let stringKeywordsAt (path: string) : JsonSchemaProvider.JsonString.Keywords =
      { common = keywordsAt path
        specific = { MinLength = None; MaxLength = None; Pattern = None; Format = None } }

    let boolKeywordsAt (path: string) : JsonSchemaProvider.JsonBoolean.Keywords = { common = keywordsAt path }


    let noFlags = { SkipRuntimeValidation = false; IgnoreSpecificKeywords = false }

    // A function, not a single top-level value: convert now caches by the node's own Path, and
    // several tests below reuse the same Path ("#") for structurally different FSharpType shapes
    // (that was harmless before caching existed). A fresh context - and so a fresh, empty
    // ConversionCache - per test keeps those tests independent of each other and of test order.
    let private dummyContext () : GenerationContext =
        { Assembly = System.Reflection.Assembly.GetExecutingAssembly()
          NamespaceName = "Test"
          RootBaseType = typeof<obj>
          SchemaHashCode = 0
          SchemaString = "{}"
          CompileFlags = noFlags
          ConversionCache = System.Collections.Concurrent.ConcurrentDictionary() }

    let toCompileTimeType (fSharpType: FSharpType) =
        (convert (dummyContext ()) Map.empty fSharpType).CompileTimeType

    let oneOfSingleBranchYieldsPlainType =
        test "oneOf with a single branch yields the branch type directly" {
            let actual = toCompileTimeType (FSharpOneOf (commonKeywords, FSharpInt (intKeywordsAt "#/oneOf/0"), []))
            Expect.equal actual typeof<int> "single-branch oneOf should not be wrapped in Choice"
        }

    let oneOfTwoBranchesYieldsChoice =
        test "oneOf with two branches yields Choice<T1,T2>" {
            let actual =
                toCompileTimeType (
                    FSharpOneOf (commonKeywords, FSharpInt (intKeywordsAt "#/oneOf/0"), [ FSharpString (stringKeywordsAt "#/oneOf/1") ])
                )
            Expect.equal actual typeof<Choice<int, string>> "two-branch oneOf should be Choice<int,string>"
        }

    let oneOfThreeBranchesYieldsNestedChoice =
        test "oneOf with three branches yields Choice<T1, Choice<T2,T3>>" {
            let actual =
                toCompileTimeType (
                    FSharpOneOf (
                        commonKeywords,
                        FSharpInt (intKeywordsAt "#/oneOf/0"),
                        [ FSharpString (stringKeywordsAt "#/oneOf/1"); FSharpBool (boolKeywordsAt "#/oneOf/2") ]
                    )
                )
            Expect.equal actual typeof<Choice<int, Choice<string, bool>>> "three-branch oneOf should nest"
        }

    let jsonArrayKeywordsAt (path: string) (minItems: int option) (maxItems: int option) : JsonSchemaProvider.JsonArray.Keywords =
        { common = keywordsAt path
          specific =
            { MinItems = minItems
              MaxItems = maxItems
              UniqueItems = false
              AllowAdditionalItems = true
              HasAdditionalItemsSchema = false } }

    let jsonArrayKeywords (minItems: int option) (maxItems: int option) : JsonSchemaProvider.JsonArray.Keywords =
        jsonArrayKeywordsAt "#" minItems maxItems

    // This can only be tested here, not via ArrayTests.fs: a schema with maxItems < minItems makes
    // buildArrayConversion fail while the type provider is generating types, i.e. it would fail to
    // *compile* a `type Bad = JsonSchemaProvider<schema=...>` declaration rather than raise
    // something Expect.throws could wrap around a running Create/Parse call.
    let maxItemsLessThanMinItemsThrows =
        test "buildArrayConversion rejects a schema where maxItems < minItems" {
            let arrayType = FSharpList(FSharpInt jsonIntegerNoneKeywords, jsonArrayKeywords (Some 3) (Some 2))
            Expect.throws (fun () -> toCompileTimeType arrayType |> ignore) "maxItems < minItems should fail fast, not silently produce a type"
        }

    // NodeConversion is a record (reference type), so obj.ReferenceEquals here proves the second
    // call returned the literal cached instance rather than a freshly-built equal-looking one -
    // Expr trees embed Vars whose identity is fresh per construction (see CommonExprs.freshLambda),
    // so two independently-computed conversions of the same node are never reference-equal even
    // when they're behaviorally identical.
    let convertCachesRepeatedCallsOnTheSameNode =
        test "convert returns the exact same cached instance for a repeated call on the same node" {
            let context = dummyContext ()
            let fSharpType = FSharpInt jsonIntegerNoneKeywords

            let first = convert context Map.empty fSharpType
            let second = convert context Map.empty fSharpType

            Expect.isTrue (obj.ReferenceEquals(first, second)) "second call should hit the cache, not recompute"
        }

    let convertDoesNotConflateDifferentNodesSharingAContext =
        test "convert keeps distinct nodes distinct within the same cache" {
            let context = dummyContext ()

            let intConversion = convert context Map.empty (FSharpInt jsonIntegerNoneKeywords)
            let stringConversion =
                convert
                    context
                    Map.empty
                    (FSharpString { jsonStringKeywords with common = { commonKeywords with Path = "#/other" } })

            Expect.notEqual intConversion.CompileTimeType stringConversion.CompileTimeType
                "two different-Path nodes must not share a cache entry"
        }

    // buildArrayConversion's minItems-prefix case recurses on itself, calling `convert` on the
    // *same* innerType at every level (see the "im not super sure ... TODO" comment on that case).
    // This pins down that the cache actually collapses that recursive re-conversion: the fix for
    // the recomputation problem the TODO was worried about.
    let convertCachesTheSharedInnerTypeAcrossArrayRecursion =
        test "convert caches an array's inner-type conversion across the recursive tuple-building levels" {
            let context = dummyContext ()
            // Array at "#", inner element at "#/items" - distinct paths, exactly like a real
            // schema's own array/items relationship, so the array's own cache entry (at "#")
            // can't collide with (and mask) its inner element's entry (at "#/items").
            let innerType = FSharpInt (intKeywordsAt "#/items")

            // Compute the inner type once up front, exactly as buildArrayConversion's first
            // recursion level will, then let the 3-element tuple case recurse over the same node.
            let innerDirect = convert context Map.empty innerType
            let arrayType = FSharpList(innerType, jsonArrayKeywordsAt "#" (Some 3) (Some 3))
            convert context Map.empty arrayType |> ignore

            let innerAfterArrayConversion = convert context Map.empty innerType
            Expect.isTrue
                (obj.ReferenceEquals(innerDirect, innerAfterArrayConversion))
                "the inner type's conversion should still be the one cached instance, not recomputed by the array recursion"
        }

    [<Tests>]
    let tests =
        testList
            "JsonSchemaProvider.Tests.NodeConversionsTests"
            [ oneOfSingleBranchYieldsPlainType
              oneOfTwoBranchesYieldsChoice
              oneOfThreeBranchesYieldsNestedChoice
              maxItemsLessThanMinItemsThrows
              convertCachesRepeatedCallsOnTheSameNode
              convertDoesNotConflateDifferentNodesSharingAContext
              convertCachesTheSharedInnerTypeAcrossArrayRecursion
              ]
