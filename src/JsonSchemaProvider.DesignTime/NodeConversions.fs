namespace JsonSchemaProvider.DesignTime

module NodeConversions =
    open System
    open System.Reflection
    open FSharp.Quotations
    open FSharp.Data
    open SchemaConversion
    open ProviderConfiguration
    open ProviderImplementation.ProvidedTypes
    open JsonSchemaProvider
    open System.Collections.Concurrent
    open ArrayShape
    open IntegerShape

    type TypeMap = Map<string, ProvidedTypeDefinition>

    // Every type carries its own Path
    // A root-level node's Path is "#".
    let pathOf (schemaType: JsonSchemaType) : string =
        match schemaType with
        | JsonNumber keywords
        | JsonInteger keywords            -> keywords.common.Path
        | JsonBoolean keywords           -> keywords.common.Path
        | JsonString keywords         -> keywords.common.Path
        | JsonObject (keywords, _)     -> keywords.common.Path
        | JsonArray(_, keywords)       -> keywords.common.Path
        | JsonOneOf (keywords, _, _)  -> keywords.Path

    // Builds `fun jsonVal -> let jsonArr = jsonVal.AsArray() in buildBody jsonArr`
    let private withJsonArrayLambda (buildBody: Var -> Expr) : Expr =

        let body jsonValVar =
            let jsonArrVar = Var($"jsonArr{Guid.NewGuid()}", typeof<JsonValue[]>)
            Expr.Let(
                jsonArrVar,
                CommonExprs.callJsonValueAsArray (Expr.Var jsonValVar),
                buildBody jsonArrVar)

        CommonExprs.freshLambda "jsonVal" typeof<JsonValue> body

    // `true` when the JsonValue array behind `jsonArrVar` has at least one element
    let private hasAtLeastOneElement (jsonArrVar: Var) : Expr =
        <@@ (%%(Expr.Var jsonArrVar): JsonValue[]).Length > 0 @@>

    // Rebuilds the flat n-tuple (n >= 2) from an ExactLength nested-pair value (h1, (h2, ... hn)).
    let private buildExactLengthToTuple (n: int) (item: ProviderConfiguration.NodeConversion) (runtimeType: Type) : ProviderConfiguration.RuntimeHelperConversion =
        let rec elems (remaining: int) (current: Expr) : Expr list =
            if remaining = 1 then
                [ current ]
            else
                Expr.TupleGet(current, 0) :: elems (remaining - 1) (Expr.TupleGet(current, 1))

        { Convert = CommonExprs.freshLambda "value" runtimeType (fun valueVar -> Expr.NewTuple(elems n (Expr.Var valueVar)))
          CompileTimeReturnType = Microsoft.FSharp.Reflection.FSharpType.MakeTupleType(Array.create n item.CompileTimeType) }

    // Creates a ToList helper; `body` builds the list from the array value.
    let private buildToList (item: ProviderConfiguration.NodeConversion) (runtimeType: Type) (body: Var -> Expr) : ProviderConfiguration.RuntimeHelperConversion =
        { Convert = CommonExprs.freshLambda "value" runtimeType body
          CompileTimeReturnType = typedefof<_ list>.MakeGenericType item.CompileTimeType }

    // Exactly one item: the item itself, with no tuple around it. The base case of ExactLength,
    // and what MaxItemsSingle wraps in an option.
    let private buildSingleItemConversion (arrayKeywords: JsonArray.Keywords) (item: ProviderConfiguration.NodeConversion) : ProviderConfiguration.ArrayConversion =
        let toRuntime =
            withJsonArrayLambda (fun jsonArrVar ->
                Expr.Application(item.ToRuntime, CommonExprs.callArrayGet 0 (Expr.Var jsonArrVar) typeof<JsonValue>))

        let toJson =
            CommonExprs.freshLambda "runtimeObj" item.RuntimeType (fun runtimeObjVar ->
                CommonExprs.newJsonValueArray (
                    Expr.NewArray(typeof<JsonValue>, [ Expr.Application(item.ToJson, Expr.Var runtimeObjVar) ])
                ))

        let toList =
            buildToList item item.RuntimeType (fun valueVar ->
                CommonExprs.newListSingleton item.RuntimeType (Expr.Var valueVar))

        { common =
            { CompileTimeType = item.CompileTimeType
              RuntimeType = item.RuntimeType
              ToRuntime = toRuntime
              ToJson = toJson
              FullyCompilable = arrayKeywords.common.CanBeCompiled && item.FullyCompilable }
          specific = { ToList = toList; ToTuple = None } }

    // Wraps the conversion of a non-empty array in an option: None is the empty array, Some holds the non-empty array.
    // Both maxItems shapes are built this way, from one item (MaxItemsSingle) or a head and a tail (MaxItemsChain).
    let private buildOptionConversion (item: ProviderConfiguration.NodeConversion) (nonEmpty: ProviderConfiguration.ArrayConversion) : ProviderConfiguration.ArrayConversion =
        let present = nonEmpty.common
        let compileTimeType = typedefof<option<_>>.MakeGenericType [| present.CompileTimeType |]
        let runtimeType = typedefof<option<_>>.MakeGenericType [| present.RuntimeType |]

        // Some (present.ToRuntime jsonVal) if the array has at least one element, otherwise None.
        let toRuntime =
            CommonExprs.freshLambda "jsonVal" typeof<JsonValue> (fun jsonValVar ->
                let jsonArrVar = Var($"jsonArr{Guid.NewGuid()}", typeof<JsonValue[]>)
                Expr.Let(
                    jsonArrVar,
                    CommonExprs.callJsonValueAsArray (Expr.Var jsonValVar),
                    Expr.IfThenElse(
                        hasAtLeastOneElement jsonArrVar,
                        CommonExprs.newOptionSome present.RuntimeType (Expr.Application(present.ToRuntime, Expr.Var jsonValVar)),
                        CommonExprs.newOptionNone present.RuntimeType)))

        // Some x -> present.ToJson x | None -> []
        let toJson =
            CommonExprs.freshLambda "runtimeObj" runtimeType (fun runtimeObjVar ->
                Expr.IfThenElse(
                    CommonExprs.getOptionIsSome present.RuntimeType (Expr.Var runtimeObjVar),
                    Expr.Application(present.ToJson, CommonExprs.getOptionValue present.RuntimeType (Expr.Var runtimeObjVar)),
                    CommonExprs.emptyJsonValueArray))

        // Some x -> nonEmpty.ToList x | None -> []
        let toList =
            buildToList item runtimeType (fun valueVar ->
                Expr.IfThenElse(
                    CommonExprs.getOptionIsSome present.RuntimeType (Expr.Var valueVar),
                    Expr.Application(nonEmpty.specific.ToList.Convert, CommonExprs.getOptionValue present.RuntimeType (Expr.Var valueVar)),
                    CommonExprs.newListEmpty item.RuntimeType))

        { common =
            { CompileTimeType = compileTimeType
              RuntimeType = runtimeType
              ToRuntime = toRuntime
              ToJson = toJson
              FullyCompilable = present.FullyCompilable }
          specific = { ToList = toList; ToTuple = None } }

    // Builds `fun runtimeObj -> JsonEquality.distinctItems (toJson runtimeObj)`, keeping each first occurrence.
    let private dropDuplicatesAfter (toJson: Expr) (runtimeType: Type) : Expr =
        CommonExprs.freshLambda "runtimeObj" runtimeType (fun runtimeObjVar ->
            CommonExprs.callDistinctItems (Expr.Application(toJson, Expr.Var runtimeObjVar)))


    // Each integer type's conversions; the narrower ones are Checked so an out-of-range number throws instead of wrapping.
    let private integerConversion (integerType: IntegerType) (fullyCompilable: bool) : ProviderConfiguration.NodeConversion =
        let sized (dotnetType: Type) (toRuntime: Expr) (toJson: Expr) : ProviderConfiguration.NodeConversion =
            { CompileTimeType = dotnetType; RuntimeType = dotnetType; ToRuntime = toRuntime; ToJson = toJson; FullyCompilable = fullyCompilable }

        match integerType with
        | IntegerType.Int8 ->
            sized typeof<sbyte> <@@ fun (jsonVal: JsonValue) -> Checked.sbyte (jsonVal.AsInteger64()) @@> <@@ fun (runtimeObj: sbyte) -> JsonValue.Number(decimal runtimeObj) @@>
        | IntegerType.UInt8 ->
            sized typeof<byte> <@@ fun (jsonVal: JsonValue) -> Checked.byte (jsonVal.AsInteger64()) @@> <@@ fun (runtimeObj: byte) -> JsonValue.Number(decimal runtimeObj) @@>
        | IntegerType.Int16 ->
            sized typeof<int16> <@@ fun (jsonVal: JsonValue) -> Checked.int16 (jsonVal.AsInteger64()) @@> <@@ fun (runtimeObj: int16) -> JsonValue.Number(decimal runtimeObj) @@>
        | IntegerType.UInt16 ->
            sized typeof<uint16> <@@ fun (jsonVal: JsonValue) -> Checked.uint16 (jsonVal.AsInteger64()) @@> <@@ fun (runtimeObj: uint16) -> JsonValue.Number(decimal runtimeObj) @@>
        | IntegerType.Int32 ->
            sized typeof<int> <@@ fun (jsonVal: JsonValue) -> jsonVal.AsInteger() @@> <@@ fun (runtimeObj: int) -> JsonValue.Number(decimal runtimeObj) @@>
        | IntegerType.UInt32 ->
            sized typeof<uint32> <@@ fun (jsonVal: JsonValue) -> Checked.uint32 (jsonVal.AsInteger64()) @@> <@@ fun (runtimeObj: uint32) -> JsonValue.Number(decimal runtimeObj) @@>
        | IntegerType.Int64 ->
            sized typeof<int64> <@@ fun (jsonVal: JsonValue) -> jsonVal.AsInteger64() @@> <@@ fun (runtimeObj: int64) -> JsonValue.Number(decimal runtimeObj) @@>
        // AsInteger64 can't hold the upper half of uint64, so this one goes through decimal.
        | IntegerType.UInt64 ->
            sized typeof<uint64> <@@ fun (jsonVal: JsonValue) -> Checked.uint64 (jsonVal.AsDecimal()) @@> <@@ fun (runtimeObj: uint64) -> JsonValue.Number(decimal runtimeObj) @@>

    // The compile error for an unbounded integer; the hinted limits belong to the type the existing bounds pick.
    let unboundedIntegerMessage (path: string) (reason: UnboundedReason) : string =
        let location = if path = "#" then "at the schema root" else $"at '%s{path.TrimStart('#', '/')}'"
        let lowerOf integerType = string (fst (rangeOf integerType))
        let upperOf integerType = string (snd (rangeOf integerType))

        let problem, fix =
            match reason with
            | NoBounds ->
                "has no lower or upper bound, so valid numbers can overflow its F# type (int)",
                "Add a lower bound (\"minimum\" or \"exclusiveMinimum\") and an upper bound (\"maximum\" or \"exclusiveMaximum\") to it.\n"
                + $"     For int: \"minimum\": %s{lowerOf IntegerType.Int32}, \"maximum\": %s{upperOf IntegerType.Int32}"
            | NoLowerBound integerType ->
                $"has no lower bound, so valid numbers can overflow its F# type (%s{fsharpName integerType})",
                "Add \"minimum\" or \"exclusiveMinimum\" to it.\n"
                + $"     For %s{fsharpName integerType}: \"minimum\": %s{lowerOf integerType}"
            | NoUpperBound integerType ->
                $"has no upper bound, so valid numbers can overflow its F# type (%s{fsharpName integerType})",
                "Add \"maximum\" or \"exclusiveMaximum\" to it.\n"
                + $"     For %s{fsharpName integerType}: \"maximum\": %s{upperOf integerType}"
            | RangeTooWide integerType ->
                $"has bounds outside the range of %s{fsharpName integerType}, so valid numbers can overflow it",
                $"Narrow the bounds so they fit in %s{fsharpName integerType}."

        $"\nThe integer %s{location} %s{problem}.\n"
        + "Pick ONE of these fixes:\n"
        + $"  A) %s{fix}\n"
        + "  B) Or remove requireIntegerBounds=true. This turns the check off for every integer in the schema,\n"
        + "     and numbers outside the type then throw at runtime."

    // Wrapper function to "conversion" that uses a cache to avoid recomputation
    // #region convert-conversion
    // #region object-fully-compilable
    let rec convert
        (context: GenerationContext)
        (typeMap: TypeMap)
        (schemaType: JsonSchemaType)
        : ProviderConfiguration.NodeConversion =
        
        // #omit-start
        // useConversionCache=false skips the cache (used by the cache benchmark and equivalence tests)
        if not context.CompileFlags.UseConversionCache then conversion context typeMap schemaType else
        // #omit-end
        context.ConversionCache.GetOrAdd(
            pathOf schemaType,
            fun _ -> conversion context typeMap schemaType
        )


    // #elide-start
    // Builds everything there is to know about turning one JsonSchemaType node into F#: its
    // compile-time type, its runtime/erased type, and the two conversion functions between
    // JsonValue and that runtime type
    //
    // Its done like this because otherwise we needed four separate functions that all needed
    // to stay in sync with each other. -- a bonus is that we also get better performance with less overhead
    // #elide-end
    // #region array-case
    and conversion
        (context: GenerationContext)
        (typeMap: TypeMap)
        (schemaType: JsonSchemaType)
        : ProviderConfiguration.NodeConversion =
        
        match schemaType with
        // #elide-start array-case
        // #elide-start object-fully-compilable
        | JsonBoolean keywords -> {
                CompileTimeType = typeof<bool>
                RuntimeType = typeof<bool>
                ToRuntime = <@@ fun (jsonVal: JsonValue) -> jsonVal.AsBoolean() @@>
                ToJson = <@@ fun (runtimeObj: bool) -> JsonValue.Boolean(runtimeObj) @@>
                // #elide-start convert-conversion
                FullyCompilable = keywords.common.CanBeCompiled
                // #elide-end convert-conversion
            }

        // #elide-start convert-conversion
        | JsonInteger keywords ->
            match classifyIntegerShape context.CompileFlags keywords with
            | InvalidIntegerBounds _ ->
                failwith "A lower bound (minimum or exclusiveMinimum) is above an upper bound (maximum or exclusiveMaximum), so no integer is valid - Please check your schema."

            | UnboundedInteger(_, reason) -> failwith (unboundedIntegerMessage keywords.common.Path reason)

            | UnsupportedIntegerKeywords _ -> integerConversion IntegerType.Int32 false

            | SizedInteger(_, integerType, typeEnforcesBounds) ->
                integerConversion integerType (keywords.common.CanBeCompiled && typeEnforcesBounds)

        | JsonNumber keywords -> {
                CompileTimeType = typeof<double>
                RuntimeType = typeof<double>
                ToRuntime = <@@ fun (jsonVal: JsonValue) -> jsonVal.AsFloat() @@>
                ToJson = <@@ fun (runtimeObj: double) -> JsonValue.Float runtimeObj @@>
                FullyCompilable = keywords.common.CanBeCompiled
            }

        // A .NET string can be null and F# can't rule that out for provided types, so a string
        // node always needs a runtime check: null becomes JSON null, which validation rejects.
        | JsonString _ -> {
                CompileTimeType = typeof<string>
                RuntimeType = typeof<string>
                ToRuntime = <@@ fun (jsonVal: JsonValue) -> jsonVal.AsString() @@>
                ToJson = <@@ fun (runtimeObj: string) ->
                                if isNull runtimeObj then JsonValue.Null else JsonValue.String runtimeObj @@>
                FullyCompilable = false
            }

        // #elide-end object-fully-compilable
        | JsonObject(keywords, properties) ->
            {
                // #elide-start object-fully-compilable
                CompileTimeType = typeMap[keywords.common.Path]
                RuntimeType = typeof<NullableJsonValue>
                ToRuntime = <@@ fun (jsonVal: JsonValue) -> NullableJsonValue jsonVal @@>
                ToJson = <@@ fun (runtimeObj: NullableJsonValue) ->
                                if isNull runtimeObj then JsonValue.Null else runtimeObj.JsonVal @@>
                // #elide-end object-fully-compilable
                // Like a string, an object value can be null, so wherever one is passed in it needs
                // validation. The object's own Create only depends on its properties (isObjectFullyCompilable).
                FullyCompilable = false
            }

        // #elide-start object-fully-compilable
        // Since arrays have compile-time type support, the conversion is delegated to the function `buildArrayConversion`.
        // #elide-end array-case
        | JsonArray(itemType, arrayKeywords) ->
            (buildArrayConversion context typeMap itemType arrayKeywords).common
        // #endregion array-case

        // OneOf types are handled by the `buildOneOfConversion` function. keywords.CanBeCompiled
        // is about the oneOf node itself (e.g. a stray keyword sitting alongside "oneOf"), which
        // buildOneOfConversion has no access to - folded in here instead.
        // With several branches, a value may match more than one (e.g. integer and number), so
        // "exactly one" always needs runtime validation. A single branch cannot overlap itself.
        | JsonOneOf (keywords, head, tail) ->
            let conv = buildOneOfConversion context typeMap (head :: tail)
            let branchesCannotOverlap = List.isEmpty tail
            { conv with FullyCompilable = keywords.CanBeCompiled && branchesCannotOverlap && conv.FullyCompilable }
        // #elide-end object-fully-compilable
        // #elide-end convert-conversion
        // #endregion
    // #elide-start object-fully-compilable
        

    

    // Performs the recursive conversion for array types based on their item type and array keywords.
    //
    // Note: this assumes minItems and maxItems can never be 0 - a 0 value is converted to None in
    // SchemaConversion.fs.
    and buildArrayConversion
        (context: GenerationContext)
        (typeMap: TypeMap)
        (itemType: JsonSchemaType)
        (arrayKeywords: JsonArray.Keywords)
        : ProviderConfiguration.ArrayConversion =

        // #elide-start buildArrayConversion
        // Default conversion for F# list arrays based on their item type.
        let defaultArrayConversion: ProviderConfiguration.ArrayConversion =
            // Extract the item type conversion
            let item = convert context typeMap itemType

            // The compile-time and runtime types for the list based on the item type conversion
            let compileTimeType = typedefof<_ list>.MakeGenericType item.CompileTimeType
            let runtimeType     = typedefof<_ list>.MakeGenericType item.RuntimeType

            let toRuntime =
                CommonExprs.freshLambda "jsonVal" typeof<JsonValue> (
                    fun jsonValVar ->

                        // Calls asArray on the JSON value to get it as an array
                        let asArray = CommonExprs.callJsonValueAsArray (Expr.Var jsonValVar)

                        // Maps each element of the JSON array to its runtime representation using the item type conversion
                        let mapped = CommonExprs.callArrayMap item.ToRuntime asArray typeof<JsonValue> item.RuntimeType

                        // Turns the array in to a list
                        CommonExprs.callListOfArray mapped item.RuntimeType
                    )

            let toJson =
                CommonExprs.freshLambda "runtimeObj" runtimeType (
                    fun runtimeObjVar ->

                        // map each element of the list back to a JSON value
                        let mappedBack = CommonExprs.callListMap item.ToJson (Expr.Var runtimeObjVar) item.RuntimeType typeof<JsonValue>

                        // convert the list of JSON values back into a JSON array
                        let arrayOfList = CommonExprs.callArrayOfList mappedBack typeof<JsonValue>

                        // create a new JSON array from the array of JSON values (Reverse of asArray)
                        CommonExprs.newJsonValueArray arrayOfList
                    )

            // Already a list, so ToList returns it unchanged.
            let toList = buildToList item runtimeType (fun valueVar -> Expr.Var valueVar)

            // Every shape with minItems gets its own conversion, so MinItems is None whenever this
            // list is used as the node's type; the check only guards against a future shape that
            // falls back to the list while keeping minItems.
            { common =
                { CompileTimeType = compileTimeType
                  RuntimeType = runtimeType
                  ToRuntime = toRuntime
                  ToJson = toJson
                  FullyCompilable = arrayKeywords.common.CanBeCompiled && arrayKeywords.specific.MinItems.IsNone && item.FullyCompilable }
              specific = { ToList = toList; ToTuple = None } }



        // #elide-end buildArrayConversion
        match classifyArrayShape context.CompileFlags itemType arrayKeywords with

        // Invalid case: minItems greater than maxItems
        | InvalidBounds _ ->
            // #elide-start buildArrayConversion
            failwith "MinItems cannot be greater than MaxItems - Please check your schema."
            // #elide-end buildArrayConversion

        // Keyword combinations that does not have a compiled type -> so we gonna fallback to runtime validation
        // If RefinedTypes is off, we need to fallback to runtime validation regardless of other keyword combinations.
        | UnsupportedKeywords _ ->
            // #elide-start buildArrayConversion
            { defaultArrayConversion with common.FullyCompilable = false }
            // #elide-end buildArrayConversion

        // Exact-size, exactly one element left: just the element itself
        | ExactLength(_, _, 1) ->
            // #elide-start buildArrayConversion
            buildSingleItemConversion arrayKeywords (convert context typeMap itemType)
            // #elide-end buildArrayConversion

        // Exact-size tuple, more than one element left: (Head, Tail) where Tail recurses on the
        // same exact-length shape with n - 1
        | ExactLength(_, _, n) ->
            // #elide-start buildArrayConversion
            let item = convert context typeMap itemType
            let tailKeywords =
                { arrayKeywords with
                    specific.MinItems = Some(n - 1)
                    specific.MaxItems = Some(n - 1) }
            let headTail = buildHeadTailConversion context typeMap itemType arrayKeywords item tailKeywords
            { headTail with specific.ToTuple = Some(buildExactLengthToTuple n item headTail.common.RuntimeType) }
            // #elide-end buildArrayConversion

        // minItems, no maxItems: (Head, Tail) where Tail recurses with minItems decremented by one,
        // dropping to None once exhausted, so the tail ends as a plain list
        | MinItemsPrefix(_, _, minItems) ->
            // #elide-start buildArrayConversion
            let item = convert context typeMap itemType
            let tailKeywords =
                { arrayKeywords with
                    specific.MinItems = if minItems > 1 then Some(minItems - 1) else None }
            buildHeadTailConversion context typeMap itemType arrayKeywords item tailKeywords
            // #elide-end buildArrayConversion

        // minItems < maxItems: (Head, Tail) where Tail recurses with both bounds decremented by one;
        // once minItems is exhausted it is dropped, and the tail becomes a maxItems shape
        | MinMaxItems(_, _, minItems, maxItems) ->
            let item = convert context typeMap itemType
            let tailKeywords =
                { arrayKeywords with
                    specific.MinItems = if minItems > 1 then Some(minItems - 1) else None
                    specific.MaxItems = Some(maxItems - 1) }
            buildHeadTailConversion context typeMap itemType arrayKeywords item tailKeywords

        // maxItems = 1: option<item>, the empty array or exactly one item.
        | MaxItemsSingle _ ->
            // #elide-start buildArrayConversion
            let item = convert context typeMap itemType
            buildOptionConversion item (buildSingleItemConversion arrayKeywords item)
            // #elide-end buildArrayConversion

        // maxItems > 1: option<(item * tail)>, the empty array or one item followed by at most
        // maxItems - 1 more, the tail built by recursing on this same function.
        | MaxItemsChain(_, _, maxItems) ->
            // #elide-start buildArrayConversion
            let item = convert context typeMap itemType
            let tailKeywords = { arrayKeywords with specific.MaxItems = Some(maxItems - 1) }
            buildOptionConversion item (buildHeadTailConversion context typeMap itemType arrayKeywords item tailKeywords)
            // #elide-end buildArrayConversion

        // Default case: unbounded list.
        | Unbounded _ -> defaultArrayConversion

        // uniqueItems, no size bound: the plain list, with duplicates dropped on the way to JSON.
        | UniqueUnbounded _ ->
            // #elide-start buildArrayConversion
            { defaultArrayConversion with
                common.ToJson = dropDuplicatesAfter defaultArrayConversion.common.ToJson defaultArrayConversion.common.RuntimeType }
            // #elide-end buildArrayConversion

        // uniqueItems + maxItems: the option chain built without uniqueItems (so its tails don't
        // drop duplicates on their own), with duplicates dropped from the whole array on the way to JSON.
        | UniqueMaxItems _ ->
            // #elide-start buildArrayConversion
            let chain = buildArrayConversion context typeMap itemType { arrayKeywords with specific.UniqueItems = false }
            { chain with common.ToJson = dropDuplicatesAfter chain.common.ToJson chain.common.RuntimeType }
            // #elide-end buildArrayConversion

    // Shared by ExactLength (n > 1), MinItemsPrefix and MinMaxItems: peels one head off the array and
    // recurses via `buildArrayConversion` on `tailKeywords` for the rest, producing (Head, Tail).
    // Splitting a value of this shape into head/tail is then just F#'s own tuple destructuring -
    // no runtime logic needed beyond building the pair itself.
    and private buildHeadTailConversion
        (context: GenerationContext)
        (typeMap: TypeMap)
        (itemType: JsonSchemaType)
        (arrayKeywords: JsonArray.Keywords)
        (item: ProviderConfiguration.NodeConversion)
        (tailKeywords: JsonArray.Keywords)
        : ProviderConfiguration.ArrayConversion =

        let tailArray = buildArrayConversion context typeMap itemType tailKeywords
        let tail = tailArray.common

        let compileTimeType = Microsoft.FSharp.Reflection.FSharpType.MakeTupleType [| item.CompileTimeType; tail.CompileTimeType |]
        let runtimeType = Microsoft.FSharp.Reflection.FSharpType.MakeTupleType [| item.RuntimeType; tail.RuntimeType |]

        let toRuntime =
            withJsonArrayLambda (fun jsonArrVar ->
                let headRuntime = Expr.Application(item.ToRuntime, CommonExprs.callArrayGet 0 (Expr.Var jsonArrVar) typeof<JsonValue>)
                let tailJsonValue = CommonExprs.newJsonValueArray (CommonExprs.callArraySkip 1 (Expr.Var jsonArrVar) typeof<JsonValue>)
                let tailRuntime = Expr.Application(tail.ToRuntime, tailJsonValue)
                Expr.NewTuple [ headRuntime; tailRuntime ])

        let toJson =
            CommonExprs.freshLambda "runtimeObj" runtimeType (fun runtimeObjVar ->
                let headBack = Expr.Application(item.ToJson, Expr.TupleGet(Expr.Var runtimeObjVar, 0))
                let tailBack = Expr.Application(tail.ToJson, Expr.TupleGet(Expr.Var runtimeObjVar, 1))
                CommonExprs.newJsonValueArray (
                    CommonExprs.callArrayAppend
                        (Expr.NewArray(typeof<JsonValue>, [ headBack ]))
                        (CommonExprs.callJsonValueAsArray tailBack)
                        typeof<JsonValue>
                ))

        // head :: tail.ToList(tail-value)
        let toList =
            buildToList item runtimeType (fun valueVar ->
                let headExpr = Expr.TupleGet(Expr.Var valueVar, 0)
                let tailExpr = Expr.TupleGet(Expr.Var valueVar, 1)
                let tailListExpr = Expr.Application(tailArray.specific.ToList.Convert, tailExpr)
                CommonExprs.newListCons item.RuntimeType headExpr tailListExpr)

        { common =
            { CompileTimeType = compileTimeType
              RuntimeType = runtimeType
              ToRuntime = toRuntime
              ToJson = toJson
              FullyCompilable = arrayKeywords.common.CanBeCompiled && item.FullyCompilable && tail.FullyCompilable }
          specific = { ToList = toList; ToTuple = None } }


    // Choice has a limit of 7 generic parameters, so more than 2 branches nest as Choice<T1, Choice<T2, Choice<T3, ...>>>
    and buildOneOfConversion
        (context: GenerationContext)
        (typeMap: TypeMap)
        (branchSchemaTypes: JsonSchemaType list)
        : ProviderConfiguration.NodeConversion =
        match branchSchemaTypes with
        | [] -> failwith "OneOf must have at least one type"
        | [ single ] -> convert context typeMap single
        | headType :: restTypes ->
            let head = convert context typeMap headType
            let tail = buildOneOfConversion context typeMap restTypes
            let compileTimeType = ProvidedTypeBuilder.MakeGenericType(typedefof<Choice<_, _>>, [ head.CompileTimeType; tail.CompileTimeType ])
            let runtimeType = ProvidedTypeBuilder.MakeGenericType(typedefof<Choice<_, _>>, [ head.RuntimeType; tail.RuntimeType ])
            let cases = Microsoft.FSharp.Reflection.FSharpType.GetUnionCases runtimeType
            let choice1, choice2 = cases.[0], cases.[1]

            let toRuntime =
                CommonExprs.freshLambda "jsonVal" typeof<JsonValue> (fun jsonValVar ->
                    let headMatches =
                        SchemaValidationExprs.validateJsonSchemaExpr (Expr.Var jsonValVar) context.SchemaHashCode context.SchemaString (pathOf headType)
                    let thenBranch = Expr.NewUnionCase(choice1, [ Expr.Application(head.ToRuntime, Expr.Var jsonValVar) ])
                    let elseBranch = Expr.NewUnionCase(choice2, [ Expr.Application(tail.ToRuntime, Expr.Var jsonValVar) ])
                    Expr.IfThenElse(headMatches, thenBranch, elseBranch))

            let toJson =
                CommonExprs.freshLambda "runtimeObj" runtimeType (fun runtimeObjVar ->
                    let isChoice1 = Expr.UnionCaseTest(Expr.Var runtimeObjVar, choice1)
                    let headValue = CommonExprs.callGetChoice1Of2 head.RuntimeType tail.RuntimeType (Expr.Var runtimeObjVar)
                    let tailValue = CommonExprs.callGetChoice2Of2 head.RuntimeType tail.RuntimeType (Expr.Var runtimeObjVar)
                    Expr.IfThenElse(isChoice1, Expr.Application(head.ToJson, headValue), Expr.Application(tail.ToJson, tailValue)))

            { CompileTimeType = compileTimeType; RuntimeType = runtimeType; ToRuntime = toRuntime; ToJson = toJson; FullyCompilable = head.FullyCompilable && tail.FullyCompilable }

    // An object is fully compilable if it can be compiled itself (no stray keywords) and all of its properties are fully compilable.
    // #elide-end object-fully-compilable
    and isObjectFullyCompilable
        (context: GenerationContext)
        (typeMap: TypeMap)
        (keywords: JsonObject.Keywords)
        (properties: (PropertyName * JsonSchemaType) list)
        : bool =
        let isCompilable (_, propertyType) =
            (convert context typeMap propertyType).FullyCompilable
        keywords.common.CanBeCompiled && List.forall isCompilable properties
    // #endregion object-fully-compilable

    let optionalOrPlainType (optional: bool) (dotnetType: Type) : Type =
        if optional then
            typedefof<_ option>.MakeGenericType(dotnetType)
        else
            dotnetType

    let nullableOrPlainType (optional: bool) (dotnetType: Type) : Type =
        if optional then
            if dotnetType.IsValueType then
                typedefof<Nullable<_>>.MakeGenericType(dotnetType)
            else
                dotnetType
        else
            dotnetType

    let defaultValueForNullableType (compileTimeType: Type) : obj =
        if compileTimeType.IsValueType then Nullable() else null

    let schemaTypeToMethodParameterType
        (context: GenerationContext)
        (typeMap: TypeMap)
        (optional: bool)
        (schemaType: JsonSchemaType)
        : Type =
        let compileTimeType = (convert context typeMap schemaType).CompileTimeType
        nullableOrPlainType optional compileTimeType
