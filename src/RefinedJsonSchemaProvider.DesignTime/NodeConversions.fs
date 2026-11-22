namespace RefinedJsonSchemaProvider.DesignTime

module NodeConversions =
    open System
    open System.Reflection
    open FSharp.Quotations
    open FSharp.Data
    open SchemaConversion
    open ProviderConfiguration
    open ProviderImplementation.ProvidedTypes
    open RefinedJsonSchemaProvider
    open System.Collections.Concurrent
    open ArrayShape
    open IntegerShape

    // Each object's Path mapped to its provided type.
    type TypeMap = Map<string, ProvidedTypeDefinition>

    // The Path of a node; the root's is "#".
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

    // True when the JSON array in jsonArrVar is non-empty.
    let private hasAtLeastOneElement (jsonArrVar: Var) : Expr =
        <@@ (%%(Expr.Var jsonArrVar): JsonValue[]).Length > 0 @@>

    // ToTuple for ExactLength n (n >= 2): the nested pairs (h1, (h2, ... hn)) as a flat n-tuple.
    let private buildExactLengthToTuple (n: int) (item: ProviderConfiguration.NodeConversion) (runtimeType: Type) : ProviderConfiguration.RuntimeHelperConversion =
        let rec elems (remaining: int) (current: Expr) : Expr list =
            if remaining = 1 then
                [ current ]
            else
                Expr.TupleGet(current, 0) :: elems (remaining - 1) (Expr.TupleGet(current, 1))

        { Convert = CommonExprs.freshLambda "value" runtimeType (fun valueVar -> Expr.NewTuple(elems n (Expr.Var valueVar)))
          CompileTimeReturnType = Microsoft.FSharp.Reflection.FSharpType.MakeTupleType(Array.create n item.CompileTimeType) }

    // A ToList helper, where body builds the list from the term.
    let private buildToList (item: ProviderConfiguration.NodeConversion) (runtimeType: Type) (body: Var -> Expr) : ProviderConfiguration.RuntimeHelperConversion =
        { Convert = CommonExprs.freshLambda "value" runtimeType body
          CompileTimeReturnType = typedefof<_ list>.MakeGenericType item.CompileTimeType }

    // The ToList/ToTuple helpers of an array conversion; NoSpecific here is a provider bug, not a user error.
    let arrayHelpers (conversion: ProviderConfiguration.NodeConversion) : ProviderConfiguration.ArrayHelpers =
        match conversion.Specific with
        | ArraySpecific helpers -> helpers
        | NoSpecific -> failwith "Internal error: an array conversion is missing its ToList/ToTuple helpers."

    // An array of exactly one item as the item itself; the base case of ExactLength and MaxItemsSingle.
    let private buildSingleItemConversion (arrayKeywords: JsonArray.Keywords) (item: ProviderConfiguration.NodeConversion) : ProviderConfiguration.NodeConversion =
        let toRuntime =
            withJsonArrayLambda (fun jsonArrVar ->
                Expr.Application(item.ToRuntime, CommonExprs.jsonArrayHead (Expr.Var jsonArrVar)))

        let toJson =
            CommonExprs.freshLambda "term" item.RuntimeType (fun termVar ->
                CommonExprs.newJsonValueArray (
                    Expr.NewArray(typeof<JsonValue>, [ Expr.Application(item.ToJson, Expr.Var termVar) ])
                ))

        let toList =
            buildToList item item.RuntimeType (fun valueVar ->
                CommonExprs.newListSingleton item.RuntimeType (Expr.Var valueVar))

        { CompileTimeType = item.CompileTimeType
          RuntimeType = item.RuntimeType
          ToRuntime = toRuntime
          ToJson = toJson
          FullyCompilable = arrayKeywords.common.CanBeCompiled && item.FullyCompilable
          Specific = ArraySpecific { ToList = toList; ToTuple = None } }

    // The conversion of a non-empty array wrapped in an option, where None is the empty array.
    let private buildOptionConversion (item: ProviderConfiguration.NodeConversion) (nonEmpty: ProviderConfiguration.NodeConversion) : ProviderConfiguration.NodeConversion =
        let present = nonEmpty
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
            CommonExprs.freshLambda "term" runtimeType (fun termVar ->
                Expr.IfThenElse(
                    CommonExprs.getOptionIsSome present.RuntimeType (Expr.Var termVar),
                    Expr.Application(present.ToJson, CommonExprs.getOptionValue present.RuntimeType (Expr.Var termVar)),
                    CommonExprs.emptyJsonValueArray))

        // Some x -> nonEmpty.ToList x | None -> []
        let toList =
            buildToList item runtimeType (fun valueVar ->
                Expr.IfThenElse(
                    CommonExprs.getOptionIsSome present.RuntimeType (Expr.Var valueVar),
                    Expr.Application((arrayHelpers nonEmpty).ToList.Convert, CommonExprs.getOptionValue present.RuntimeType (Expr.Var valueVar)),
                    CommonExprs.newListEmpty item.RuntimeType))

        { CompileTimeType = compileTimeType
          RuntimeType = runtimeType
          ToRuntime = toRuntime
          ToJson = toJson
          FullyCompilable = present.FullyCompilable
          Specific = ArraySpecific { ToList = toList; ToTuple = None } }

    // fun term -> JsonEquality.distinctItems (toJson term), keeping each first occurrence.
    let private dropDuplicatesAfter (toJson: Expr) (runtimeType: Type) : Expr =
        CommonExprs.freshLambda "term" runtimeType (fun termVar ->
            CommonExprs.callDistinctItems (Expr.Application(toJson, Expr.Var termVar)))

    // Each integer type's conversions; the narrower ones are Checked so an out-of-range number throws instead of wrapping.
    let private integerConversion (integerType: IntegerType) (fullyCompilable: bool) : ProviderConfiguration.NodeConversion =
        let sized (dotnetType: Type) (toRuntime: Expr) (toJson: Expr) : ProviderConfiguration.NodeConversion =
            { CompileTimeType = dotnetType; RuntimeType = dotnetType; ToRuntime = toRuntime; ToJson = toJson; FullyCompilable = fullyCompilable; Specific = NoSpecific }

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

    // The conversion of a node, cached by its Path.
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
    // The types and conversions of one node, built together so they can't disagree.
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
                // #omit-start
                Specific = NoSpecific
                // #omit-end
            }

        // #elide-start convert-conversion
        | JsonInteger keywords ->
            match classifyIntegerShape context.CompileFlags keywords with
            | InvalidIntegerBounds ->
                failwith "A lower bound (minimum or exclusiveMinimum) is above an upper bound (maximum or exclusiveMaximum), so no integer is valid - Please check your schema."

            | UnboundedInteger(reason) -> failwith (unboundedIntegerMessage keywords.common.Path reason)

            | UnsupportedIntegerKeywords -> integerConversion IntegerType.Int32 false

            | SizedInteger(integerType, typeEnforcesBounds) ->
                integerConversion integerType (keywords.common.CanBeCompiled && typeEnforcesBounds)

        | JsonNumber keywords -> {
                CompileTimeType = typeof<double>
                RuntimeType = typeof<double>
                ToRuntime = <@@ fun (jsonVal: JsonValue) -> jsonVal.AsFloat() @@>
                ToJson = <@@ fun (runtimeObj: double) -> JsonValue.Float runtimeObj @@>
                FullyCompilable = keywords.common.CanBeCompiled
                Specific = NoSpecific
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
                Specific = NoSpecific
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
                // #omit-start
                Specific = NoSpecific
                // #omit-end
            }

        // #elide-start object-fully-compilable
        // An array's type depends on its shape, so buildArrayConversion builds it.
        // #elide-end array-case
        | JsonArray(itemType, arrayKeywords) ->
            buildArrayConversion context typeMap itemType arrayKeywords
        // #endregion array-case

        // Several branches can overlap (e.g. integer and number), so only a single branch skips validation.
        | JsonOneOf (keywords, head, tail) ->
            let conv = buildOneOfConversion context typeMap (head :: tail)
            let branchesCannotOverlap = List.isEmpty tail
            { conv with FullyCompilable = keywords.CanBeCompiled && branchesCannotOverlap && conv.FullyCompilable }
        // #elide-end object-fully-compilable
        // #elide-end convert-conversion
        // #endregion
    // #elide-start object-fully-compilable
        

    

    // The conversion of an array node by its ArrayShape; minItems and maxItems are never 0 here.
    and buildArrayConversion
        (context: GenerationContext)
        (typeMap: TypeMap)
        (itemType: JsonSchemaType)
        (arrayKeywords: JsonArray.Keywords)
        : ProviderConfiguration.NodeConversion =

        // #elide-start buildArrayConversion
        // The plain list conversion of the shapes without a compiled size.
        let defaultArrayConversion: ProviderConfiguration.NodeConversion =
            let item = convert context typeMap itemType

            let compileTimeType = typedefof<_ list>.MakeGenericType item.CompileTimeType
            let runtimeType     = typedefof<_ list>.MakeGenericType item.RuntimeType

            let toRuntime =
                CommonExprs.freshLambda "jsonVal" typeof<JsonValue> (
                    fun jsonValVar ->
                        let asArray = CommonExprs.callJsonValueAsArray (Expr.Var jsonValVar)
                        let mapped = CommonExprs.callArrayMap item.ToRuntime asArray typeof<JsonValue> item.RuntimeType
                        CommonExprs.callListOfArray mapped item.RuntimeType
                    )

            let toJson =
                CommonExprs.freshLambda "term" runtimeType (
                    fun termVar ->
                        let mappedBack = CommonExprs.callListMap item.ToJson (Expr.Var termVar) item.RuntimeType typeof<JsonValue>
                        let arrayOfList = CommonExprs.callArrayOfList mappedBack typeof<JsonValue>
                        CommonExprs.newJsonValueArray arrayOfList
                    )

            // Already a list, so ToList evaluates to the term itself.
            let toList = buildToList item runtimeType (fun valueVar -> Expr.Var valueVar)

            // MinItems is always None here; the check guards a future shape that falls back to the list.
            { CompileTimeType = compileTimeType
              RuntimeType = runtimeType
              ToRuntime = toRuntime
              ToJson = toJson
              FullyCompilable = arrayKeywords.common.CanBeCompiled && arrayKeywords.specific.MinItems.IsNone && item.FullyCompilable
              Specific = ArraySpecific { ToList = toList; ToTuple = None } }



        // #elide-end buildArrayConversion
        match classifyArrayShape context.CompileFlags arrayKeywords with

        | InvalidBounds ->
            // #elide-start buildArrayConversion
            failwith "MinItems cannot be greater than MaxItems - Please check your schema."
            // #elide-end buildArrayConversion

        // No compiled type for these keywords, so the plain list is validated at runtime.
        | UnsupportedKeywords ->
            // #elide-start buildArrayConversion
            { defaultArrayConversion with FullyCompilable = false }
            // #elide-end buildArrayConversion

        // One item left: the item itself.
        | ExactLength 1 ->
            // #elide-start buildArrayConversion
            buildSingleItemConversion arrayKeywords (convert context typeMap itemType)
            // #elide-end buildArrayConversion

        // (head, tail), where the tail has exactly n - 1 items.
        | ExactLength n ->
            // #elide-start buildArrayConversion
            let item = convert context typeMap itemType
            let tailKeywords =
                { arrayKeywords with
                    specific.MinItems = Some(n - 1)
                    specific.MaxItems = Some(n - 1) }
            let headTail = buildHeadTailConversion context typeMap itemType arrayKeywords item tailKeywords
            { headTail with Specific = ArraySpecific { arrayHelpers headTail with ToTuple = Some(buildExactLengthToTuple n item headTail.RuntimeType) } }
            // #elide-end buildArrayConversion

        // (head, tail), where the tail has minItems - 1 and ends as a plain list.
        | MinItemsPrefix minItems ->
            // #elide-start buildArrayConversion
            let item = convert context typeMap itemType
            let tailKeywords =
                { arrayKeywords with
                    specific.MinItems = if minItems > 1 then Some(minItems - 1) else None }
            buildHeadTailConversion context typeMap itemType arrayKeywords item tailKeywords
            // #elide-end buildArrayConversion

        // (head, tail), where the tail has both bounds lowered by one and ends as a maxItems shape.
        | MinMaxItems(minItems, maxItems) ->
            let item = convert context typeMap itemType
            let tailKeywords =
                { arrayKeywords with
                    specific.MinItems = if minItems > 1 then Some(minItems - 1) else None
                    specific.MaxItems = Some(maxItems - 1) }
            buildHeadTailConversion context typeMap itemType arrayKeywords item tailKeywords

        // The empty array or one item.
        | MaxItemsSingle ->
            // #elide-start buildArrayConversion
            let item = convert context typeMap itemType
            buildOptionConversion item (buildSingleItemConversion arrayKeywords item)
            // #elide-end buildArrayConversion

        // The empty array, or one item followed by at most maxItems - 1 more.
        | MaxItemsChain maxItems ->
            // #elide-start buildArrayConversion
            let item = convert context typeMap itemType
            let tailKeywords = { arrayKeywords with specific.MaxItems = Some(maxItems - 1) }
            buildOptionConversion item (buildHeadTailConversion context typeMap itemType arrayKeywords item tailKeywords)
            // #elide-end buildArrayConversion

        | Unbounded -> defaultArrayConversion

        // The plain list, with duplicates dropped on the way to JSON.
        | UniqueUnbounded ->
            // #elide-start buildArrayConversion
            { defaultArrayConversion with
                ToJson = dropDuplicatesAfter defaultArrayConversion.ToJson defaultArrayConversion.RuntimeType }
            // #elide-end buildArrayConversion

        // Built without uniqueItems, so only the whole array drops duplicates, not each tail.
        | UniqueMaxItems _ ->
            let nestedPairs =
                buildArrayConversion context typeMap itemType { arrayKeywords with specific.UniqueItems = false }
            { nestedPairs with ToJson = dropDuplicatesAfter nestedPairs.ToJson nestedPairs.RuntimeType }

    // The pair (head, tail) of a non-empty array, where the tail is built from tailKeywords.
    and private buildHeadTailConversion
        (context: GenerationContext)
        (typeMap: TypeMap)
        (itemType: JsonSchemaType)
        (arrayKeywords: JsonArray.Keywords)
        (item: ProviderConfiguration.NodeConversion)
        (tailKeywords: JsonArray.Keywords)
        : ProviderConfiguration.NodeConversion =

        let tail = buildArrayConversion context typeMap itemType tailKeywords

        // #region head-tail-types
        let compileTimeType =
            // #elide-start buildHeadTailConversion
            Microsoft.FSharp.Reflection.FSharpType.MakeTupleType [| item.CompileTimeType; tail.CompileTimeType |]
            // #elide-end buildHeadTailConversion
        let runtimeType =
            // #elide-start buildHeadTailConversion
            Microsoft.FSharp.Reflection.FSharpType.MakeTupleType [| item.RuntimeType; tail.RuntimeType |]
            // #elide-end buildHeadTailConversion
        // #endregion head-tail-types

        // #region head-tail-to-runtime
        let toRuntime =
            // #elide-start buildHeadTailConversion
            withJsonArrayLambda (fun jsonArrVar ->
                let headJson = CommonExprs.jsonArrayHead (Expr.Var jsonArrVar)
                let tailJson = CommonExprs.jsonArrayTail (Expr.Var jsonArrVar)
                let headRuntime = Expr.Application(item.ToRuntime, headJson)
                let tailRuntime = Expr.Application(tail.ToRuntime, tailJson)
                Expr.NewTuple [ headRuntime; tailRuntime ]
            )
            // #elide-end buildHeadTailConversion
        // #endregion head-tail-to-runtime

        // #region head-tail-to-json
        let toJson =
            // #elide-start buildHeadTailConversion
            CommonExprs.freshLambda "term" runtimeType (fun termVar ->
                let headRuntime = Expr.TupleGet(Expr.Var termVar, 0)
                let tailRuntime = Expr.TupleGet(Expr.Var termVar, 1)
                let headJson = Expr.Application(item.ToJson, headRuntime)
                let tailJson = Expr.Application(tail.ToJson, tailRuntime)
                CommonExprs.prependToJsonArray headJson tailJson
            )
            // #elide-end buildHeadTailConversion
        // #endregion head-tail-to-json

        // #omit-start buildHeadTailConversion
        // head :: tail.ToList(tail-value)
        let toList =
            buildToList item runtimeType (fun valueVar ->
                let headExpr = Expr.TupleGet(Expr.Var valueVar, 0)
                let tailExpr = Expr.TupleGet(Expr.Var valueVar, 1)
                let tailListExpr = Expr.Application((arrayHelpers tail).ToList.Convert, tailExpr)
                CommonExprs.newListCons item.RuntimeType headExpr tailListExpr)

        // #omit-end buildHeadTailConversion
        { CompileTimeType = compileTimeType
          RuntimeType = runtimeType
          ToRuntime = toRuntime
          ToJson = toJson
          FullyCompilable = arrayKeywords.common.CanBeCompiled && item.FullyCompilable && tail.FullyCompilable
          // #omit-start buildHeadTailConversion
          Specific = ArraySpecific { ToList = toList; ToTuple = None }
          // #omit-end buildHeadTailConversion
        }


    // The conversion of oneOf branches as nested Choice<head, tail>, since Choice has at most seven cases.
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
                CommonExprs.freshLambda "term" runtimeType (fun termVar ->
                    let isChoice1 = Expr.UnionCaseTest(Expr.Var termVar, choice1)
                    let headValue = CommonExprs.callGetChoice1Of2 head.RuntimeType tail.RuntimeType (Expr.Var termVar)
                    let tailValue = CommonExprs.callGetChoice2Of2 head.RuntimeType tail.RuntimeType (Expr.Var termVar)
                    Expr.IfThenElse(isChoice1, Expr.Application(head.ToJson, headValue), Expr.Application(tail.ToJson, tailValue)))

            { CompileTimeType = compileTimeType; RuntimeType = runtimeType; ToRuntime = toRuntime; ToJson = toJson; FullyCompilable = head.FullyCompilable && tail.FullyCompilable; Specific = NoSpecific }

    // True when the object's own keywords compile and every property is fully compilable.
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

    // The type of a property getter: an option when the property is optional.
    let optionalOrPlainType (optional: bool) (dotnetType: Type) : Type =
        if optional then
            typedefof<_ option>.MakeGenericType(dotnetType)
        else
            dotnetType

    // The type of a Create parameter: Nullable when it is optional and a value type.
    let nullableOrPlainType (optional: bool) (dotnetType: Type) : Type =
        if optional then
            if dotnetType.IsValueType then
                typedefof<Nullable<_>>.MakeGenericType(dotnetType)
            else
                dotnetType
        else
            dotnetType

    // The default of an optional Create parameter: an empty Nullable or null.
    let defaultValueForNullableType (compileTimeType: Type) : obj =
        if compileTimeType.IsValueType then Nullable() else null

    // The type of the Create parameter for a node.
    let schemaTypeToMethodParameterType
        (context: GenerationContext)
        (typeMap: TypeMap)
        (optional: bool)
        (schemaType: JsonSchemaType)
        : Type =
        let compileTimeType = (convert context typeMap schemaType).CompileTimeType
        nullableOrPlainType optional compileTimeType
