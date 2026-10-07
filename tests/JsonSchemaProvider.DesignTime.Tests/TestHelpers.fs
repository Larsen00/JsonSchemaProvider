namespace JsonSchemaProvider.Tests

// Shared setup for design-time tests: a GenerationContext built from real schema text, a type map
// for its objects, and evaluation of the generated quotations.
module TestHelpers =
    open System
    open FSharp.Quotations
    open FSharp.Linq.RuntimeHelpers
    open FSharp.Data
    open ProviderImplementation.ProvidedTypes
    open JsonSchemaProvider
    open JsonSchemaProvider.DesignTime.SchemaConversion
    open JsonSchemaProvider.DesignTime.ProviderConfiguration
    open JsonSchemaProvider.DesignTime.NodeConversions

    let defaultFlags : CompileFlags = { SkipRuntimeValidation = false; IgnoreSpecificKeywords = false }

    // Same hash/source pairing as TypeProvider.run, so generated validation code finds the schema.
    let contextWith (flags: CompileFlags) (schemaText: string) : GenerationContext =
        let schema = SchemaCache.parseSchema schemaText
        { Assembly = Reflection.Assembly.GetExecutingAssembly()
          NamespaceName = "Test"
          RootBaseType = typeof<NullableJsonValue>
          SchemaHashCode = schemaText.GetHashCode()
          SchemaString = schema.ToJson()
          CompileFlags = flags
          ConversionCache = Collections.Concurrent.ConcurrentDictionary() }

    // One placeholder provided type per object node, keyed by its Path.
    let typeMapFor (context: GenerationContext) (schemaType: JsonSchemaType) : TypeMap =
        let rec objectPaths schemaType =
            match schemaType with
            | JsonObject(keywords, properties) -> keywords.common.Path :: (properties |> List.collect (snd >> objectPaths))
            | JsonArray(inner, _) -> objectPaths inner
            | JsonOneOf(_, head, tail) -> head :: tail |> List.collect objectPaths
            | JsonBoolean _ | JsonInteger _ | JsonNumber _ | JsonString _ -> []

        objectPaths schemaType
        |> List.mapi (fun i path ->
            path, ProvidedTypeDefinition(context.Assembly, context.NamespaceName, $"Obj{i}", Some typeof<NullableJsonValue>))
        |> Map.ofList

    type Fixture =
        { Context: GenerationContext
          TypeMap: TypeMap
          Root: JsonSchemaType }

    let fixtureWith (flags: CompileFlags) (schemaText: string) : Fixture =
        let context = contextWith flags schemaText
        let root = parseJsonSchema schemaText
        { Context = context; TypeMap = typeMapFor context root; Root = root }

    let fixture (schemaText: string) = fixtureWith defaultFlags schemaText

    let convertRoot (f: Fixture) : NodeConversion = convert f.Context f.TypeMap f.Root

    let arrayConversionOf (f: Fixture) : ArrayConversion =
        match f.Root with
        | JsonArray(inner, keywords) -> buildArrayConversion f.Context f.TypeMap inner keywords
        | _ -> failwith "Root schema is not an array"

    // An integer array schema; `keywords` is spliced in after "items", e.g. ", \"minItems\": 2".
    let intArray (keywords: string) =
        """{ "type": "array", "items": { "type": "integer" }""" + keywords + " }"

    let eval (expr: Expr) : obj = LeafExpressionConverter.EvaluateQuotation expr

    let apply (lambda: Expr) (argType: Type) (arg: obj) : obj =
        eval (Expr.Application(lambda, Expr.Value(arg, argType)))

    let compact (json: JsonValue) = json.ToString(JsonSaveOptions.DisableFormatting)

    let normalize (jsonText: string) = compact (JsonValue.Parse jsonText)

    let toRuntime (conv: NodeConversion) (jsonText: string) : obj =
        apply conv.ToRuntime typeof<JsonValue> (JsonValue.Parse jsonText)

    let toJsonText (conv: NodeConversion) (value: obj) : string =
        apply conv.ToJson conv.RuntimeType value :?> JsonValue |> compact

    let roundTrip (conv: NodeConversion) (jsonText: string) : string =
        toJsonText conv (toRuntime conv jsonText)

    let isResultOf (expectedOk: Type) (actual: Type) =
        actual.IsGenericType
        && actual.GetGenericTypeDefinition() = typedefof<Result<_, _>>
        && actual.GetGenericArguments() = [| expectedOk; typeof<string list> |]
