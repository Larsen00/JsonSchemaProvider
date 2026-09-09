namespace JsonSchemaProvider.DesignTime

module TypeLevelConversion =
    open System
    open SchemaConversion
    open ProviderImplementation.ProvidedTypes
    open JsonSchemaProvider
    // open Microsoft.FSharp.Reflection

    type ClassMap = Map<string, ProvidedTypeDefinition>

    // This is the getter function. An it produces the type of an allready created instance of the fsharp type. 
    let rec fSharpTypeToCompileTimeType
        (classMap: ClassMap)
        (fSharpType: FSharpType)
        (compileFlags: ProviderConfiguration.CompileFlags)
        : Type =
        match fSharpType with
        | FSharpBool _ -> typeof<bool>
        | FSharpClass(keywords, _) -> classMap[keywords.common.Path]
        | FSharpList(innerFSharpType, arrayKeywords) ->
            let innerStaticType = fSharpTypeToCompileTimeType classMap innerFSharpType compileFlags
            JsonArrayProvidedType.FSharpListType innerStaticType arrayKeywords compileFlags

        | FSharpDouble _ -> typeof<double>
        | FSharpInt _ -> typeof<int>
        | FSharpString _ -> typeof<string>
        | FSharpOneOf innerFSharpTypes ->
            JsonOneOf.FSharpOneOfType <| List.map (fun t -> fSharpTypeToCompileTimeType classMap t compileFlags) innerFSharpTypes
            


    let rec fSharpTypeToRuntimeType (classMap: ClassMap) (fSharpType: FSharpType) (compileFlags: ProviderConfiguration.CompileFlags) : Type =
        match fSharpType with
        | FSharpBool _ -> typeof<bool>
        | FSharpClass _ -> typeof<NullableJsonValue>
        | FSharpList(innerFSharpType, arrayKeywords) ->
            let innerRuntimeType = fSharpTypeToRuntimeType classMap innerFSharpType compileFlags
            JsonArrayProvidedType.FSharpListType innerRuntimeType arrayKeywords compileFlags
        | FSharpDouble _ -> typeof<double>
        | FSharpInt _ -> typeof<int>
        | FSharpString _ -> typeof<string>
        | FSharpOneOf innerFSharpTypes ->
            JsonOneOf.FSharpOneOfType <| List.map (fun t -> fSharpTypeToRuntimeType classMap t compileFlags) innerFSharpTypes

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

    let rec fSharpTypeToMethodParameterType
        (classMap: ClassMap)
        (optional: bool)
        (fSharpType: FSharpType)
        (compileFlags: ProviderConfiguration.CompileFlags)
        : Type =
        let compileTimeType = fSharpTypeToCompileTimeType classMap fSharpType compileFlags
        nullableOrPlainType optional compileTimeType
