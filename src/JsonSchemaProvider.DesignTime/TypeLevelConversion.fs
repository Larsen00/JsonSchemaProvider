namespace JsonSchemaProvider.DesignTime

module TypeLevelConversion =
    open System
    open SchemaConversion
    open ProviderImplementation.ProvidedTypes
    open JsonSchemaProvider
    // open Microsoft.FSharp.Reflection

    let rec fSharpTypeToCompileTimeType
        (classMap: Map<string, ProvidedTypeDefinition>)
        (fSharpType: FSharpType)
        (compileFlags: ProviderConfiguration.CompileFlags)
        : Type =
        match fSharpType with
        | FSharpBool -> typeof<bool>
        | FSharpClass(name, _) -> classMap[name]
        | FSharpList(innerFSharpType, arrayKeywords) ->
            let innerStaticType = fSharpTypeToCompileTimeType classMap innerFSharpType compileFlags
            JsonArray.FSharpListType innerStaticType arrayKeywords compileFlags
            
        | FSharpDouble -> typeof<double>
        | FSharpInt -> typeof<int>
        | FSharpString -> typeof<string>
        | FSharpOneOf innerFSharpTypes -> 
            JsonOneOf.FSharpOneOfType <| List.map (fun t -> fSharpTypeToCompileTimeType classMap t compileFlags) innerFSharpTypes
            


    let rec fSharpTypeToRuntimeType (classMap: Map<string, ProvidedTypeDefinition>) (fSharpType: FSharpType) (compileFlags: ProviderConfiguration.CompileFlags) : Type =
        match fSharpType with
        | FSharpBool -> typeof<bool>
        | FSharpClass(_) -> typeof<NullableJsonValue>
        | FSharpList(innerFSharpType, arrayKeywords) -> 
            let innerRuntimeType = fSharpTypeToRuntimeType classMap innerFSharpType compileFlags
            JsonArray.FSharpListType innerRuntimeType arrayKeywords compileFlags
        | FSharpDouble -> typeof<double>
        | FSharpInt -> typeof<int>
        | FSharpString -> typeof<string>
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
        (classMap: Map<string, ProvidedTypeDefinition>)
        (optional: bool)
        (fSharpType: FSharpType)
        (compileFlags: ProviderConfiguration.CompileFlags)
        : Type =
        let compileTimeType = fSharpTypeToCompileTimeType classMap fSharpType compileFlags
        nullableOrPlainType optional compileTimeType
