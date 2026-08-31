namespace JsonSchemaProvider.DesignTime

module TypeProvider =
    open System
    open System.Reflection
    open SchemaConversion
    open TypeLevelConversion
    open ExprGenerator
    open ProviderImplementation.ProvidedTypes
    open NJsonSchema
    open JsonSchemaProvider
    open FSharp.Data

    type private GenerationContext =
        { Assembly: Assembly
          NamespaceName: string
          RuntimeType: Type
          SchemaHashCode: int32
          SchemaString: string
          CompileFlags: ProviderConfiguration.CompileFlags }

    let rec private extractNestedClasses (fSharpType: FSharpType)  =
        match fSharpType with
        | FSharpClass(classID, properties) -> [(classID, properties)]
        | FSharpList(inner, _) -> extractNestedClasses inner
        | FSharpOneOf types -> types |> List.collect extractNestedClasses
        | FSharpBool | FSharpInt(_) | FSharpDouble | FSharpString -> []

    // create providedProperties for classes
    let rec private createProvidedProperties
        (context: GenerationContext)
        (classMap: Map<Guid, ProvidedTypeDefinition>)
        (fsharptype: FSharpType )
        : ProvidedProperty list =

        match fsharptype with
        | FSharpClass (_, []) -> []
        | FSharpClass (classId, (name, keywords, fsharptype as property :: rest)) -> 

            let plainPropertyCompileTimeType = fSharpTypeToCompileTimeType classMap fsharptype context.CompileFlags

            ProvidedProperty(
                propertyName = name,
                propertyType = optionalOrPlainType (not keywords.Required) plainPropertyCompileTimeType,
                getterCode = generatePropertyGetter classMap property context.CompileFlags
            )
            :: createProvidedProperties context classMap (FSharpClass (classId, rest))

        | _ -> failwith "idk not done"


    let private createMethodParameter (context: GenerationContext) classMap (fsharptype: FSharpType) isRequired parameterName =
        let parameterType = fSharpTypeToMethodParameterType classMap (not isRequired) fsharptype context.CompileFlags

        if isRequired then
            ProvidedParameter(parameterName, parameterType)
        else
            ProvidedParameter(parameterName, parameterType, false, defaultValueForNullableType parameterType)

    let rec private createMethodParameters (context: GenerationContext) classMap (fsharptype: FSharpType) =
        match fsharptype with
        | FSharpClass (_, []) -> []
        | FSharpClass (classId, (name, keywords, innerfsharptype) :: rest) ->
            createMethodParameter context classMap innerfsharptype keywords.Required name
            :: createMethodParameters context classMap (FSharpClass (classId, rest))

        | FSharpBool | FSharpInt _ | FSharpDouble | FSharpString ->
            [ createMethodParameter context classMap fsharptype true "value" ]

        | _ -> failwith "also dont know - createMethodParameters"

                
    // The .create method to create in instance of the provided type
    let private createProvidedCreateMethod
        (context: GenerationContext)
        (nestedClass: bool)
        (classMap: Map<Guid, ProvidedTypeDefinition>)
        (fsharptype: FSharpType)
        (providedTypeDefinition: ProvidedTypeDefinition)
        : ProvidedMethod =

        ProvidedMethod(
            methodName = "Create",
            parameters = createMethodParameters context classMap fsharptype,
            returnType = providedTypeDefinition,
            invokeCode =
                generateCreateInvokeCode
                    nestedClass
                    classMap
                    context.SchemaHashCode
                    context.SchemaString
                    fsharptype
                    context.CompileFlags,
            isStatic = true
        )

    let private createProvidedParseMethod
        (context: GenerationContext)
        (returnType: Type)
        : ProvidedMethod =
        let schemaHashCode = context.SchemaHashCode
        let schemaString = context.SchemaString

        ProvidedMethod(
            methodName = "Parse",
            parameters = [ ProvidedParameter("json", typeof<string>) ],
            returnType = returnType,
            isStatic = true,
            invokeCode =
                fun args ->
                    <@@
                        let schema = SchemaCache.retrieveSchema schemaHashCode schemaString

                        let validationErrors = schema.Validate((%%args[0]): string)

                        if Seq.isEmpty validationErrors then
                            NullableJsonValue(JsonValue.Parse(%%args[0]))
                        else
                            let message =
                                validationErrors
                                |> Seq.map (fun validationError -> validationError.ToString())
                                |> fun msgs ->
                                    System.String.Join(", ", msgs) |> sprintf "JSON Schema validation failed: %s"

                            raise (ArgumentException(message, ((%%args[0]): string)))
                    @@>
        )

    let private createprovidedTypeDefinition (context: GenerationContext) nestedClass className =
        ProvidedTypeDefinition(
            context.Assembly,
            context.NamespaceName,
            className + (if nestedClass then "Obj" else ""),
            Some context.RuntimeType
        )

    let rec private buildClassMapHelper context (nestedClass: bool) (name: string) (fsharptype: FSharpType) : (Guid * ProvidedTypeDefinition) list =
        match fsharptype with
        | FSharpClass(classId, properties) ->

            let thisTypeDef = createprovidedTypeDefinition context nestedClass name

            let childEntries =
                properties
                |> List.collect (fun (propertyName, _, t) -> buildClassMapHelper context true propertyName t)

            childEntries
            |> List.iter (fun (_, nestedClassProvidedTypeDefinition) -> thisTypeDef.AddMember nestedClassProvidedTypeDefinition)

            let merged = Map.ofList childEntries

            createProvidedProperties context merged fsharptype
            |> List.iter (fun providedProperty -> thisTypeDef.AddMember(providedProperty))

            let createMethod = createProvidedCreateMethod context nestedClass merged fsharptype thisTypeDef
            thisTypeDef.AddMember(createMethod)

            if not nestedClass then
                let parseMethod = createProvidedParseMethod context thisTypeDef
                thisTypeDef.AddMember(parseMethod)

            (classId, thisTypeDef) :: childEntries
        | FSharpList(inner, _) -> buildClassMapHelper context nestedClass name inner
        | FSharpOneOf types -> types |> List.collect (buildClassMapHelper context nestedClass name)
        | FSharpBool | FSharpInt _ | FSharpDouble | FSharpString -> []

    let private buildClassMap context (nestedClass: bool) (name: string) (fsharptype: FSharpType) : Map<Guid, ProvidedTypeDefinition> =
        buildClassMapHelper context nestedClass name fsharptype |> Map.ofList


    let run
        (schema: JsonSchema)
        (schemaHashCode: int32)
        (assembly: Assembly)
        (namespaceName: string)
        (typeName: string)
        (runtimeType: Type)
        (compileFlags: ProviderConfiguration.CompileFlags)
        : ProvidedTypeDefinition =

        let context =
            { Assembly = assembly
              NamespaceName = namespaceName
              RuntimeType = runtimeType
              SchemaHashCode = schemaHashCode
              SchemaString = schema.ToJson()
              CompileFlags = compileFlags }


        match parseJsonSchemaStructured schema |> jsonSchemaTypeToFSharpType with
        | FSharpClass(rootClassId, _) as fsharptype ->
            let classMap = buildClassMap context false typeName fsharptype
            classMap[rootClassId]
        | (FSharpBool | FSharpInt _ | FSharpDouble | FSharpString) as fsharptype ->
            let providedTypeDefinition = createprovidedTypeDefinition context false typeName

            let createMethod = createProvidedCreateMethod context false Map.empty fsharptype providedTypeDefinition
            providedTypeDefinition.AddMember(createMethod)

            let parseMethod = createProvidedParseMethod context providedTypeDefinition
            providedTypeDefinition.AddMember(parseMethod)

            providedTypeDefinition
        | _ -> failwith "Root schema must be an object or a primitive" // TODO: lift this restriction when list/oneOf root is wired up
