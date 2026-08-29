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
        | FSharpClass(classID, properties) -> [ (classID, properties) ]
        | FSharpList(inner, _) -> extractNestedClasses inner
        | FSharpOneOf types -> types |> List.collect extractNestedClasses
        | FSharpBool | FSharpInt(_) | FSharpDouble | FSharpString -> []

    // create providedProperties for classes
    let rec private createProvidedProperties
        (context: GenerationContext)
        (classMap: Map<Guid, ProvidedTypeDefinition>)
        (properties: List<PropertyName * JsonObject.SpecificKeywords * FSharpType> )
        : ProvidedProperty list =

        match properties with
        | [] -> []
        | name, keywords, fsharptype as property :: rest -> 

            let plainPropertyCompileTimeType = fSharpTypeToCompileTimeType classMap fsharptype context.CompileFlags

            ProvidedProperty(
                propertyName = name,
                propertyType = optionalOrPlainType keywords.Required plainPropertyCompileTimeType,
                getterCode = generatePropertyGetter classMap property context.CompileFlags
            )
            :: createProvidedProperties context classMap rest

    let rec private createMethodParameters (context: GenerationContext) classMap (properties: List<PropertyName * JsonObject.SpecificKeywords * FSharpType> )  =
        

        match properties with
        | [] -> []
        | (name, keywords, fsharptype) :: rest when keywords.Required ->
            let parameterType = fSharpTypeToMethodParameterType classMap keywords.Required fsharptype context.CompileFlags
            ProvidedParameter(name, parameterType, false, defaultValueForNullableType parameterType) 
            :: createMethodParameters context classMap rest

        | (name, keywords, fsharptype) :: rest ->
            let parameterType = fSharpTypeToMethodParameterType classMap keywords.Required fsharptype context.CompileFlags
            ProvidedParameter(name, parameterType)
            :: createMethodParameters context classMap rest

                
    // The .create method to create in instance of the provided type
    let private createProvidedCreateMethod
        (context: GenerationContext)
        (nestedClass: bool)
        (classMap: Map<Guid, ProvidedTypeDefinition>)
        (properties: List<PropertyName * JsonObject.SpecificKeywords * FSharpType> )
        (providedTypeDefinition: ProvidedTypeDefinition)
        : ProvidedMethod =

        ProvidedMethod(
            methodName = "Create",
            parameters = createMethodParameters context classMap properties,
            returnType = providedTypeDefinition,
            invokeCode =
                generateCreateInvokeCode
                    nestedClass
                    classMap
                    context.SchemaHashCode
                    context.SchemaString
                    properties
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

    let rec private createNestedClassProvidedTypeDefinitions
        (context: GenerationContext)
        properties
        : Map<Guid, ProvidedTypeDefinition> =
        properties
        |> List.collect (fun (name, keywords, fsharptype) -> extractNestedClasses fsharptype)
        |> List.map (fun (name, nestedProperties) ->
            name, fSharpClassTreeToProvidedTypeDefinition context "name" nestedProperties true)
        |> Map.ofList

    and private fSharpClassTreeToProvidedTypeDefinition
        (context: GenerationContext)
        (className: string)
        properties
        (nestedClass: bool)
        : ProvidedTypeDefinition =

        let providedTypeDefinition = createprovidedTypeDefinition context nestedClass className

        let classMap = createNestedClassProvidedTypeDefinitions context properties

        classMap
        |> Map.values
        |> Seq.iter (fun nestedClassProvidedTypeDefinition ->
            providedTypeDefinition.AddMember(nestedClassProvidedTypeDefinition))

        let providedProperties = createProvidedProperties context classMap properties

        providedProperties
        |> List.iter (fun providedProperty -> providedTypeDefinition.AddMember(providedProperty))

        let createMethod =
            createProvidedCreateMethod
                context
                nestedClass
                classMap
                properties
                providedTypeDefinition

        providedTypeDefinition.AddMember(createMethod)

        if not nestedClass then
            let parseMethod =
                createProvidedParseMethod context providedTypeDefinition

            providedTypeDefinition.AddMember(parseMethod)

        providedTypeDefinition

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
        | FSharpClass(classID, properties) ->
            fSharpClassTreeToProvidedTypeDefinition context typeName properties false
        | _ -> failwith "Root schema must be an object" // TODO: lift this restriction when oneOf-as-root is supported
