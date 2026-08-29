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

    let rec private extractNestedClasses (fSharpType: FSharpType) : (string * FSharpProperty list) list =
      match fSharpType with
      | FSharpClass(name, properties) -> [ (name, properties) ]
      | FSharpList(inner, _) -> extractNestedClasses inner
      | FSharpOneOf types -> types |> List.collect extractNestedClasses
      | FSharpBool | FSharpInt(_) | FSharpDouble | FSharpString -> []

    let private createProvidedProperties
        (context: GenerationContext)
        (classMap: Map<string, ProvidedTypeDefinition>)
        (properties: FSharpProperty list)
        : ProvidedProperty list =
        [ for { Name = name
                Optional = optional
                FSharpType = fSharpType } as property in properties ->

              let plainPropertyCompileTimeType =
                  fSharpTypeToCompileTimeType classMap fSharpType context.CompileFlags

              ProvidedProperty(
                  propertyName = name,
                  propertyType = optionalOrPlainType optional plainPropertyCompileTimeType,
                  getterCode = generatePropertyGetter classMap property context.CompileFlags
              ) ]

    let private createProvidedCreateMethod
        (context: GenerationContext)
        (nestedClass: bool)
        (classMap: Map<string, ProvidedTypeDefinition>)
        (properties: FSharpProperty list)
        (providedTypeDefinition: ProvidedTypeDefinition)
        : ProvidedMethod =
        let parameters =
            [ for property in properties ->
                  let parameterType =
                      fSharpTypeToMethodParameterType classMap property.Optional property.FSharpType context.CompileFlags

                  if property.Optional then
                      ProvidedParameter(property.Name, parameterType, false, defaultValueForNullableType parameterType)
                  else
                      ProvidedParameter(property.Name, parameterType) ]

        ProvidedMethod(
            methodName = "Create",
            parameters = parameters,
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

    let rec private createNestedClassProvidedTypeDefinitions
        (context: GenerationContext)
        (properties: FSharpProperty list)
        : Map<string, ProvidedTypeDefinition> =
        properties
        |> List.collect (fun property -> extractNestedClasses property.FSharpType)
        |> List.map (fun (name, nestedProperties) ->
            name, fSharpClassTreeToProvidedTypeDefinition context name nestedProperties true)
        |> Map.ofList

    and private fSharpClassTreeToProvidedTypeDefinition
        (context: GenerationContext)
        (className: string)
        (properties: FSharpProperty list)
        (nestedClass: bool)
        : ProvidedTypeDefinition =
        let providedTypeDefinition =
            ProvidedTypeDefinition(
                context.Assembly,
                context.NamespaceName,
                className + (if nestedClass then "Obj" else ""),
                Some(context.RuntimeType)
            )


        let classMap =
            createNestedClassProvidedTypeDefinitions context properties

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

        match parseJsonSchemaStructured schema |> jsonObjectToFSharpClass typeName with
        | FSharpClass(className, properties) ->
            fSharpClassTreeToProvidedTypeDefinition context className properties false
        | _ -> failwith "Root schema must be an object" // TODO: lift this restriction when oneOf-as-root is supported
