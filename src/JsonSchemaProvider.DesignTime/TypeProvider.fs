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

    type private ProvidedTypeData =
        { Assembly: Assembly
          NamespaceName: string
          RuntimeType: Type }

    let rec private extractNestedClasses (fSharpType: FSharpType) : (string * FSharpProperty list) list =
      match fSharpType with
      | FSharpClass(name, properties) -> [ (name, properties) ]
      | FSharpList(inner, _) -> extractNestedClasses inner
      | FSharpOneOf types -> types |> List.collect extractNestedClasses
      | FSharpBool | FSharpInt | FSharpDouble | FSharpString -> []

    let private createProvidedProperties
        (classMap: Map<string, ProvidedTypeDefinition>)
        (properties: FSharpProperty list)
        (compileFlags: ProviderConfiguration.CompileFlags)
        : ProvidedProperty list =
        [ for { Name = name
                Optional = optional
                FSharpType = fSharpType } as property in properties ->

              let plainPropertyCompileTimeType = fSharpTypeToCompileTimeType classMap fSharpType compileFlags

              ProvidedProperty(
                  propertyName = name,
                  propertyType = optionalOrPlainType optional plainPropertyCompileTimeType,
                  getterCode = generatePropertyGetter classMap property compileFlags
              ) ]

    let private createProvidedCreateMethod
        (nestedClass: bool)
        (classMap: Map<string, ProvidedTypeDefinition>)
        (properties: FSharpProperty list)
        (schemaHashCode: int32)
        (schemaString: string)
        (providedTypeDefinition: ProvidedTypeDefinition)
        (compileFlags: ProviderConfiguration.CompileFlags)
        : ProvidedMethod =
        let parameters =
            [ for property in properties ->
                  let parameterType =
                      fSharpTypeToMethodParameterType classMap property.Optional property.FSharpType compileFlags

                  if property.Optional then
                      ProvidedParameter(property.Name, parameterType, false, defaultValueForNullableType parameterType)
                  else
                      ProvidedParameter(property.Name, parameterType) ]

        ProvidedMethod(
            methodName = "Create",
            parameters = parameters,
            returnType = providedTypeDefinition,
            invokeCode = generateCreateInvokeCode nestedClass classMap schemaHashCode schemaString properties compileFlags,
            isStatic = true
        )

    let private createProvidedParseMethod
        (returnType: Type)
        (schemaHashCode: int32)
        (schemaString: string)
        : ProvidedMethod =
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
        (schemaHashCode: int32)
        (schemaString: string)
        (providedTypeData: ProvidedTypeData)
        (properties: FSharpProperty list)
        (compileFlags: ProviderConfiguration.CompileFlags)
        : Map<string, ProvidedTypeDefinition> =
        properties 
        |> List.collect (fun property -> extractNestedClasses property.FSharpType)
        |> List.map (fun (name, nestedProperties) ->
            name, fSharpClassTreeToProvidedTypeDefinition schemaHashCode schemaString providedTypeData name nestedProperties true compileFlags)
        |> Map.ofList

    and private fSharpClassTreeToProvidedTypeDefinition
        (schemaHashCode: int32)
        (schemaString: string)
        (providedTypeData: ProvidedTypeData)
        (className: string)
        (properties: FSharpProperty list)
        (nestedClass: bool)
        (compileFlags: ProviderConfiguration.CompileFlags)
        : ProvidedTypeDefinition =
        let providedTypeDefinition =
            ProvidedTypeDefinition(
                providedTypeData.Assembly,
                providedTypeData.NamespaceName,
                className + (if nestedClass then "Obj" else ""),
                Some(providedTypeData.RuntimeType)
            )


        let classMap =
            createNestedClassProvidedTypeDefinitions schemaHashCode schemaString providedTypeData properties compileFlags

        classMap
        |> Map.values
        |> Seq.iter (fun nestedClassProvidedTypeDefinition ->
            providedTypeDefinition.AddMember(nestedClassProvidedTypeDefinition))

        let providedProperties = createProvidedProperties classMap properties compileFlags

        providedProperties
        |> List.iter (fun providedProperty -> providedTypeDefinition.AddMember(providedProperty))

        let createMethod =
            createProvidedCreateMethod
                nestedClass
                classMap
                properties
                schemaHashCode
                schemaString
                providedTypeDefinition
                compileFlags

        providedTypeDefinition.AddMember(createMethod)

        if not nestedClass then
            let parseMethod =
                createProvidedParseMethod providedTypeDefinition schemaHashCode schemaString

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
        
        let providedTypeData ={ 
                Assembly = assembly
                NamespaceName = namespaceName
                RuntimeType = runtimeType 
        }

        match parseJsonSchemaStructured schema |> jsonObjectToFSharpClass typeName with
        | FSharpClass(className, properties) ->
            fSharpClassTreeToProvidedTypeDefinition schemaHashCode (schema.ToJson()) providedTypeData className properties false compileFlags
        | _ -> failwith "Root schema must be an object" // TODO: lift this restriction when oneOf-as-root is supported
