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

    // Many of the functions reuse a lot of the same static data hence a record type to store it.
    type private GenerationContext =
        { Assembly: Assembly
          NamespaceName: string
          RuntimeType: Type
          SchemaHashCode: int32
          SchemaString: string
          CompileFlags: ProviderConfiguration.CompileFlags }

    // Function that can locate the next fsharpclass inside a type
    let rec private extractNestedClasses (fSharpType: FSharpType)  =
        match fSharpType with
        | FSharpClass(classID, properties) -> [(classID, properties)]
        | FSharpList(inner, _) -> extractNestedClasses inner
        | FSharpOneOf types -> types |> List.collect extractNestedClasses
        | FSharpBool | FSharpInt(_) | FSharpDouble | FSharpString -> []

    // create providedProperties for classes
    let rec private createProvidedProperties
        (context: GenerationContext)
        (classMap: ClassMap)
        (fsharptype: FSharpType )
        : ProvidedProperty list =

        match fsharptype with
        | FSharpClass (_, []) -> []
        | FSharpClass (keywords, (name, fsharptype as property :: rest)) -> 

            let plainPropertyCompileTimeType = fSharpTypeToCompileTimeType classMap fsharptype context.CompileFlags

            ProvidedProperty(
                propertyName = name,
                propertyType = optionalOrPlainType (not <| Map.find name keywords.Required) plainPropertyCompileTimeType,
                getterCode = generatePropertyGetter classMap keywords property context.CompileFlags
            )
            :: createProvidedProperties context classMap (FSharpClass (keywords, rest))

        | _ -> failwith "idk not done"


    let private createMethodParameter (context: GenerationContext) (classMap: ClassMap) (fsharptype: FSharpType) isRequired parameterName =
        let parameterType = fSharpTypeToMethodParameterType classMap (not isRequired) fsharptype context.CompileFlags

        if isRequired then
            ProvidedParameter(parameterName, parameterType)
        else
            ProvidedParameter(parameterName, parameterType, false, defaultValueForNullableType parameterType)

    let rec private createMethodParameters (context: GenerationContext) (classMap: ClassMap) (fsharptype: FSharpType) =
        match fsharptype with
        | FSharpClass (_, []) -> []
        | FSharpClass (keywords, (name, innerfsharptype) :: rest) ->
            createMethodParameter context classMap innerfsharptype (Map.find name keywords.Required) name
            :: createMethodParameters context classMap (FSharpClass (keywords, rest))

        | FSharpBool | FSharpInt _ | FSharpDouble | FSharpString ->
            [ createMethodParameter context classMap fsharptype true "value" ]

        | _ -> failwith "also dont know - createMethodParameters"


    // The .create method to create in instance of the provided type
    let private createProvidedCreateMethod
        (context: GenerationContext)
        (classMap: ClassMap)
        (fsharptype: FSharpType)
        (returnType: Type)
        : ProvidedMethod =

        ProvidedMethod(
            methodName = "Create",
            parameters = createMethodParameters context classMap fsharptype,
            returnType = returnType,
            invokeCode =
                generateCreateInvokeCode
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

    let rec private buildClassMapHelper context (nestedClass: bool) (name: string) (fsharptype: FSharpType) : (String * ProvidedTypeDefinition) list =
        match fsharptype with
        | FSharpClass(keywords, properties) ->

            let thisTypeDef = createprovidedTypeDefinition context nestedClass name

            let childEntries =
                properties
                |> List.collect (fun (propertyName, t) -> buildClassMapHelper context true propertyName t)

            childEntries
            |> List.iter (fun (_, nestedClassProvidedTypeDefinition) -> thisTypeDef.AddMember nestedClassProvidedTypeDefinition)

            let merged = Map.ofList childEntries

            createProvidedProperties context merged fsharptype
            |> List.iter (fun providedProperty -> thisTypeDef.AddMember(providedProperty))

            let createMethod = createProvidedCreateMethod context merged fsharptype thisTypeDef
            thisTypeDef.AddMember(createMethod)

            if not nestedClass then
                let parseMethod = createProvidedParseMethod context thisTypeDef
                thisTypeDef.AddMember(parseMethod)

            (keywords.Path, thisTypeDef) :: childEntries
        | FSharpList(inner, _) -> buildClassMapHelper context nestedClass name inner
        | FSharpOneOf types -> types |> List.collect (buildClassMapHelper context nestedClass name)
        | FSharpBool | FSharpInt _ | FSharpDouble | FSharpString -> []

    let private buildClassMap context (nestedClass: bool) (name: string) (fsharptype: FSharpType) : Map<String, ProvidedTypeDefinition> =
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


        match parseJsonSchemaStructured schema schema |> jsonSchemaTypeToFSharpType with
        | FSharpClass(keywords, _) as fsharptype ->
            buildClassMap context false typeName fsharptype
            |> Map.find keywords.Path 
            
        | FSharpBool | FSharpInt _ | FSharpDouble | FSharpString as fsharptype ->

            // Class map contains nested classes inside the type - since a primitive type dont have nested classes this is empty.
            let classMap = Map.empty

            let providedTypeDefinition = createprovidedTypeDefinition context false typeName

            let returnType = fSharpTypeToCompileTimeType classMap fsharptype compileFlags

            let createMethod = createProvidedCreateMethod context classMap fsharptype returnType
            providedTypeDefinition.AddMember createMethod

            let parseMethod = createProvidedParseMethod context providedTypeDefinition
            providedTypeDefinition.AddMember parseMethod

            providedTypeDefinition
        | _ -> failwith "Root schema must be an object or a primitive" // TODO: lift this restriction when list/oneOf root is wired up
