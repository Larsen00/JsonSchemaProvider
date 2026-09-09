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

    // Function that can locate the next fsharpclass inside a type
    let rec private extractNestedClasses (fSharpType: FSharpType)  =
        match fSharpType with
        | FSharpClass(classID, properties) -> [(classID, properties)]
        | FSharpList(inner, _) -> extractNestedClasses inner
        | FSharpOneOf (head, tail) -> head :: tail |> List.collect extractNestedClasses
        | FSharpBool _ | FSharpInt _ | FSharpDouble _ | FSharpString _ -> []

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
                propertyType = optionalOrPlainType (not <| Map.find name keywords.specific.Required) plainPropertyCompileTimeType,
                getterCode = generatePropertyGetter context classMap keywords property
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
            createMethodParameter context classMap innerfsharptype (Map.find name keywords.specific.Required) name
            :: createMethodParameters context classMap (FSharpClass (keywords, rest))

        | FSharpBool _ | FSharpInt _ | FSharpDouble _ | FSharpString _ | FSharpList _ ->
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
            invokeCode = generateCreateInvokeCode context classMap fsharptype,
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

    let private createprovidedTypeDefinition (context: GenerationContext) (suffix: string) className =
        ProvidedTypeDefinition(
            context.Assembly,
            context.NamespaceName,
            className + suffix,
            Some context.RuntimeType
        )

    // suffix identifies *why* this class is nested (property vs list item vs oneOf case) and
    // doubles as the "is this the root" check below - the root is the only caller that passes "".
    let rec private buildClassMapHelper context (suffix: string) (name: string) (fsharptype: FSharpType) : (String * ProvidedTypeDefinition) list =
        match fsharptype with
        | FSharpClass(keywords, properties) ->

            let thisTypeDef = createprovidedTypeDefinition context suffix name

            let childEntries =
                properties
                |> List.collect (fun (propertyName, t) -> buildClassMapHelper context "Obj" propertyName t)

            childEntries
            |> List.iter (fun (_, nestedClassProvidedTypeDefinition) -> thisTypeDef.AddMember nestedClassProvidedTypeDefinition)

            let merged = Map.ofList childEntries

            createProvidedProperties context merged fsharptype
            |> List.iter (fun providedProperty -> thisTypeDef.AddMember(providedProperty))

            let createMethod = createProvidedCreateMethod context merged fsharptype thisTypeDef
            thisTypeDef.AddMember(createMethod)

            if suffix = "" then
                let parseMethod = createProvidedParseMethod context thisTypeDef
                thisTypeDef.AddMember(parseMethod)

            (keywords.common.Path, thisTypeDef) :: childEntries
        | FSharpList(inner, _) -> buildClassMapHelper context "Item" name inner
        | FSharpOneOf (head, tail) -> head :: tail |> List.collect (buildClassMapHelper context "Case" name)
        | FSharpBool _ | FSharpInt _ | FSharpDouble _ | FSharpString _ -> []

    let private buildClassMap context (suffix: string) (name: string) (fsharptype: FSharpType) : Map<String, ProvidedTypeDefinition> =
        buildClassMapHelper context suffix name fsharptype |> Map.ofList


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
            buildClassMap context "" typeName fsharptype
            |> Map.find keywords.common.Path

        | FSharpBool _ | FSharpInt _ | FSharpDouble _ | FSharpString _ as fsharptype ->

            // Class map contains nested classes inside the type - since a primitive type dont have nested classes this is empty.
            let classMap = Map.empty

            let providedTypeDefinition = createprovidedTypeDefinition context "" typeName

            let returnType = fSharpTypeToCompileTimeType classMap fsharptype compileFlags

            let createMethod = createProvidedCreateMethod context classMap fsharptype returnType
            providedTypeDefinition.AddMember createMethod

            let parseMethod = createProvidedParseMethod context providedTypeDefinition
            providedTypeDefinition.AddMember parseMethod

            providedTypeDefinition
        
        | FSharpList _ as fsharplist ->
            let classMap = buildClassMap context "" "" fsharplist

            let providedTypeDefinition = createprovidedTypeDefinition context "" typeName

            extractNestedClasses fsharplist
            |> List.iter (fun (keywords, _) -> providedTypeDefinition.AddMember classMap[keywords.common.Path])

            let returnType = fSharpTypeToCompileTimeType classMap fsharplist compileFlags

            let createMethod = createProvidedCreateMethod context classMap fsharplist returnType
            providedTypeDefinition.AddMember createMethod

            providedTypeDefinition

        | _ -> failwith "Root schema must be an object or a primitive" // TODO: lift this restriction when list/oneOf root is wired up
