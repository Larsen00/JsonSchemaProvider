namespace JsonSchemaProvider.DesignTime

module TypeProvider =
    open System
    open System.Reflection
    open SchemaConversion
    open ProviderConfiguration
    open NodeConversions
    open ExprGenerator
    open ProviderImplementation.ProvidedTypes
    open NJsonSchema
    open JsonSchemaProvider
    open System.Collections.Concurrent
    open FSharp.Quotations

    // Function that can locate the next fsharpclass inside a type
    let rec private extractNestedClasses (schemaType: JsonSchemaType)  =
        match schemaType with
        | JsonObject(classID, properties) -> [(classID, properties)]
        | JsonArray(inner, _) -> extractNestedClasses inner
        | JsonOneOf (_, head, tail) -> head :: tail |> List.collect extractNestedClasses
        | JsonBoolean _ | JsonInteger _ | JsonNumber _ | JsonString _ -> []

    let private createProvidedTypeDefinition (context: GenerationContext) (suffix: string) className =
        ProvidedTypeDefinition(
            context.Assembly,
            context.NamespaceName,
            className + suffix,
            Some context.RootBaseType
        )

    // Creates a static method (e.g. ToList) that applies the given conversion to its argument.
    let private createProvidedConversionMethod (methodName: string) (parameterType: Type) (conversion: ProviderConfiguration.RuntimeHelperConversion) : ProvidedMethod =
        ProvidedMethod(
            methodName = methodName,
            parameters = [ ProvidedParameter("value", parameterType) ],
            returnType = conversion.CompileTimeReturnType,
            isStatic = true,
            invokeCode = fun args -> Expr.Application(conversion.Convert, args.[0])
        )

    // create providedProperties for classes
    let rec private createProvidedProperties
        (context: GenerationContext)
        (typeMap: TypeMap)
        (schemaType: JsonSchemaType )
        : ProvidedProperty list =

        match schemaType with
        | JsonObject (_, []) -> []
        | JsonObject (keywords, (name, schemaType as property :: rest)) -> 

            let plainPropertyCompileTimeType = (convert context typeMap schemaType).CompileTimeType

            ProvidedProperty(
                propertyName = name,
                propertyType = optionalOrPlainType (not <| Map.find name keywords.specific.Required) plainPropertyCompileTimeType,
                getterCode = generatePropertyGetter context typeMap keywords property
            )
            :: createProvidedProperties context typeMap (JsonObject (keywords, rest))

        | _ -> failwith "idk not done"


    // Adds ToList/ToTuple for one array to `target`, then helper types for any arrays inside its items.
    let rec private addArrayHelperMethods
        (context: GenerationContext)
        (typeMap: TypeMap)
        (target: ProvidedTypeDefinition)
        (innerType: JsonSchemaType)
        (arrayKeywords: JsonArray.Keywords)
        : unit =

        let array = buildArrayConversion context typeMap innerType arrayKeywords
        let parameterType = array.common.CompileTimeType

        target.AddMember(createProvidedConversionMethod "ToList" parameterType array.specific.ToList)

        array.specific.ToTuple
        |> Option.iter (fun toTuple -> target.AddMember(createProvidedConversionMethod "ToTuple" parameterType toTuple))

        addArrayHelperTypes context typeMap target "Item" innerType

    // Adds a "<name>Array" helper type to `parent` for each array in `schemaType`; oneOf branch i is named "<name>Case<i>".
    and private addArrayHelperTypes
        (context: GenerationContext)
        (typeMap: TypeMap)
        (parent: ProvidedTypeDefinition)
        (name: string)
        (schemaType: JsonSchemaType)
        : unit =

        match schemaType with
        | JsonArray(innerType, arrayKeywords) ->
            let helperType = createProvidedTypeDefinition context "Array" name
            addArrayHelperMethods context typeMap helperType innerType arrayKeywords
            parent.AddMember helperType
        | JsonOneOf(_, head, tail) ->
            head :: tail
            |> List.iteri (fun i branch -> addArrayHelperTypes context typeMap parent $"{name}Case{i + 1}" branch)
        // A class gets its own helpers when buildTypeMapHelper builds it.
        | JsonObject _ | JsonBoolean _ | JsonInteger _ | JsonNumber _ | JsonString _ -> ()

    let private createMethodParameter (context: GenerationContext) (typeMap: TypeMap) (schemaType: JsonSchemaType) isRequired parameterName =
        let parameterType = schemaTypeToMethodParameterType context typeMap (not isRequired) schemaType

        if isRequired then
            ProvidedParameter(parameterName, parameterType)
        else
            ProvidedParameter(parameterName, parameterType, false, defaultValueForNullableType parameterType)

    let rec private createMethodParameters (context: GenerationContext) (typeMap: TypeMap) (schemaType: JsonSchemaType) =
        match schemaType with
        | JsonObject (_, []) -> []
        | JsonObject (keywords, (name, innerSchemaType) :: rest) ->
            createMethodParameter context typeMap innerSchemaType (Map.find name keywords.specific.Required) name
            :: createMethodParameters context typeMap (JsonObject (keywords, rest))

        | JsonBoolean _ | JsonInteger _ | JsonNumber _ | JsonString _ | JsonArray _ | JsonOneOf _ ->
            [ createMethodParameter context typeMap schemaType true "value" ]


    // The .create method to create in instance of the provided type
    let private createProvidedCreateMethod
        (context: GenerationContext)
        (typeMap: TypeMap)
        (schemaType: JsonSchemaType)
        (returnType: Type)
        : ProvidedMethod =

        ProvidedMethod(
            methodName = "Create",
            parameters = createMethodParameters context typeMap schemaType,
            returnType = returnType,
            invokeCode = generateCreateInvokeCode context typeMap schemaType,
            isStatic = true
        )


    // Parse evaluates to Result<valueType, string list> for every root kind - see
    // ExprGenerator.generateParseInvokeCode for why it can't drop the Result the way Create can.
    let private createProvidedParseMethod
        (context: GenerationContext)
        (path: string)
        (valueType: Type)
        (runtimeType: Type)
        (toRuntime: Expr)
        : ProvidedMethod =

        ProvidedMethod(
            methodName = "Parse",
            parameters = [ ProvidedParameter("json", typeof<string>) ],
            returnType = typedefof<Result<_,_>>.MakeGenericType(valueType, typeof<string list>),
            isStatic = true,
            invokeCode = generateParseInvokeCode context path runtimeType toRuntime
        )

    // suffix identifies *why* this class is nested (property vs list item vs oneOf case); the root passes "".
    let rec private buildTypeMapHelper
        (context: GenerationContext)
        (suffix: string)
        (name: string)
        (schemaType: JsonSchemaType)
        : (String * ProvidedTypeDefinition) list =

        match schemaType with
        | JsonObject(keywords, properties) ->
            let providedType = createProvidedTypeDefinition context suffix name

            let nestedTypes =
                properties
                |> List.collect (fun (propertyName, propertyType) ->
                    buildTypeMapHelper context "Obj" propertyName propertyType)

            nestedTypes
            |> List.iter (fun (_, nestedType) -> providedType.AddMember nestedType)

            // The class itself is registered too, so convert can be called on its own node.
            let typeMap = Map.ofList ((keywords.common.Path, providedType) :: nestedTypes)
            let conversion = convert context typeMap schemaType

            createProvidedProperties context typeMap schemaType
            |> List.iter (fun property -> providedType.AddMember property)

            // #elide-start
            properties
            |> List.iter (fun (propertyName, propertyType) ->
                addArrayHelperTypes context typeMap providedType propertyName propertyType)
            // #elide-end

            // #omit-start
            // Create returns the type directly when it can't fail, otherwise a Result.
            let returnType =
                if context.CompileFlags.SkipRuntimeValidation || conversion.FullyCompilable then
                    providedType :> Type
                else
                    typedefof<Result<_,_>>.MakeGenericType(providedType, typeof<string list>)
            // #omit-end
            // #old let returnType = providedType :> Type

            let createMethod =
                createProvidedCreateMethod context typeMap schemaType returnType
            providedType.AddMember createMethod

            // Like Create, every class gets Parse, validated against its own sub-schema by Path.
            let parseMethod =
                createProvidedParseMethod
                    context keywords.common.Path conversion.CompileTimeType conversion.RuntimeType conversion.ToRuntime
            providedType.AddMember parseMethod

            (keywords.common.Path, providedType) :: nestedTypes
        | JsonArray(itemType, _) -> buildTypeMapHelper context "Item" name itemType
        // #omit-start
        // #region oneof-case-naming
        | JsonOneOf (_, head, tail) ->
            // Branch i gets "Case<i>", so object branches of one oneOf never share a name.
            head :: tail
            |> List.mapi (fun i branch -> buildTypeMapHelper context $"Case{i + 1}" name branch)
            |> List.concat
        // #endregion
        // #omit-end
        | JsonBoolean _ | JsonInteger _ | JsonNumber _ | JsonString _ -> []

    let private buildTypeMap context (suffix: string) (name: string) (schemaType: JsonSchemaType) : TypeMap =
        buildTypeMapHelper context suffix name schemaType |> Map.ofList


    let run
        (schema: JsonSchema)
        (schemaHashCode: int32)
        (assembly: Assembly)
        (namespaceName: string)
        (typeName: string)
        (rootBaseType: Type)
        (compileFlags: ProviderConfiguration.CompileFlags)
        : ProvidedTypeDefinition =

        let context ={ 
            Assembly = assembly
            NamespaceName = namespaceName
            RootBaseType = rootBaseType
            SchemaHashCode = schemaHashCode
            SchemaString = schema.ToJson()
            CompileFlags = compileFlags 

            // An empty conversion cache to store function calls to convert
            ConversionCache = ConcurrentDictionary()
        }

        // #region root-match
        match parseJsonSchemaStructured schema schema with
        | JsonObject(keywords, _) as schemaType ->
            buildTypeMap context "" typeName schemaType
            |> Map.find keywords.common.Path

        | JsonBoolean _ | JsonInteger _ | JsonNumber _ | JsonString _ as schemaType ->

            // #omit-start
            // Class map contains nested classes inside the type - since a primitive type dont have nested classes this is empty.
            // #omit-end
            let typeMap = Map.empty

            let providedTypeDefinition = createProvidedTypeDefinition context "" typeName

            let conversions = convert context typeMap schemaType
            // #omit-start
            let innerReturnType = conversions.CompileTimeType
            let resultType =
                if context.CompileFlags.SkipRuntimeValidation || conversions.FullyCompilable then
                    innerReturnType
                else
                    typedefof<Result<_,_>>.MakeGenericType(innerReturnType, typeof<string list>)
            // #omit-end
            // #old let returnType = conversions.CompileTimeType

            // #omit-start
            let createMethod = createProvidedCreateMethod context typeMap schemaType resultType
            // #omit-end
            // #old let createMethod = createProvidedCreateMethod context typeMap schemaType returnType
            providedTypeDefinition.AddMember createMethod

            let parseMethod = createProvidedParseMethod context (pathOf schemaType) conversions.CompileTimeType conversions.RuntimeType conversions.ToRuntime
            providedTypeDefinition.AddMember parseMethod

            providedTypeDefinition

        // #omit-start
        | JsonArray _ | JsonOneOf _ as schemaType ->
        // #omit-end
        // #old | JsonArray _ as schemaType ->
            let typeMap = buildTypeMap context "" "" schemaType

            let providedTypeDefinition = createProvidedTypeDefinition context "" typeName

            extractNestedClasses schemaType
            |> List.iter (fun (keywords, _) -> providedTypeDefinition.AddMember typeMap[keywords.common.Path])
            // #elide-start

            let conversions = convert context typeMap schemaType
            let innerReturnType = conversions.CompileTimeType


            let resultType =
                if  context.CompileFlags.SkipRuntimeValidation || conversions.FullyCompilable then
                    // When the conversion is fully compilable, we dont need to use the result wrapper as it dont need validation
                    innerReturnType
                else
                    typedefof<Result<_,_>>.MakeGenericType(innerReturnType, typeof<string list>)

            let createMethod = createProvidedCreateMethod context typeMap schemaType resultType
            providedTypeDefinition.AddMember createMethod

            let parseMethod = createProvidedParseMethod context (pathOf schemaType) conversions.CompileTimeType conversions.RuntimeType conversions.ToRuntime
            providedTypeDefinition.AddMember parseMethod

            // Root array: helpers go directly on the root type. Root oneOf: "Case<i>Array" types.
            match schemaType with
            | JsonArray(innerType, arrayKeywords) -> addArrayHelperMethods context typeMap providedTypeDefinition innerType arrayKeywords
            | _ -> addArrayHelperTypes context typeMap providedTypeDefinition "" schemaType
            // #elide-end

            providedTypeDefinition
            // #endregion
