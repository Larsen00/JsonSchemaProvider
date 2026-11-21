namespace RefinedJsonSchemaProvider.DesignTime

open System.IO
open System.Reflection
open FSharp.Core.CompilerServices
open ProviderImplementation.ProvidedTypes
open RefinedJsonSchemaProvider

[<TypeProvider>]
type RefinedJsonSchemaProviderImpl(config: TypeProviderConfig) as this =
    inherit
        TypeProviderForNamespaces(
            config,
            assemblyReplacementMap = [ ("RefinedJsonSchemaProvider.DesignTime", "RefinedJsonSchemaProvider") ],
            addDefaultProbingLocation = true
        )

    let namespaceName = "RefinedJsonSchemaProvider"
    let thisAssembly = Assembly.GetExecutingAssembly()

    let staticParams = [ 
            ProvidedStaticParameter("schema", typeof<string>, "")
            ProvidedStaticParameter("schemaFile", typeof<string>, "")
            ProvidedStaticParameter("skipRuntimeValidation", typeof<bool>, false)
            ProvidedStaticParameter("refinedTypes", typeof<bool>, true)
            ProvidedStaticParameter("requireIntegerBounds", typeof<bool>, false)
            ProvidedStaticParameter("useConversionCache", typeof<bool>, true)
        ]

    let rootBaseType = typeof<NullableJsonValue>

    let jsonSchemaType =
        ProvidedTypeDefinition(thisAssembly, namespaceName, "RefinedJsonSchemaProvider", baseType = Some rootBaseType)

    let instantiate (typeName: string) (parameterValues: obj[]) =
        match parameterValues with
        | [| :? string as schemaSource; :? string as schemaFile; :? bool as skipRuntimeValidation; :? bool as refinedTypes; :? bool as requireIntegerBounds; :? bool as useConversionCache |] ->
            if schemaSource = "" && schemaFile = "" || schemaSource <> "" && schemaFile <> "" then
                failwith "Only one of schema or schemaFile must be set."

            let schemaString =
                if schemaSource <> "" then
                    schemaSource
                else
                    File.ReadAllText(schemaFile)

            let schema = SchemaCache.parseSchema schemaString
            let schemaHashCode = schemaString.GetHashCode()
            let compileUsingKeywordFlags : ProviderConfiguration.CompileFlags = { 
                SkipRuntimeValidation = skipRuntimeValidation
                RefinedTypes = refinedTypes
                RequireIntegerBounds = requireIntegerBounds
                UseConversionCache = useConversionCache
            }

            let providedType =
                TypeProvider.run schema schemaHashCode thisAssembly namespaceName typeName rootBaseType compileUsingKeywordFlags

            providedType
        | paramValues -> failwithf "Unexpected parameter values %A." paramValues

    do
        jsonSchemaType.DefineStaticParameters(parameters = staticParams, instantiationFunction = instantiate)

        this.AddNamespace(namespaceName, [ jsonSchemaType ])
