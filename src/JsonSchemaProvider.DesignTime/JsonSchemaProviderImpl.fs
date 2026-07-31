namespace JsonSchemaProvider.DesignTime

open System.IO
open System.Reflection
open FSharp.Core.CompilerServices
open ProviderImplementation.ProvidedTypes
open JsonSchemaProvider

[<TypeProvider>]
type JsonSchemaProviderImpl(config: TypeProviderConfig) as this =
    inherit
        TypeProviderForNamespaces(
            config,
            assemblyReplacementMap = [ ("JsonSchemaProvider.DesignTime", "JsonSchemaProvider") ],
            addDefaultProbingLocation = true
        )

    let namespaceName = "JsonSchemaProvider"
    let thisAssembly = Assembly.GetExecutingAssembly()

    let staticParams = [ 
            ProvidedStaticParameter("schema", typeof<string>, "")
            ProvidedStaticParameter("schemaFile", typeof<string>, "")
            ProvidedStaticParameter("compileMinItems", typeof<bool>, false)
        ]

    let runtimeType = typeof<NullableJsonValue>

    let jsonSchemaType =
        ProvidedTypeDefinition(thisAssembly, namespaceName, "JsonSchemaProvider", baseType = Some runtimeType)

    let instantiate (typeName: string) (parameterValues: obj[]) =
        match parameterValues with
        | [| :? string as schemaSource; :? string as schemaFile; :? bool as compileMinItems |] ->
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
                CompileMinItems = compileMinItems 
            }

            let providedType =
                TypeProvider.run schema schemaHashCode thisAssembly namespaceName typeName runtimeType compileUsingKeywordFlags

            providedType
        | paramValues -> failwithf "Unexpected parameter values %A." paramValues

    do
        jsonSchemaType.DefineStaticParameters(parameters = staticParams, instantiationFunction = instantiate)

        this.AddNamespace(namespaceName, [ jsonSchemaType ])
