module ValidatedTypesImplementation

open System
open System.Collections.Generic
open System.IO
open System.Reflection
open FSharp.Quotations
open FSharp.Core.CompilerServices
open MyNamespace
open ProviderImplementation
open ProviderImplementation.ProvidedTypes
open ValidatedTypes.DesignTime
// open ValidatedTypes.DesignTime.Number


// Put any utility helpers here
[<AutoOpen>]
module internal Helpers =
    let x = 1

[<TypeProvider>]
type BasicErasingProvider (config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces (config, addDefaultProbingLocation=true)

    let namespaceName  = "ValidatedTypes.Provided"
    let thisAssembly  = Assembly.GetExecutingAssembly()

    let createType typeName (constrains: string) =

        match Number.parse constrains with
        | Error errors ->
            let errorMessages = String.Join("; ", errors)
            raise (ArgumentException errorMessages)

        | Ok _ ->
            // The Ok case above is only a design-time validation gate - it
            // catches malformed constraint JSON as a compile-time error.
            // The parsed NumberValidations value itself can't be embedded
            // into the quotation below (it's a design-time-only instance of
            // an erased provided type), so the quotation re-parses the raw
            // string at runtime instead.
            let t = ProvidedTypeDefinition(thisAssembly, namespaceName, typeName, Some typeof<obj>)

            let tryCreate =
                ProvidedMethod(
                    methodName = "create",
                    parameters = [ ProvidedParameter("value", typeof<float>)],
                    returnType = typeof<Result<float, list<string>>>,
                    isStatic = true,
                    invokeCode = (fun args ->

                        <@@
                            let inputValue = %%args.[0] : float
                            Number.create inputValue constrains
                        @@>
                    )
                )

            t.AddMember tryCreate


            t
                
    do
        // Define the provided type "Number"
        let NumberType = ProvidedTypeDefinition(thisAssembly, namespaceName, "Number", Some typeof<obj>)

        // Define the static parameters for the "Number" type
        NumberType.DefineStaticParameters(
            [
                ProvidedStaticParameter("Json",typeof<string>, parameterDefaultValue = "{}")
            ],
            fun typeName args -> createType typeName (args.[0] :?> string))

        this.AddNamespace(namespaceName, [NumberType])

