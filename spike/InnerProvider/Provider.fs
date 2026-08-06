namespace InnerProvider

open System.Reflection
open ProviderImplementation.ProvidedTypes
open FSharp.Core.CompilerServices

[<AutoOpen>]
module Utilities =
    let tryCreateBoundedString (length: int) (value: string) : Result<string, string> =
        if value.Length > length then
            Error(sprintf "InnerProvider: value exceeded bound: '%s' (len %d) > %d" value value.Length length)
        else
            Ok value

/// Minimal standalone type provider, structurally modelled on the
/// (unrelated, prior-art) ConstrainedTypes project's BoundedString<Length>.
/// Spike-only: proves whether a *second* provider can programmatically
/// drive this provider's ITypeProvider surface. See OuterProvider.
[<TypeProvider>]
type InnerProviderProvider(config: TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces(config, addDefaultProbingLocation = true)

    let ns = "InnerProvider"
    let asm = Assembly.GetExecutingAssembly()

    let boundedStringProvider =
        ProvidedTypeDefinition(asm, ns, "BoundedString", Some typeof<string>)

    do
        boundedStringProvider.DefineStaticParameters(
            [ ProvidedStaticParameter("Length", typeof<int>) ],
            fun name args ->
                let length = args.[0] :?> int
                let provided = ProvidedTypeDefinition(asm, ns, name, Some typeof<string>)

                let tryCreate =
                    ProvidedMethod(
                        "TryCreate",
                        [ ProvidedParameter("value", typeof<string>) ],
                        typeof<Result<string, string>>,
                        invokeCode = (fun args -> <@@ tryCreateBoundedString length (%%args.[0]: string) @@>),
                        isStatic = true
                    )

                provided.AddMember(tryCreate)

                provided
        )

        this.AddNamespace(ns, [ boundedStringProvider ])

[<TypeProviderAssembly>]
do ()
