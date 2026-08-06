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

    let tryCreateRangedInt (min: int) (max: int) (value: int) : Result<int, string> =
        if value < min || value > max then
            Error(sprintf "InnerProvider: value out of range: %d not in [%d, %d]" value min max)
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

    // Spike addition: does ProvidedStaticParameter support a default value,
    // so `Int<Min=0, Max=100>` AND `Int<Min=0>` (Max omitted) both work?
    let rangedIntProvider = ProvidedTypeDefinition(asm, ns, "Int", Some typeof<int>)

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

        rangedIntProvider.DefineStaticParameters(
            [ ProvidedStaticParameter("Min", typeof<int>)
              ProvidedStaticParameter("Max", typeof<int>, parameterDefaultValue = System.Int32.MaxValue) ],
            fun name args ->
                let min = args.[0] :?> int
                let max = args.[1] :?> int
                let provided = ProvidedTypeDefinition(asm, ns, name, Some typeof<int>)

                let tryCreate =
                    ProvidedMethod(
                        "TryCreate",
                        [ ProvidedParameter("value", typeof<int>) ],
                        typeof<Result<int, string>>,
                        invokeCode = (fun args -> <@@ tryCreateRangedInt min max (%%args.[0]: int) @@>),
                        isStatic = true
                    )

                provided.AddMember(tryCreate)

                provided
        )

        this.AddNamespace(ns, [ boundedStringProvider; rangedIntProvider ])

[<TypeProviderAssembly>]
do ()
