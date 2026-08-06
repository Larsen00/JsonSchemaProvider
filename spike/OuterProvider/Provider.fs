namespace OuterProvider

open System.Reflection
open ProviderImplementation.ProvidedTypes
open FSharp.Core.CompilerServices
open InnerProvider

/// Spike result (see notes/nested-type-provider-spike.md for the full
/// write-up and the two dead ends that led here):
///
/// A type provider CAN programmatically drive a second, independent
/// provider's ITypeProvider surface, and reuse its generated members -
/// but only if two rules are followed:
///
///  1. The outer provider's ProvidedTypeDefinition must erase directly to
///     a real ground type (here: `string`), never to the inner provider's
///     own still-erased Type. Erasing straight to the inner provider's
///     type fails to resolve at compile time (FS1109).
///  2. Any inner member must be invoked via `ITypeProvider.GetInvokerExpression`,
///     not by hand-building an `Expr` with a `ConstructorInfo`/`MethodInfo`
///     reflected off the inner provider's type. The reflected member's
///     parameter/return types live in the inner provider's own type-identity
///     context, and don't line up with the outer provider's `Expr`s even
///     though they print identically (FS3033, "type mismatch ... System.String
///     vs System.String"). `GetInvokerExpression` delegates back to the
///     owning provider, which knows how to resolve its own types.
///
/// Error handling here uses `Result<string, string>` (a `TryCreate` static
/// method), not exceptions - matching the smart-constructor/tryCreate
/// pattern from notes/ideas/smart-constructor-semantic-types.md rather than
/// the throwing style the first version of this spike used.
[<TypeProvider>]
type OuterProviderProvider(config: TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces(config, addDefaultProbingLocation = true)

    let ns = "OuterProvider"
    let asm = Assembly.GetExecutingAssembly()

    // Constructed directly as an ordinary object - not via the compiler's
    // static-argument syntax. This alone works fine.
    let inner: ITypeProvider = new InnerProviderProvider(config) :> ITypeProvider

    let innerBoundedStringRaw: System.Type =
        inner.GetNamespaces()
        |> Seq.find (fun n -> n.NamespaceName = "InnerProvider")
        |> fun n -> n.GetTypes()
        |> Array.find (fun t -> t.Name = "BoundedString")

    let testProvider = ProvidedTypeDefinition(asm, ns, "Test", Some typeof<obj>)

    do
        testProvider.DefineStaticParameters(
            [ ProvidedStaticParameter("Length", typeof<int>) ],
            fun name args ->
                let length = args.[0] :?> int

                // Drive InnerProvider's ApplyStaticArguments directly - this
                // is a real static-argument application on a *different*
                // provider instance, using a value only known here at
                // generation time (mirrors a value parsed from a JSON Schema).
                let innerApplied: System.Type =
                    inner.ApplyStaticArguments(
                        innerBoundedStringRaw,
                        [| "InnerBoundedString" + string length |],
                        [| box length |]
                    )

                // Rule 1: flatten the erasure to a real ground type.
                let provided = ProvidedTypeDefinition(asm, ns, name, Some typeof<string>)

                let innerTryCreate: MethodInfo =
                    innerApplied.GetMethods() |> Array.find (fun m -> m.Name = "TryCreate")

                // Rule 2: let InnerProvider build the invocation itself.
                let tryCreate =
                    ProvidedMethod(
                        "TryCreate",
                        [ ProvidedParameter("value", typeof<string>) ],
                        typeof<Result<string, string>>,
                        invokeCode = (fun args -> inner.GetInvokerExpression(innerTryCreate, Array.ofList args)),
                        isStatic = true
                    )

                provided.AddMember(tryCreate)

                provided
        )

        this.AddNamespace(ns, [ testProvider ])

[<TypeProviderAssembly>]
do ()
