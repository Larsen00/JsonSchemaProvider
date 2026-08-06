namespace JsonSchemaProvider.DesignTime

module JsonArrayProvidedType =
    open System
    open JsonSchemaProvider

    let FSharpListType (innerStaticType: Type) (arrayKeywords: JsonArray.SpecificKeywords) (compileFlags: ProviderConfiguration.CompileFlags) =
        match arrayKeywords with
        | { MinItems = Some minItems } when compileFlags.CompileMinItems ->

            // This generates a tuple where the if the minItems is n > 0 then the tuple will be T * T * ... * T * List<T> where T is the innerStaticType and there are n occurrences of T in the tuple.
            let listType = typedefof<_ list>.MakeGenericType innerStaticType
            Array.append (Array.create minItems innerStaticType) [| listType |]
            |> Microsoft.FSharp.Reflection.FSharpType.MakeTupleType

        | _ ->
            typedefof<_ list>.MakeGenericType innerStaticType
