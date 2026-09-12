namespace JsonSchemaProvider.DesignTime

module JsonArrayProvidedType =
    open System
    open JsonSchemaProvider

    let rec FSharpListType (innerStaticType: Type) (arrayKeywords: JsonArray.Keywords) (compileFlags: ProviderConfiguration.CompileFlags) =
        match arrayKeywords.specific with
        | { MinItems = Some minItems } when compileFlags.CompileMinItems ->

            let new_keywords = { arrayKeywords with specific = { arrayKeywords.specific with MinItems = None } }

            let listType = FSharpListType innerStaticType new_keywords compileFlags

            // This generates a tuple where the if the minItems is n > 0 then the tuple will be T * T * ... * T * List<T> where T is the innerStaticType and there are n occurrences of T in the tuple.
            Array.append (Array.create minItems innerStaticType) [| listType |]
            |> Microsoft.FSharp.Reflection.FSharpType.MakeTupleType

        | _ ->
            typedefof<_ list>.MakeGenericType innerStaticType
