namespace JsonSchemaProvider.DesignTime

module JsonArrayProvidedType =
    open System
    open JsonSchemaProvider


    // Trying to build the type as recursively
    // Note: This functions assumes that minItems and maxitems can never be 0 - as a 0 value is converted to None in the schemaConversion.fs
    let rec FSharpListType (innerStaticType: Type) (arrayKeywords: JsonArray.Keywords) (compileFlags: ProviderConfiguration.CompileFlags) =
        match arrayKeywords.specific with

        // When minItems is greater than maxItems, it is an invalid schema.
        | { MinItems = Some minItems; MaxItems = Some maxItems } when minItems > maxItems -> failwith  "MinItems cannot be greater than MaxItems - Please check your schema."

        // When maxItems equals minItems then we create a fixed-size tuple with exactly minItems elements of the inner type.
        | { MinItems = Some minItems; MaxItems = Some maxItems } when minItems = maxItems ->
            Array.create minItems innerStaticType
            |> Microsoft.FSharp.Reflection.FSharpType.MakeTupleType

        | { MinItems = Some minItems; MaxItems = maxItems } when compileFlags.CompileMinItems ->

            // When genrating the type and both minItems and maxItems are specified, we create a tuple with exactly minItems elements of the inner type followed by a recursively defined list for the remaining elements.
            let new_max = match maxItems with Some m -> Some (m - minItems) | None -> None
            let new_keywords = { arrayKeywords with specific = { arrayKeywords.specific with MinItems = None; MaxItems = new_max }}

            let listType = FSharpListType innerStaticType new_keywords compileFlags

            // This generates a tuple where the if the minItems is n > 0 then the tuple will be T * T * ... * T * List<T> where T is the innerStaticType and there are n occurrences of T in the tuple.
            Array.append (Array.create minItems innerStaticType) [| listType |]
            |> Microsoft.FSharp.Reflection.FSharpType.MakeTupleType

        // When maxItems is specified as 1, it the same as having an optional single element of the inner type.
        | { MaxItems = Some 1 } -> typedefof<option<_>>.MakeGenericType[| innerStaticType |]

        // When maxItems is specified as greater than 1, we create an optional tuple where the first element is the inner type and the second element is a recursively defined list for the remaining elements.
        | { MaxItems = Some maxItems } -> 
            let new_keywords = { arrayKeywords with specific = { arrayKeywords.specific with MaxItems = Some (maxItems - 1) }}
            let tailType = FSharpListType innerStaticType new_keywords compileFlags
            
            // Pair type is: (innerStaticType * tailType)
            let pairType = Microsoft.FSharp.Reflection.FSharpType.MakeTupleType [| innerStaticType; tailType |]
            typedefof<option<_>>.MakeGenericType[| pairType |]


        // Default case: when no specific minItems or maxItems constraints are provided, we use a standard F# list.
        | _ -> typedefof<_ list>.MakeGenericType innerStaticType
