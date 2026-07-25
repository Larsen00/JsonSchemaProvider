namespace JsonSchemaProvider.DesignTime

module JsonOneOf =
    open System

    // Choice have the limitation of having maximum of 7 generic parameters, so we need to nest them for more than 2 types
    // Todo an idea could be to ship the provider with a tool box that can turn the Choice into a discriminated union
    let rec FSharpOneOfType (innerFSharpTypes: Type list) : Type =
        match innerFSharpTypes with
        | [] -> failwith "OneOf must have at least one type"
        | [singleType] -> singleType
        | firstType  :: rest -> typedefof<Choice<_,_>>.MakeGenericType(firstType, FSharpOneOfType rest)
        

