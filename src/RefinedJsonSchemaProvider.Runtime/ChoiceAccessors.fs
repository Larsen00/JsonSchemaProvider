namespace RefinedJsonSchemaProvider

// Called by the generated oneOf ToJson, which has already tested which constructor it has.
module ChoiceAccessors =
    // The value inside Choice1Of2.
    let getChoice1Of2<'a, 'b> (c: Choice<'a, 'b>) : 'a =
        match c with
        | Choice1Of2 v -> v
        | Choice2Of2 _ -> invalidOp "Expected Choice1Of2"

    // The value inside Choice2Of2.
    let getChoice2Of2<'a, 'b> (c: Choice<'a, 'b>) : 'b =
        match c with
        | Choice1Of2 _ -> invalidOp "Expected Choice2Of2"
        | Choice2Of2 v -> v
