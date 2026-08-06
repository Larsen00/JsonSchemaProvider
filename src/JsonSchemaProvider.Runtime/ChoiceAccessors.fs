namespace JsonSchemaProvider

module ChoiceAccessors =
    let getChoice1Of2<'a, 'b> (c: Choice<'a, 'b>) : 'a =
        match c with
        | Choice1Of2 v -> v
        | Choice2Of2 _ -> invalidOp "Expected Choice1Of2"

    let getChoice2Of2<'a, 'b> (c: Choice<'a, 'b>) : 'b =
        match c with
        | Choice1Of2 _ -> invalidOp "Expected Choice2Of2"
        | Choice2Of2 v -> v
