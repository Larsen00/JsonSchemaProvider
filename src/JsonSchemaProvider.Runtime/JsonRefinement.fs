namespace JsonSchemaProvider

open FSharp.Data

module JsonRefinement =

    let tryString (jsonVal: JsonValue) : string option =
        match jsonVal with
        | JsonValue.String s -> Some s
        | _ -> None

    let tryInteger (jsonVal: JsonValue) : int option =
        match jsonVal with
        // From doc: For example, 1 and 1.0 are two ways to represent the same value in JSON. JSON Schema considers that value an integer no matter which representation was used.
        | JsonValue.Number n when n = System.Math.Truncate n -> Some (int n)
        | _ -> None

    let tryFloat (jsonVal: JsonValue) : float option =
        match jsonVal with
        | JsonValue.Number n -> Some (float n)
        | _ -> None

    let tryBoolean (jsonVal: JsonValue) : bool option =
        match jsonVal with
        | JsonValue.Boolean b -> Some b
        | _ -> None

    let tryArray (jsonVal: JsonValue) constains : JsonValue array option =
        match jsonVal with
        | JsonValue.Array arr when JsonArray.validateJsonValue arr constains -> Some arr
        | _ -> None

    let tryObject (jsonVal: JsonValue) =
        match jsonVal with
        | JsonValue.Record props -> Some props
        | _ -> None
