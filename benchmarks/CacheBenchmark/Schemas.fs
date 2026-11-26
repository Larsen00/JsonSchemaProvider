namespace RefinedJsonSchemaProvider.Benchmarks

// Families of schemas that grow with one size parameter.
module Schemas =

    let private obj (properties: (string * string) list) (required: string list) =
        let props = properties |> List.map (fun (n, s) -> $"\"{n}\": {s}") |> String.concat ", "
        let req = required |> List.map (fun n -> $"\"{n}\"") |> String.concat ", "
        $"{{ \"type\": \"object\", \"properties\": {{ {props} }}, \"required\": [ {req} ] }}"

    let private primitives =
        [ "id", """{ "type": "integer", "minimum": 0, "maximum": 100000 }"""
          "name", """{ "type": "string", "minLength": 1 }"""
          "score", """{ "type": "number", "minimum": 0 }"""
          "active", """{ "type": "boolean" }""" ]

    let private array (items: string) (keywords: string) =
        $"{{ \"type\": \"array\", \"items\": {items}{keywords} }}"

    // A general large schema: a tree of exactly `objects` objects, filled level by level with up to
    // three child objects each (object k has the children 3k+1, 3k+2, 3k+3). Every object also has
    // four primitive properties and a list of small objects, so it accounts for 9 nodes.
    let objectTree (objects: int) : string =
        let rec objectAt k =
            let children =
                [ 1..3 ]
                |> List.map (fun c -> 3 * k + c)
                |> List.filter (fun child -> child < objects)
                |> List.mapi (fun i child -> $"child{i}", objectAt child)
            let tag =
                obj
                    [ "key", """{ "type": "string" }"""
                      "value", """{ "type": "string" }""" ]
                    [ "key" ]
            let tags = "tags", array tag ""
            obj (primitives @ [ tags ] @ children) [ "id"; "name" ]
        objectAt 0

    // Arrays with refined types: one object holding arrays of length n in every shape the
    // provider refines (exact, minimum, maximum, minimum and maximum), with integer and object items.
    let refinedArrays (n: int) : string =
        let item = obj primitives [ "id"; "name" ]
        let bounded = """{ "type": "integer", "minimum": 0, "maximum": 255 }"""
        let exact = $", \"minItems\": {n}, \"maxItems\": {n}"
        let range = $", \"minItems\": {max 1 (n / 2)}, \"maxItems\": {n}"
        obj
            [ "exactInts", array bounded exact
              "exactObjects", array item exact
              "minInts", array bounded $", \"minItems\": {n}"
              "maxInts", array bounded $", \"maxItems\": {n}"
              "maxObjects", array item $", \"maxItems\": {n}"
              "rangeInts", array bounded range
              "matrix", array (array bounded exact) exact ]
            [ "exactInts" ]

    // A chain of objects nested `depth` levels deep, each with two primitive properties.
    let deepChain (depth: int) : string =
        let rec level d =
            let next = if d = depth then [] else [ "next", level (d + 1) ]
            obj ([ List.item 0 primitives; List.item 3 primitives ] @ next) [ "id" ]
        level 0

    // Number of schema nodes, counted as the "type" and "oneOf" keywords in the text.
    let nodeCount (schemaText: string) : int =
        let count (word: string) =
            (schemaText.Length - schemaText.Replace(word, "").Length) / word.Length
        count "\"type\"" + count "\"oneOf\""
