namespace JsonSchemaProvider.Tests

[<AutoOpen>]
module TestHelpers =
    let expectEqual actual expected =
        if actual <> expected then
            failwithf "Expected: %A\nActual:   %A" expected actual

    let expectOk result =
        match result with
        | Ok value -> value
        | Error errors -> failwithf "Expected Ok, got Error %A" errors

    let expectError result =
        match result with
        | Error _ -> ()
        | Ok value -> failwithf "Expected Error, got Ok %A" value
