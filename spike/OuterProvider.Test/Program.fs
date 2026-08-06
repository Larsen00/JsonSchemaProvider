module Program

type Bounded5 = OuterProvider.Test<5>

[<EntryPoint>]
let main _ =
    let mutable failures = 0

    // Test 1: a valid value goes through the outer provider's generated
    // TryCreate, which calls into InnerProvider's generated TryCreate, and
    // comes back as Ok - no exceptions anywhere.
    match Bounded5.TryCreate("hi") with
    | Ok v -> printfn "Test 1 (valid -> Ok): PASS -> %s" v
    | Error e ->
        failures <- failures + 1
        printfn "Test 1 (valid -> Ok): FAIL -> got Error %s" e

    // Test 2: an invalid value comes back as Error, with the message
    // originating from INSIDE InnerProvider's own validation code - proving
    // the outer provider's generated method really calls through to the
    // inner provider's real logic, not a stub.
    match Bounded5.TryCreate("this string is definitely longer than five characters") with
    | Error e when e.Contains("InnerProvider:") -> printfn "Test 2 (invalid -> Error): PASS -> %s" e
    | Error e ->
        failures <- failures + 1
        printfn "Test 2 (invalid -> Error): FAIL -> wrong error message: %s" e
    | Ok v ->
        failures <- failures + 1
        printfn "Test 2 (invalid -> Error): FAIL -> got Ok %s" v

    if failures = 0 then
        printfn "\nALL TESTS PASSED - Result-based nested type provider composition works end-to-end."
        0
    else
        printfn "\n%d TEST(S) FAILED" failures
        1
