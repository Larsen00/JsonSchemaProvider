module Program

type Bounded5 = OuterProvider.Test<5>

// Named static args, one with both parameters, one omitting the defaulted one.
type Range0To100 = InnerProvider.Int<Min = 0, Max = 100>
type RangeFrom0 = InnerProvider.Int<Min = 0>

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

    // Test 3: Min=0, Max=100 - both explicit. 150 should be rejected (over Max).
    match Range0To100.TryCreate(150) with
    | Error e -> printfn "Test 3 (Min=0,Max=100 rejects 150): PASS -> %s" e
    | Ok v ->
        failures <- failures + 1
        printfn "Test 3 (Min=0,Max=100 rejects 150): FAIL -> got Ok %d" v

    // Test 4: Min=0 only, Max omitted -> should default to Int32.MaxValue,
    // so the same 150 that Test 3 rejected should now be accepted.
    match RangeFrom0.TryCreate(150) with
    | Ok v -> printfn "Test 4 (Min=0 only accepts 150 via defaulted Max): PASS -> %d" v
    | Error e ->
        failures <- failures + 1
        printfn "Test 4 (Min=0 only accepts 150 via defaulted Max): FAIL -> got Error %s" e

    // Test 5: Min=0 only - still enforces Min (negative should still fail).
    match RangeFrom0.TryCreate(-1) with
    | Error e -> printfn "Test 5 (Min=0 only still rejects -1): PASS -> %s" e
    | Ok v ->
        failures <- failures + 1
        printfn "Test 5 (Min=0 only still rejects -1): FAIL -> got Ok %d" v

    if failures = 0 then
        printfn "\nALL TESTS PASSED - Result-based nested type provider composition, and defaulted static parameters, both work end-to-end."
        0
    else
        printfn "\n%d TEST(S) FAILED" failures
        1
