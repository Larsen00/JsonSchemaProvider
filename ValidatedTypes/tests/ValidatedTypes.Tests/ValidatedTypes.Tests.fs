module ValidatedTypesTests

open ValidatedTypes.Provided
open NUnit.Framework

type Minimum1 = Number<"{\"minimum\": 1}">

[<Test>]
let ``create succeeds when value meets minimum`` () =
    match Minimum1.create(5.0) with
    | Ok v -> Assert.That(v, Is.EqualTo(5.0))
    | Error errs -> Assert.Fail(String.concat "; " errs)

[<Test>]
let ``create fails when value is below minimum`` () =
    match Minimum1.create(0.0) with
    | Error _ -> Assert.Pass()
    | Ok _ -> Assert.Fail("expected a validation error")

