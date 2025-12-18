namespace WoofWare.LiangHyphenation.Test

open NUnit.Framework
open ApiSurface

[<TestFixture>]
module TestSurface =
    let assembly = typeof<WoofWare.LiangHyphenation.PackedTrie>.Assembly

    [<Test; Explicit "still iterating">]
    let ``Ensure API surface has not been modified`` () = ApiSurface.assertIdentical assembly

    [<Test; Explicit>]
    let ``Update API surface`` () =
        ApiSurface.writeAssemblyBaseline assembly

    [<Test; Explicit "still iterating">]
    let ``Ensure public API is fully documented`` () =
        DocCoverage.assertFullyDocumented assembly

    [<Test; Explicit "Not yet published">]
    // https://github.com/nunit/nunit3-vs-adapter/issues/876
    let ``EnsureVersionIsMonotonic`` () =
        MonotonicVersion.validate assembly "WoofWare.LiangHyphenation"
