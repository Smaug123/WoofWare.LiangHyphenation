namespace WoofWare.LiangHyphenation.Test

open NUnit.Framework
open FsUnitTyped
open WoofWare.LiangHyphenation.Construction

[<TestFixture>]
module TestPatternParsing =

    [<Test>]
    let ``Pattern parsing handles basic patterns`` () =
        let parsed = Pattern.parse ".hy3p"
        parsed.Length |> shouldEqual 4
        parsed.[0] |> shouldEqual (struct ('.', 0uy))
        parsed.[1] |> shouldEqual (struct ('h', 0uy))
        parsed.[2] |> shouldEqual (struct ('y', 0uy))
        parsed.[3] |> shouldEqual (struct ('p', 3uy))

    [<Test>]
    let ``Pattern parsing handles leading digits`` () =
        let parsed = Pattern.parse "4ab"
        parsed.Length |> shouldEqual 2
        parsed.[0] |> shouldEqual (struct ('a', 4uy))
        parsed.[1] |> shouldEqual (struct ('b', 0uy))
