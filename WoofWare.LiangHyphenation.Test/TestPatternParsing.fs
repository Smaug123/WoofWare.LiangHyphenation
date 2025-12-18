namespace WoofWare.LiangHyphenation.Test

open NUnit.Framework
open FsUnitTyped
open WoofWare.LiangHyphenation.Construction

[<TestFixture>]
module TestPatternParsing =

    [<Test>]
    let ``Pattern parsing handles basic patterns`` () =
        // ".hy3p" has 4 chars and 5 priority positions
        // Priorities: [0 before '.', 0 before 'h', 0 before 'y', 3 before 'p', 0 after 'p']
        let parsed = Pattern.parse ".hy3p"
        parsed.Chars |> shouldEqual [| '.' ; 'h' ; 'y' ; 'p' |]
        parsed.Priorities |> shouldEqual [| 0uy ; 0uy ; 0uy ; 3uy ; 0uy |]

    [<Test>]
    let ``Pattern parsing handles leading digits`` () =
        // "4ab" has 2 chars and 3 priority positions
        // Priorities: [4 before 'a', 0 before 'b', 0 after 'b']
        let parsed = Pattern.parse "4ab"
        parsed.Chars |> shouldEqual [| 'a' ; 'b' |]
        parsed.Priorities |> shouldEqual [| 4uy ; 0uy ; 0uy |]

    [<Test>]
    let ``Pattern parsing handles trailing digits`` () =
        // ".ace4" has 4 chars and 5 priority positions
        // Priorities: [0 before '.', 0 before 'a', 0 before 'c', 0 before 'e', 4 after 'e']
        let parsed = Pattern.parse ".ace4"
        parsed.Chars |> shouldEqual [| '.' ; 'a' ; 'c' ; 'e' |]
        parsed.Priorities |> shouldEqual [| 0uy ; 0uy ; 0uy ; 0uy ; 4uy |]

    [<Test>]
    let ``Pattern parsing handles multiple digits`` () =
        // "1a2b3c4" has 3 chars and 4 priority positions
        let parsed = Pattern.parse "1a2b3c4"
        parsed.Chars |> shouldEqual [| 'a' ; 'b' ; 'c' |]
        parsed.Priorities |> shouldEqual [| 1uy ; 2uy ; 3uy ; 4uy |]
