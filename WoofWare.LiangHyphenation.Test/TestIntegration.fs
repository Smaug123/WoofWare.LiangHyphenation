namespace WoofWare.LiangHyphenation.Test

open FsUnitTyped
open NUnit.Framework
open WoofWare.LiangHyphenation

[<TestFixture>]
module TestIntegration =

    let snapshotCases =
        // "university" uses the exception from hyph-en-gb.hyp.txt
        // "hyphenation" uses pattern-based hyphenation (no exception defined)
        [ "university", "uni-ver-sity" ; "hyphenation", "hy-phen-a-tion" ]
        |> List.map TestCaseData

    [<TestCaseSource(nameof snapshotCases)>]
    let ``Hyphenation points for some words out-of-the-box`` (word : string, expected : string) =
        let trie = LanguageData.load KnownLanguage.EnGb
        Hyphenation.hyphenateWord trie word "-" |> shouldEqual expected

    /// This test documents the semantics of the priority values returned by `Hyphenation.hyphenate`.
    /// These values are used as examples in the README.
    [<Test>]
    let ``Priority array semantics for hyphenation`` () =
        let trie = LanguageData.load KnownLanguage.EnGb

        // The word "hyphenation" gives us a good example of the priority semantics.
        // Each position in the array represents the inter-letter gap at that index.
        let priorities = Hyphenation.hyphenate trie "hyphenation"

        // The priority array has length (word.Length - 1), one value per inter-letter position.
        priorities
        |> shouldEqual [| 2uy ; 3uy ; 0uy ; 4uy ; 4uy ; 1uy ; 1uy ; 2uy ; 4uy ; 2uy |]

        // Odd values indicate valid hyphenation points; even values suppress hyphenation.
        // Position 1 (y-p): priority 3 (odd) → hy-phenation
        // Position 5 (n-a): priority 1 (odd) → hyphen-ation
        // Position 6 (a-t): priority 1 (odd) → hyphena-tion
        let points = Hyphenation.getHyphenationPoints trie "hyphenation"
        points |> shouldEqual [| 1 ; 5 ; 6 |]
