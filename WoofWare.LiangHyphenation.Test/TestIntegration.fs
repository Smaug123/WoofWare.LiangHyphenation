namespace WoofWare.LiangHyphenation.Test

open FsUnitTyped
open NUnit.Framework
open WoofWare.LiangHyphenation

[<TestFixture>]
module TestIntegration =

    let snapshotCases =
        [ "university", "uni-ver-sity"; "hyphenation", "hyp-he-n-a-t-i-o-n" ]
        |> List.map TestCaseData

    [<TestCaseSource(nameof snapshotCases)>]
    let ``Hyphenation points for some words out-of-the-box`` (word: string, expected: string) =
        let trie = LanguageData.load KnownLanguage.EnGb
        Hyphenation.hyphenateWord trie word "-" |> shouldEqual expected
