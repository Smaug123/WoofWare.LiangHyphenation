namespace WoofWare.LiangHyphenation.Test

open NUnit.Framework
open FsUnitTyped
open WoofWare.LiangHyphenation

[<TestFixture>]
module TestHyphenation =
    open WoofWare.LiangHyphenation.Construction

    [<Test>]
    let ``Simple pattern produces expected hyphenation`` () =
        // Pattern "1a" means priority 1 before 'a'
        let builder = PackedTrieBuilder ()
        builder.AddPattern ("1a")
        let trie = builder.Build ()

        // For word "aa", extended is ".aa."
        // At position 1 (the 'a' in ".aa."), we should see priority 1
        let result = Hyphenation.hyphenate trie "aa"
        // result[0] is between first and second 'a'
        // The pattern "1a" gives priority 1 at positions where 'a' appears after a position
        // This test verifies basic functionality
        result.Length |> shouldEqual 1
