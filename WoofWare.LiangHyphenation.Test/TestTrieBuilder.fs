module WoofWare.LiangHyphenation.Test.TestTrieBuilder

open NUnit.Framework
open FsUnitTyped
open WoofWare.LiangHyphenation
open WoofWare.LiangHyphenation.Construction

[<TestFixture>]
module TestTrieBuilder =

    [<Test>]
    let ``Empty trie can be built and queried`` () =
        let builder = PackedTrieBuilder()
        let trie = builder.Build()
        let result = Hyphenation.hyphenate trie "hello"
        result.Length |> shouldEqual 4
        result |> shouldEqual [| 0uy; 0uy; 0uy; 0uy |]
