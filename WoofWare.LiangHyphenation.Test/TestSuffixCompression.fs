namespace WoofWare.LiangHyphenation.Test

open System.Collections.Generic
open System.Runtime.CompilerServices
open NUnit.Framework
open FsCheck
open FsCheck.FSharp
open WoofWare.LiangHyphenation
open WoofWare.LiangHyphenation.Construction
open FsUnitTyped

/// Tests for suffix compression functionality.
[<TestFixture>]
module TestSuffixCompression =

    /// Count unique nodes in a linked trie using reference equality.
    let private countUniqueNodes (root : LinkedTrieNode option) : int =
        let seen =
            HashSet<LinkedTrieNode>
                { new IEqualityComparer<LinkedTrieNode> with
                    member _.Equals (x, y) = Object.referenceEquals x y
                    member _.GetHashCode x = RuntimeHelpers.GetHashCode x
                }

        let rec visit (node : LinkedTrieNode option) =
            match node with
            | None -> ()
            | Some n ->
                if seen.Add n then
                    visit n.Left
                    visit n.Right

        visit root
        seen.Count

    /// Build a linked trie from patterns without compressing.
    let private buildUncompressed (patterns : string seq) : LinkedTrieNode option =
        let builder = LinkedTrieBuilder ()

        for p in patterns do
            let parsed = Pattern.parse p
            builder.Insert parsed

        builder.Root

    /// Query the linked trie for pattern priorities at a given position in extended word.
    /// Returns the priority vector if a pattern ends at the current position, None otherwise.
    let private queryLinkedTrie (root : LinkedTrieNode option) (word : string) : byte array =
        if word.Length < 2 then
            [||]
        else
            let priorities = Array.zeroCreate<byte> (word.Length - 1)
            let extended = "." + word.ToLowerInvariant () + "."

            /// Find child with given char in the binary tree (Left = sibling, Right = child)
            let rec findChild (c : char) (node : LinkedTrieNode option) : LinkedTrieNode option =
                match node with
                | None -> None
                | Some n -> if n.Char = c then Some n else findChild c n.Left

            for start = 0 to extended.Length - 1 do
                let mutable current = root
                let mutable pos = start
                let mutable continue' = true

                while continue' && pos < extended.Length do
                    let c = extended.[pos]

                    match current |> Option.bind (fun n -> findChild c n.Right) with
                    | Some nextNode ->
                        // Check if pattern ends here
                        match nextNode.PatternPriorities with
                        | Some patternPriorities ->
                            for i = 0 to patternPriorities.Length - 1 do
                                let origPos = start + i - 2

                                if origPos >= 0 && origPos < priorities.Length then
                                    if patternPriorities.[i] > priorities.[origPos] then
                                        priorities.[origPos] <- patternPriorities.[i]
                        | None -> ()

                        current <- Some nextNode
                        pos <- pos + 1
                    | None -> continue' <- false

            priorities

    // ========================================================================
    // Property Tests
    // ========================================================================

    [<Test>]
    let ``Compression preserves trie semantics`` () =
        let property (patterns : string list) (word : string) =
            // Build uncompressed trie
            let uncompressedRoot = buildUncompressed patterns

            // Query before compression
            let beforeResult = queryLinkedTrie uncompressedRoot word

            // Apply compression (mutates in place, but returns mapping)
            let _ = SuffixCompression.compress uncompressedRoot

            // Query after compression
            let afterResult = queryLinkedTrie uncompressedRoot word

            // Semantics should be identical
            afterResult |> shouldEqual beforeResult

        let gen = Gen.zip Generators.patternList Generators.lowercaseWord |> Arb.fromGen
        Check.One (FsCheckConfig.config, Prop.forAll gen (fun (patterns, word) -> property patterns word))

    [<Test>]
    let ``Compression reduces node count for patterns with shared suffixes`` () =
        // Patterns that share a common suffix "tion"
        let patterns = [ "1tion" ; "a1tion" ; "e1tion" ; "i1tion" ; "o1tion" ; "u1tion" ]

        let root = buildUncompressed patterns
        let beforeCount = countUniqueNodes root

        let _ = SuffixCompression.compress root
        let afterCount = countUniqueNodes root

        // Compression should reduce node count since "tion" subtrie is shared
        afterCount |> shouldBeSmallerThan beforeCount

        // Specifically, we should save at least 4 nodes (t, i, o, n shared across 5 suffixes)
        // Before: each of a,e,i,o,u has its own t->i->o->n chain = 5 * 4 = 20 nodes for suffixes
        // After: one shared t->i->o->n chain = 4 nodes, plus 5 prefix nodes = 9 nodes
        // But root is shared too, so the math is a bit different. Just verify meaningful reduction.
        let reduction = beforeCount - afterCount
        reduction |> shouldBeGreaterThan 10

    [<Test>]
    let ``Compression merges identical leaf nodes`` () =
        // Patterns with the same ending character and no children should merge
        let patterns = [ "ab1" ; "cb1" ; "db1" ]

        let root = buildUncompressed patterns
        let beforeCount = countUniqueNodes root

        let _ = SuffixCompression.compress root
        let afterCount = countUniqueNodes root

        // The 'b' nodes at the end are identical (same char, same priorities, no children)
        // so they should be merged
        afterCount |> shouldBeSmallerThan beforeCount

    [<Test>]
    let ``Compression is idempotent`` () =
        let property (patterns : string list) =
            let root = buildUncompressed patterns
            let _ = SuffixCompression.compress root
            let afterFirstCount = countUniqueNodes root

            // Compress again
            let _ = SuffixCompression.compress root
            let afterSecondCount = countUniqueNodes root

            // Should be the same - compression is idempotent
            afterSecondCount |> shouldEqual afterFirstCount

        let gen = Generators.patternList |> Arb.fromGen
        Check.One (FsCheckConfig.config, Prop.forAll gen property)

    [<Test>]
    let ``Compressed trie produces same hyphenation as naive reference`` () =
        // This is the key semantic property: after compression, the packed trie
        // should still produce identical results to the naive (uncompressed) reference.
        let property (patterns : string list) (word : string) =
            let naiveTrie = NaiveTrie.build patterns

            let packedTrie =
                let builder = PackedTrieBuilder ()
                builder.AddPatterns patterns
                builder.Build () // This applies compression internally

            let naiveResult = NaiveTrie.hyphenate naiveTrie word
            let packedResult = Hyphenation.hyphenate packedTrie word

            packedResult |> shouldEqual naiveResult

        let gen = Gen.zip Generators.patternList Generators.lowercaseWord |> Arb.fromGen
        Check.One (FsCheckConfig.config, Prop.forAll gen (fun (patterns, word) -> property patterns word))

    [<Test>]
    let ``Empty trie compression is a no-op`` () =
        let root = buildUncompressed []
        let beforeCount = countUniqueNodes root

        let _ = SuffixCompression.compress root
        let afterCount = countUniqueNodes root

        // Root node only (the '\000' sentinel)
        beforeCount |> shouldEqual 1
        afterCount |> shouldEqual 1

    [<Test>]
    let ``Single pattern compression preserves pattern`` () =
        let patterns = [ ".hy3ph" ]
        let word = "hyphen"

        let root = buildUncompressed patterns
        let beforeResult = queryLinkedTrie root word

        let _ = SuffixCompression.compress root
        let afterResult = queryLinkedTrie root word

        afterResult |> shouldEqual beforeResult
        // Verify the pattern actually applies
        afterResult |> shouldNotEqual (Array.zeroCreate (word.Length - 1))
