namespace WoofWare.LiangHyphenation.Construction

open WoofWare.LiangHyphenation

/// Build a packed trie from hyphenation patterns.
type PackedTrieBuilder() =
    let trieBuilder = LinkedTrieBuilder()

    /// Add a pattern (e.g., ".hy3p")
    member _.AddPattern(pattern: string) : unit =
        let parsed = Pattern.parse pattern
        trieBuilder.Insert(parsed)

    /// Add multiple patterns
    member this.AddPatterns(patterns: string seq) : unit =
        for p in patterns do
            this.AddPattern(p)

    /// Build the packed trie
    member _.Build() : PackedTrie =
        let root = trieBuilder.Root

        // Apply suffix compression
        let _ = SuffixCompression.compress root

        // Collect alphabet
        let _, charMap = TriePacking.collectNodesAndAlphabet root

        let alphabetSize =
            charMap |> Array.filter (fun x -> x >= 0<alphabetIndex>) |> Array.length

        // Pack the trie
        TriePacking.pack root charMap alphabetSize
