namespace WoofWare.LiangHyphenation.Construction

open WoofWare.LiangHyphenation

/// A mutable data structure which defines the patterns and exceptions from which a PackedTrie is constructed.
type PackedTrieBuilder () =
    let trieBuilder = LinkedTrieBuilder ()

    /// Add a pattern (e.g., ".hy3p").
    /// Time: O(m * s) where m is pattern length and s is max sibling count at each trie level.
    /// In the worst case s is bounded by alphabet size, but typically s is small because patterns share prefixes.
    /// This is fast; the expensive operation is Build.
    member _.AddPattern (pattern : string) : unit =
        let parsed = Pattern.parse pattern
        trieBuilder.Insert parsed

    /// Add multiple patterns.
    /// Time: O(P * s) where P is total length of all patterns and s is max sibling count.
    /// This is fast; the expensive operation is Build.
    member this.AddPatterns (patterns : string seq) : unit =
        for p in patterns do
            this.AddPattern p

    /// Add an exception (e.g., "uni-ver-sity").
    /// Exceptions override pattern-based hyphenation for specific words.
    /// Time: O(m * s) where m is exception length and s is max sibling count. This is fast.
    member this.AddException (exception' : string) : unit =
        let pattern = Pattern.exceptionToPattern exception'
        this.AddPattern pattern

    /// Add multiple exceptions.
    /// Time: O(E * s) where E is total length of all exceptions and s is max sibling count. This is fast.
    member this.AddExceptions (exceptions : string seq) : unit =
        for e in exceptions do
            this.AddException e

    /// Build the packed trie.
    /// Time: O(N * A) where N is total unique nodes and A is alphabet size.
    /// This is the expensive operation: it performs suffix compression, alphabet collection, and first-fit packing.
    member _.Build () : PackedTrie =
        let root = trieBuilder.Root

        // Apply suffix compression
        let _ = SuffixCompression.compress root

        // Collect alphabet
        let _, charMap = TriePacking.collectNodesAndAlphabet root

        let alphabetSize =
            charMap |> Array.filter (fun x -> x >= 0<alphabetIndex>) |> Array.length

        // Pack the trie
        TriePacking.pack root charMap alphabetSize
