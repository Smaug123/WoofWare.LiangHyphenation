namespace WoofWare.LiangHyphenation.Test

open System
open NUnit.Framework
open FsCheck
open FsCheck.FSharp
open WoofWare.LiangHyphenation
open WoofWare.LiangHyphenation.Construction
open FsUnitTyped

// ============================================================================
// Naive Reference Implementation (obviously correct, not optimized)
// ============================================================================

module NaiveTrie =
    /// A naive trie node using Map for transitions
    type NaiveTrieNode =
        {
            /// Transitions: char -> (priority at this transition, child node)
            Children: Map<char, byte * NaiveTrieNode>
        }

    let empty: NaiveTrieNode = { Children = Map.empty }

    /// Insert a parsed pattern into the naive trie
    let rec insert (pattern: struct (char * byte) array) (index: int) (node: NaiveTrieNode) : NaiveTrieNode =
        if index >= pattern.Length then
            node
        else
            let struct (c, priority) = pattern.[index]

            let existingPriority, child =
                match Map.tryFind c node.Children with
                | Some(p, child) -> (max p priority, child)
                | None -> (priority, empty)

            let newChild = insert pattern (index + 1) child

            { node with
                Children = Map.add c (existingPriority, newChild) node.Children }

    /// Build a naive trie from patterns
    let build (patterns: string seq) : NaiveTrieNode =
        patterns
        |> Seq.map Pattern.parse
        |> Seq.fold (fun node pattern -> insert pattern 0 node) empty

    /// Try to transition in the naive trie
    let tryTransition (node: NaiveTrieNode) (c: char) : struct (byte * NaiveTrieNode) voption =
        match Map.tryFind c node.Children with
        | Some(priority, child) -> ValueSome(struct (priority, child))
        | None -> ValueNone

    /// Hyphenate using the naive trie (reference implementation)
    let hyphenate (trie: NaiveTrieNode) (word: string) : byte array =
        if word.Length < 2 then
            [||]
        else
            let priorities = Array.zeroCreate<byte> (word.Length - 1)
            let extended = "." + word.ToLowerInvariant() + "."

            for start = 0 to extended.Length - 1 do
                let mutable node = trie
                let mutable pos = start
                let mutable continue' = true

                while continue' && pos < extended.Length do
                    let c = extended.[pos]

                    match tryTransition node c with
                    | ValueSome(struct (priority, nextNode)) ->
                        let interLetterPos = start + (pos - start) - 1

                        if interLetterPos >= 0 && interLetterPos < priorities.Length then
                            if priority > priorities.[interLetterPos] then
                                priorities.[interLetterPos] <- priority

                        node <- nextNode
                        pos <- pos + 1
                    | ValueNone -> continue' <- false

            priorities

// ============================================================================
// FsCheck Generators
// ============================================================================

module Generators =
    /// Generate a valid Liang pattern character (lowercase letter or '.')
    let patternChar: Gen<char> =
        Gen.frequency [ (26, Gen.choose (int 'a', int 'z') |> Gen.map char); (1, Gen.constant '.') ]

    /// Generate a valid priority digit (0-9)
    let priorityDigit: Gen<char> = Gen.choose (int '0', int '9') |> Gen.map char

    /// Generate a valid Liang hyphenation pattern
    /// Format: optional digit, then (char, optional digit)+
    let validPattern: Gen<string> =
        gen {
            let! leadingDigit = Gen.optionOf priorityDigit
            let! charCount = Gen.choose (1, 8)
            let! chars = Gen.listOfLength charCount patternChar
            let! trailingDigits = Gen.listOfLength charCount (Gen.optionOf priorityDigit)

            let sb = System.Text.StringBuilder()
            leadingDigit |> Option.iter (sb.Append >> ignore)

            for i = 0 to chars.Length - 1 do
                sb.Append(chars.[i]) |> ignore
                trailingDigits.[i] |> Option.iter (sb.Append >> ignore)

            return sb.ToString()
        }

    /// Generate a lowercase word for hyphenation testing
    let lowercaseWord: Gen<string> =
        gen {
            let! length = Gen.choose (1, 15)
            let! chars = Gen.listOfLength length (Gen.choose (int 'a', int 'z') |> Gen.map char)
            return String(chars |> List.toArray)
        }

    /// Generate a list of valid patterns
    let patternList: Gen<string list> =
        Gen.listOf validPattern |> Gen.map (List.truncate 50)

// ============================================================================
// Property-Based Tests
// ============================================================================

[<TestFixture>]
module PropertyTests =

    let config =
        Config.QuickThrowOnFailure.WithMaxTest(10000).WithQuietOnSuccess(true).WithParallelRunConfig
            { MaxDegreeOfParallelism = max 1 (Environment.ProcessorCount / 2) }

    [<Test>]
    let ``Packed trie matches naive trie for hyphenation`` () =
        let mutable nonZeroCount = 0
        let mutable zeroCount = 0

        let property (patterns: string list) (word: string) =
            // Build both tries
            let naiveTrie = NaiveTrie.build patterns

            let packedTrie =
                let builder = PackedTrieBuilder()
                builder.AddPatterns(patterns)
                builder.Build()

            // Hyphenate with both
            let naiveResult = NaiveTrie.hyphenate naiveTrie word
            let packedResult = Hyphenation.hyphenate packedTrie word

            // Track distribution: do we ever get non-zero priorities?
            if naiveResult |> Array.exists (fun p -> p > 0uy) then
                nonZeroCount <- nonZeroCount + 1
            else
                zeroCount <- zeroCount + 1

            // They should match (throws on failure for interpretable output)
            packedResult |> shouldEqual naiveResult

        let gen = Gen.zip Generators.patternList Generators.lowercaseWord |> Arb.fromGen

        Check.One(config, Prop.forAll gen (fun (patterns, word) -> property patterns word))

        // Verify we hit interesting cases
        printfn $"Distribution: %d{nonZeroCount} non-zero, %d{zeroCount} zero"

    [<Test>]
    let ``Hyphenation points are within bounds`` () =
        let property (patterns: string list) (word: string) =
            let packedTrie =
                let builder = PackedTrieBuilder()
                builder.AddPatterns(patterns)
                builder.Build()

            let points = Hyphenation.getHyphenationPoints packedTrie word

            // All points should be valid indices
            if word.Length < 2 then
                points.Length |> shouldEqual 0
            else
                for p in points do
                    (p >= 0) |> shouldEqual true
                    (p < word.Length - 1) |> shouldEqual true

        let gen = Gen.zip Generators.patternList Generators.lowercaseWord |> Arb.fromGen

        Check.One(config, Prop.forAll gen (fun (patterns, word) -> property patterns word))

    [<Test>]
    let ``Hyphenation is deterministic`` () =
        let property (patterns: string list) (word: string) =
            let packedTrie =
                let builder = PackedTrieBuilder()
                builder.AddPatterns(patterns)
                builder.Build()

            let result1 = Hyphenation.hyphenate packedTrie word
            let result2 = Hyphenation.hyphenate packedTrie word

            result2 |> shouldEqual result1

        let gen = Gen.zip Generators.patternList Generators.lowercaseWord |> Arb.fromGen

        Check.One(config, Prop.forAll gen (fun (patterns, word) -> property patterns word))

    [<Test>]
    let ``Empty pattern set produces no hyphenation`` () =
        let property (word: string) =
            let packedTrie =
                let builder = PackedTrieBuilder()
                builder.Build()

            let points = Hyphenation.getHyphenationPoints packedTrie word
            points.Length |> shouldEqual 0

        let gen = Generators.lowercaseWord |> Arb.fromGen
        Check.One(config, Prop.forAll gen property)

    [<Test>]
    let ``Short words have no hyphenation points`` () =
        let property (patterns: string list) =
            let packedTrie =
                let builder = PackedTrieBuilder()
                builder.AddPatterns(patterns)
                builder.Build()

            // Words of length 0 or 1 should have no hyphenation points
            let emptyPoints = Hyphenation.getHyphenationPoints packedTrie ""
            let singleCharPoints = Hyphenation.getHyphenationPoints packedTrie "a"

            emptyPoints.Length |> shouldEqual 0
            singleCharPoints.Length |> shouldEqual 0

        let gen = Generators.patternList |> Arb.fromGen
        Check.One(config, Prop.forAll gen property)

    [<Test>]
    let ``Regression: patterns with shared substrings`` () =
        // This test case found a bug where the root state was incorrectly defined
        let patterns = [ "9e5q7z1a8"; "4o6e3e5nw1u0i9e0"; "6c0f1l5xb6o7" ]
        let word = "ulnrqvjd"

        let naiveTrie = NaiveTrie.build patterns

        let packedTrie =
            let builder = PackedTrieBuilder()
            builder.AddPatterns(patterns)
            builder.Build()

        let naiveResult = NaiveTrie.hyphenate naiveTrie word
        let packedResult = Hyphenation.hyphenate packedTrie word

        packedResult |> shouldEqual naiveResult
        // Word has no matching patterns, so all priorities should be 0
        packedResult |> shouldEqual [| 0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy |]

// ============================================================================
// Position Correctness Tests (independent of reference implementation)
// ============================================================================

[<TestFixture>]
module PositionCorrectnessTests =

    let config =
        Config.QuickThrowOnFailure.WithMaxTest(1000).WithQuietOnSuccess(true)

    /// Generate a lowercase letter
    let lowercaseLetter: Gen<char> = Gen.choose (int 'a', int 'z') |> Gen.map char

    /// Generate an odd priority (1, 3, 5, 7, 9) - these create hyphenation points
    let oddPriority: Gen<int> = Gen.elements [ 1; 3; 5; 7; 9 ]

    /// Generate two distinct lowercase letters
    let twoDistinctLetters: Gen<char * char> =
        gen {
            let! a = lowercaseLetter
            let! b = lowercaseLetter |> Gen.filter (fun c -> c <> a)
            return (a, b)
        }

    /// Generate three distinct lowercase letters
    let threeDistinctLetters: Gen<char * char * char> =
        gen {
            let! a = lowercaseLetter
            let! b = lowercaseLetter |> Gen.filter (fun c -> c <> a)
            let! c = lowercaseLetter |> Gen.filter (fun c -> c <> a && c <> b)
            return (a, b, c)
        }

    [<Test>]
    let ``Property 1: Leading priority at word-start should be discarded`` () =
        // Pattern "{p}{x}" means "priority p before letter x"
        // When matching at word start, this priority applies BEFORE the word boundary
        // and should NOT create a hyphenation point inside the word
        let property (x: char, y: char) (p: int) =
            let pattern = $"%d{p}%c{x}"
            let word = $"%c{x}%c{y}"

            let packedTrie =
                let builder = PackedTrieBuilder()
                builder.AddPattern(pattern)
                builder.Build()

            let points = Hyphenation.getHyphenationPoints packedTrie word

            // The priority applies before 'x', which at word-start is the word boundary.
            // This should NOT create any hyphenation point inside the word.
            points.Length |> shouldEqual 0

        let gen = Gen.zip twoDistinctLetters oddPriority |> Arb.fromGen
        Check.One(config, Prop.forAll gen (fun ((x, y), p) -> property (x, y) p))

    [<Test>]
    let ``Property 2: Pattern with word-start marker positions priority correctly`` () =
        // Pattern ".{x}1{y}" should create a hyphenation point between x and y
        // when the word is "{x}{y}" (matched at word start)
        let property (x: char, y: char) =
            let pattern = $".%c{x}1%c{y}"
            let word = $"%c{x}%c{y}"

            let packedTrie =
                let builder = PackedTrieBuilder()
                builder.AddPattern(pattern)
                builder.Build()

            let points = Hyphenation.getHyphenationPoints packedTrie word

            // Should have exactly one hyphenation point at position 0 (between x and y)
            points |> shouldEqual [| 0 |]

        let gen = twoDistinctLetters |> Arb.fromGen
        Check.One(config, Prop.forAll gen property)

    [<Test>]
    let ``Property 3: Mid-word pattern positions priority correctly`` () =
        // Pattern "{a}1{b}{c}" means priority 1 between a and b
        // When matching "x{a}{b}{c}" (where x is a prefix letter), the hyphenation
        // point should be between a and b (position 1), not shifted elsewhere
        let property (prefix: char, a: char, b: char, c: char) =
            let pattern = $"%c{a}1%c{b}%c{c}"
            let word = $"%c{prefix}%c{a}%c{b}%c{c}"

            let packedTrie =
                let builder = PackedTrieBuilder()
                builder.AddPattern(pattern)
                builder.Build()

            let points = Hyphenation.getHyphenationPoints packedTrie word

            // The pattern matches at position 1 (after prefix).
            // Priority 1 is between 'a' and 'b', which is inter-letter position 1 in the word.
            points |> shouldEqual [| 1 |]

        let gen =
            gen {
                let! prefix = lowercaseLetter
                let! (a, b, c) = threeDistinctLetters
                // Ensure prefix is distinct from a to avoid pattern matching at position 0
                let! prefix = lowercaseLetter |> Gen.filter (fun p -> p <> a)
                return (prefix, a, b, c)
            }
            |> Arb.fromGen

        Check.One(config, Prop.forAll gen property)
