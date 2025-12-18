namespace WoofWare.LiangHyphenation.Test

open System.IO
open System.Reflection
open NUnit.Framework
open FsUnitTyped
open WoofWare.LiangHyphenation
open WoofWare.LiangHyphenation.Construction

[<TestFixture>]
module TestSerialization =

    let private embeddedResourcePath (language : KnownLanguage) =
        Path.Combine (
            FileInfo(__SOURCE_DIRECTORY__).Directory.FullName,
            "WoofWare.LiangHyphenation",
            "Data",
            LanguageData.getResourceNameFragment language
        )

    /// Load patterns from an embedded resource text file (one pattern per line).
    let private loadPatternsFromResource (assembly : Assembly) (resourceName : string) : string seq =
        use stream = assembly.GetManifestResourceStream resourceName

        if isNull stream then
            let available = assembly.GetManifestResourceNames () |> String.concat ", "
            failwith $"Embedded resource '%s{resourceName}' not found. Available: %s{available}"

        use reader = new StreamReader (stream)
        let patterns = ResizeArray<string> ()
        let mutable line = reader.ReadLine ()

        while not (isNull line) do
            let trimmed = line.Trim ()

            if trimmed.Length > 0 then
                patterns.Add trimmed

            line <- reader.ReadLine ()

        patterns :> _

    /// Get the patterns and exceptions used to build the packed trie.
    /// Patterns contains hyph-en-gb.hyp.txt and hyph-en-gb.pat.txt, for example.
    /// This function returns a map with key "en-gb" and values {the patterns loaded from pat.txt, the exceptions loaded from hyp.txt}.
    let private getPatternsAndExceptions () : Map<string, string[] * string[]> =
        let assembly = Assembly.GetExecutingAssembly ()
        let prefix = "WoofWare.LiangHyphenation.Test.Patterns.hyph-"
        let patSuffix = ".pat.txt"

        assembly.GetManifestResourceNames ()
        |> Array.filter (fun name -> name.StartsWith prefix && name.EndsWith patSuffix)
        |> Array.map (fun patResource ->
            let langKey =
                patResource.Substring (prefix.Length, patResource.Length - prefix.Length - patSuffix.Length)

            let hypResource = $"{prefix}{langKey}.hyp.txt"
            let patterns = loadPatternsFromResource assembly patResource |> Seq.toArray

            let exceptions =
                if Array.contains hypResource (assembly.GetManifestResourceNames ()) then
                    loadPatternsFromResource assembly hypResource |> Seq.toArray
                else
                    [||]

            langKey, (patterns, exceptions)
        )
        |> Map.ofArray

    /// Build a PackedTrie from the canonical pattern and exception sources for a given language.
    let private buildPackedTrie (language : KnownLanguage) : PackedTrie =
        let fragment = LanguageData.getResourceNameFragment language
        let langKey = fragment.Replace (".bin", "")
        let allData = getPatternsAndExceptions ()

        match Map.tryFind langKey allData with
        | None ->
            let available = allData |> Map.toSeq |> Seq.map fst |> String.concat ", "
            failwith $"No patterns found for language key '%s{langKey}'. Available: %s{available}"
        | Some (patterns, exceptions) ->
            let builder = PackedTrieBuilder ()
            builder.AddPatterns patterns
            builder.AddExceptions exceptions
            builder.Build ()

    /// Compare two priority arrays for equality
    let private prioritiesEqual (a : byte array option) (b : byte array option) : bool =
        match a, b with
        | None, None -> true
        | Some arrA, Some arrB -> arrA.Length = arrB.Length && Array.forall2 (=) arrA arrB
        | _ -> false

    /// Compare two tries for structural equality.
    let private triesEqual (a : PackedTrie) (b : PackedTrie) : bool =
        a.Data.Length = b.Data.Length
        && a.Bases.Length = b.Bases.Length
        && a.AlphabetSize = b.AlphabetSize
        && a.PatternPriorities.Length = b.PatternPriorities.Length
        && Array.forall2 (fun (x : PackedTrieEntry) (y : PackedTrieEntry) -> x.Value = y.Value) a.Data b.Data
        && Array.forall2 (=) a.Bases b.Bases
        && Array.forall2 (=) a.CharMap b.CharMap
        && Array.forall2 prioritiesEqual a.PatternPriorities b.PatternPriorities

    let languageCases = UnionCases.all<KnownLanguage> ()

    [<TestCaseSource(nameof languageCases)>]
    [<Explicit("Run this test to regenerate the embedded resource file")>]
    let ``Regenerate embedded resource`` (language : KnownLanguage) =
        let trie = buildPackedTrie language
        let bytes = PackedTrieSerialization.serialize trie
        let path = embeddedResourcePath language
        File.WriteAllBytes (path, bytes)
        printfn $"Wrote %d{bytes.Length} bytes to %s{path}"

    [<TestCaseSource(nameof languageCases)>]
    let ``Embedded resource matches regenerated trie`` (language : KnownLanguage) =
        let freshTrie = buildPackedTrie language
        let existingTrie = LanguageData.load language

        // Compare the deserialized tries rather than raw bytes, since GZip output
        // is platform-dependent (e.g., the OS byte in the header differs between Linux and macOS).
        if not (triesEqual freshTrie existingTrie) then
            let dataMatch =
                Array.forall2
                    (fun (x : PackedTrieEntry) (y : PackedTrieEntry) -> x.Value = y.Value)
                    freshTrie.Data
                    existingTrie.Data

            let basesMatch = Array.forall2 (=) freshTrie.Bases existingTrie.Bases
            let charMapMatch = Array.forall2 (=) freshTrie.CharMap existingTrie.CharMap

            failwith
                $"Embedded resource for %O{language} is out of date. Data length: %d{freshTrie.Data.Length} vs %d{existingTrie.Data.Length}, \
                  Bases length: %d{freshTrie.Bases.Length} vs %d{existingTrie.Bases.Length}, \
                  AlphabetSize: %d{int freshTrie.AlphabetSize} vs %d{int existingTrie.AlphabetSize}, \
                  Data match: %b{dataMatch}, Bases match: %b{basesMatch}, CharMap match: %b{charMapMatch}. \
                  Run the 'Regenerate embedded resource' test to update."

    [<Test>]
    let ``Serialization round-trips correctly`` () =
        // Build a trie from some test patterns
        let builder = PackedTrieBuilder ()
        builder.AddPatterns [ ".hy3p" ; "4ab1c" ; "1a" ; "tion5" ]
        let original = builder.Build ()

        // Serialize and deserialize
        let bytes = PackedTrieSerialization.serialize original
        let roundTripped = PackedTrieSerialization.deserialize bytes

        // Verify structural equality
        roundTripped.Data.Length |> shouldEqual original.Data.Length
        roundTripped.Bases.Length |> shouldEqual original.Bases.Length
        roundTripped.AlphabetSize |> shouldEqual original.AlphabetSize

        roundTripped.PatternPriorities.Length
        |> shouldEqual original.PatternPriorities.Length

        for i = 0 to original.Data.Length - 1 do
            roundTripped.Data.[i].Value |> shouldEqual original.Data.[i].Value

        for i = 0 to original.Bases.Length - 1 do
            roundTripped.Bases.[i] |> shouldEqual original.Bases.[i]

        for i = 0 to 65535 do
            roundTripped.CharMap.[i] |> shouldEqual original.CharMap.[i]

        for i = 0 to original.PatternPriorities.Length - 1 do
            prioritiesEqual roundTripped.PatternPriorities.[i] original.PatternPriorities.[i]
            |> shouldEqual true

    [<Test>]
    let ``Deserialized trie produces same hyphenation as original`` () =
        // Build a trie from some test patterns
        let builder = PackedTrieBuilder ()
        builder.AddPatterns [ ".hy3p" ; "4ab1c" ; "1a" ; "tion5" ; "ex1am3ple" ]
        let original = builder.Build ()

        // Serialize and deserialize
        let bytes = PackedTrieSerialization.serialize original
        let roundTripped = PackedTrieSerialization.deserialize bytes

        // Test hyphenation with various words
        let testWords = [ "hyphenation" ; "example" ; "action" ; "abc" ; "hello" ]

        for word in testWords do
            let originalResult = Hyphenation.hyphenate original word
            let roundTrippedResult = Hyphenation.hyphenate roundTripped word
            roundTrippedResult |> shouldEqual originalResult

    [<TestCaseSource(nameof languageCases)>]
    let ``Can load language data`` (language : KnownLanguage) =
        let trie = LanguageData.load language
        trie.Data |> shouldNotEqual null
        trie.Bases |> shouldNotEqual null
        trie.CharMap |> shouldNotEqual null
