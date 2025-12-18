namespace WoofWare.LiangHyphenation.Test

open System.IO
open System.Reflection
open NUnit.Framework
open FsUnitTyped
open WoofWare.LiangHyphenation
open WoofWare.LiangHyphenation.Construction

[<TestFixture>]
module TestSerialization =

    let private embeddedResourceName = LanguageData.getResourceName KnownLanguage.EnGb

    let private patternsResourceName =
        "WoofWare.LiangHyphenation.Test.Patterns.hyph-en-gb.pat.txt"

    let private exceptionsResourceName =
        "WoofWare.LiangHyphenation.Test.Patterns.hyph-en-gb.hyp.txt"

    let private embeddedResourcePath =
        Path.Combine (
            FileInfo(__SOURCE_DIRECTORY__).Directory.FullName,
            "WoofWare.LiangHyphenation",
            "Data",
            LanguageData.getResourceNameFragment KnownLanguage.EnGb
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

    /// Get the patterns used to build the packed trie.
    let private getPatterns () : string seq =
        let assembly = Assembly.GetExecutingAssembly ()
        loadPatternsFromResource assembly patternsResourceName

    /// Get the exceptions used to build the packed trie.
    let private getExceptions () : string seq =
        let assembly = Assembly.GetExecutingAssembly ()
        loadPatternsFromResource assembly exceptionsResourceName

    /// Build a PackedTrie from the canonical pattern and exception sources.
    let private buildPackedTrie () : PackedTrie =
        let patterns = getPatterns ()
        let exceptions = getExceptions ()
        let builder = PackedTrieBuilder ()
        builder.AddPatterns patterns
        builder.AddExceptions exceptions
        builder.Build ()

    [<Test>]
    [<Explicit("Run this test to regenerate the embedded resource file")>]
    let ``Regenerate embedded resource`` () =
        let trie = buildPackedTrie ()
        let bytes = PackedTrieSerialization.serialize trie
        File.WriteAllBytes (embeddedResourcePath, bytes)
        printfn $"Wrote %d{bytes.Length} bytes to %s{embeddedResourcePath}"

    [<Test>]
    let ``Embedded resource matches regenerated trie`` () =
        let trie = buildPackedTrie ()
        let freshBytes = PackedTrieSerialization.serialize trie

        let existingBytes = File.ReadAllBytes embeddedResourcePath

        if freshBytes <> existingBytes then
            if freshBytes.Length <> existingBytes.Length then
                failwith
                    $"Embedded resource is out of date. Expected %d{freshBytes.Length} bytes but file has %d{existingBytes.Length} bytes. Run the 'Regenerate embedded resource' test to update."
            let firstDifference =
                seq { 0 .. freshBytes.Length - 1 }
                |> Seq.find (fun i -> freshBytes.[i] <> existingBytes.[i])
            failwith $"Embedded resource differs, first at index %i{firstDifference}"

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

        for i = 0 to original.Data.Length - 1 do
            roundTripped.Data.[i].Value |> shouldEqual original.Data.[i].Value

        for i = 0 to original.Bases.Length - 1 do
            roundTripped.Bases.[i] |> shouldEqual original.Bases.[i]

        for i = 0 to 65535 do
            roundTripped.CharMap.[i] |> shouldEqual original.CharMap.[i]

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

    let cases = UnionCases.all<KnownLanguage> ()

    [<TestCaseSource(nameof cases)>]
    let ``Can load language data`` (language : KnownLanguage) =
        let trie = LanguageData.load language
        trie.Data |> shouldNotEqual null
        trie.Bases |> shouldNotEqual null
        trie.CharMap |> shouldNotEqual null
