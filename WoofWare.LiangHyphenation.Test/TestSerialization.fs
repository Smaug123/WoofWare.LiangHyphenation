namespace WoofWare.LiangHyphenation.Test

open System.IO
open System.IO.Compression
open System.Reflection
open NUnit.Framework
open FsCheck
open FsCheck.FSharp
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

    /// GZip-compress raw bytes the same way `PackedTrieSerialization.serialize` does, so we can
    /// craft hand-made (e.g. wrong-version) payloads for the deserializer to reject.
    let private gzipBytes (uncompressed : byte array) : byte array =
        use output = new MemoryStream ()
        use gzip = new GZipStream (output, CompressionLevel.Optimal)
        gzip.Write (uncompressed, 0, uncompressed.Length)
        gzip.Close ()
        output.ToArray ()

    /// A pattern "character" rendered as a string: a BMP letter, '.', or an astral (non-BMP)
    /// character, which in a UTF-16 string is a surrogate pair (two lone-surrogate code units).
    let private patternCharStr : Gen<string> =
        Gen.frequency
            [
                (20, Gen.choose (int 'a', int 'z') |> Gen.map (char >> string))
                (2, Gen.constant ".")
                (5, Gen.choose (0x10000, 0x10FFFF) |> Gen.map System.Char.ConvertFromUtf32)
            ]

    /// A valid Liang pattern whose alphabet may include astral characters.
    let private astralPattern : Gen<string> =
        gen {
            let! leadingDigit = Gen.optionOf Generators.priorityDigit
            let! charCount = Gen.choose (1, 8)
            let! chars = Gen.listOfLength charCount patternCharStr
            let! trailingDigits = Gen.listOfLength charCount (Gen.optionOf Generators.priorityDigit)

            let sb = System.Text.StringBuilder ()
            leadingDigit |> Option.iter (sb.Append >> ignore<System.Text.StringBuilder>)

            for i = 0 to chars.Length - 1 do
                sb.Append chars.[i] |> ignore<System.Text.StringBuilder>

                trailingDigits.[i]
                |> Option.iter (sb.Append >> ignore<System.Text.StringBuilder>)

            return sb.ToString ()
        }

    let private astralPatternList : Gen<string list> =
        Gen.listOf astralPattern |> Gen.map (List.truncate 30)

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

    [<Test>]
    let ``Serialization refuses priority vectors longer than 255`` () =
        // A priority vector has length (pattern chars + 1) and is serialized with a single-byte length
        // prefix (with 0 reserved to mean None). A pattern of 255+ characters would overflow that byte,
        // silently corrupting the data on round-trip, so serialization must fail loudly instead.
        let builder = PackedTrieBuilder ()
        builder.AddPatterns [ System.String ('a', 300) ]
        let trie = builder.Build ()

        let exn =
            Assert.Throws<exn> (fun () -> PackedTrieSerialization.serialize trie |> ignore<byte array>)

        exn.Message.Contains "301" |> shouldEqual true

    [<Test>]
    let ``Regression: serialization round-trips a trie whose alphabet contains an astral character`` () =
        // U+1F600 GRINNING FACE is astral: in a UTF-16 string it's the surrogate pair D83D DE00.
        // Each half enters the alphabet as a lone surrogate. Serialization used to throw here, because
        // BinaryWriter.Write(char) refuses to write surrogate chars; we now write raw uint16 code units.
        let astral = "\U0001F600"

        let builder = PackedTrieBuilder ()
        builder.AddPatterns [ "." + astral + "3b" ]
        let original = builder.Build ()

        let roundTripped =
            original
            |> PackedTrieSerialization.serialize
            |> PackedTrieSerialization.deserialize

        triesEqual original roundTripped |> shouldEqual true

        // The deserialized trie hyphenates identically: a break after the whole emoji (inter-letter
        // index 1) and crucially none between the two surrogate halves (index 0).
        let word = astral + "b"
        Hyphenation.getHyphenationPoints roundTripped word |> shouldEqual [| 1 |]

        Hyphenation.getHyphenationPoints roundTripped word
        |> shouldEqual (Hyphenation.getHyphenationPoints original word)

    [<Test>]
    let ``Serialization round-trips for arbitrary patterns including astral characters`` () =
        let property (patterns : string list) =
            let original =
                let builder = PackedTrieBuilder ()
                builder.AddPatterns patterns
                builder.Build ()

            let roundTripped =
                original
                |> PackedTrieSerialization.serialize
                |> PackedTrieSerialization.deserialize

            triesEqual original roundTripped |> shouldEqual true

        let gen = astralPatternList |> Arb.fromGen
        // 10000 trie builds + serializations is needlessly slow; 1000 amply exercises the surrogate path.
        Check.One (FsCheckConfig.config.WithMaxTest 1000, Prop.forAll gen property)

    [<Test>]
    let ``Deserializing an unsupported (old) version fails loudly`` () =
        // magic "LHYP" followed by version byte 1 (the pre-astral format). The byte layout of the
        // char field changed in v2, so a stale v1 file must be rejected loudly on the version check
        // rather than silently misdecoded.
        let v1Header = [| 0x4Cuy ; 0x48uy ; 0x59uy ; 0x50uy ; 1uy |]
        let payload = gzipBytes v1Header

        let exn =
            Assert.Throws<exn> (fun () -> PackedTrieSerialization.deserialize payload |> ignore<PackedTrie>)

        exn.Message.Contains "version" |> shouldEqual true

    [<TestCaseSource(nameof languageCases)>]
    let ``Can load language data`` (language : KnownLanguage) =
        let trie = LanguageData.load language
        trie.Data |> shouldNotEqual null
        trie.Bases |> shouldNotEqual null
        trie.CharMap |> shouldNotEqual null
