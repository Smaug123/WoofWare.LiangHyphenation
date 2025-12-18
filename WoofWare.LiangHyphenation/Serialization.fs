namespace WoofWare.LiangHyphenation

open System.IO
open System.IO.Compression
open System.Reflection
open System.Text

/// Module containing methods for ser/de of a PackedTrie, so that you can precompute one and store it.
///
/// WoofWare.LiangHyphenation ships with precomputed data represented by the `KnownLanguage` DU.
[<RequireQualifiedAccess>]
module PackedTrieSerialization =
    // Magic bytes: "LHYP" (Liang HYPhenation)
    let private magic = [| 0x4Cuy ; 0x48uy ; 0x59uy ; 0x50uy |]
    let private version = 1uy

    let private serializeToStream (trie : PackedTrie) (stream : Stream) : unit =
        use writer = new BinaryWriter (stream, Encoding.UTF8, leaveOpen = true)

        // Write magic and version
        writer.Write magic
        writer.Write version

        // Write Data array (32-bit entries now)
        writer.Write trie.Data.Length

        for entry in trie.Data do
            writer.Write entry.Value

        // Write Bases array
        writer.Write trie.Bases.Length

        for baseIdx in trie.Bases do
            writer.Write (int baseIdx)

        // Write CharMap (compressed: only non-negative entries)
        let charMapEntries =
            trie.CharMap
            |> Array.mapi (fun i idx -> (char<int> i, idx))
            |> Array.filter (fun (_, idx) -> idx >= 0<alphabetIndex>)

        writer.Write charMapEntries.Length

        for c, idx in charMapEntries do
            writer.Write c
            writer.Write (idx / 1<alphabetIndex>)

        // Write AlphabetSize
        writer.Write (int trie.AlphabetSize)

        // Write PatternPriorities array
        writer.Write trie.PatternPriorities.Length

        for priorities in trie.PatternPriorities do
            match priorities with
            | None -> writer.Write 0uy // 0 length means None
            | Some arr ->
                writer.Write (byte arr.Length)
                writer.Write arr

    let private deserializeFromStream (stream : Stream) : PackedTrie =
        use reader = new BinaryReader (stream, Encoding.UTF8, leaveOpen = true)

        // Read and verify magic
        let readMagic = reader.ReadBytes 4

        if readMagic <> magic then
            failwith "Invalid PackedTrie data: bad magic bytes"

        // Read and verify version
        let readVersion = reader.ReadByte ()

        if readVersion <> version then
            failwith $"Unsupported PackedTrie version: %d{int readVersion} (expected %d{int version})"

        // Read Data array (32-bit entries)
        let dataLength = reader.ReadInt32 ()

        let trieData =
            Array.init dataLength (fun _ -> PackedTrieEntry.OfValue (reader.ReadUInt32 ()))

        // Read Bases array
        let basesLength = reader.ReadInt32 ()

        let bases : int<trieIndex> array =
            Array.init basesLength (fun _ -> LanguagePrimitives.Int32WithMeasure (reader.ReadInt32 ()))

        // Read CharMap (compressed)
        let charMap : int<alphabetIndex> array = Array.create 65536 -1<alphabetIndex>
        let charMapEntryCount = reader.ReadInt32 ()

        for _ = 0 to charMapEntryCount - 1 do
            let c = reader.ReadChar ()
            let asciiCode = int<char> c
            let idx = reader.ReadInt32 ()
            charMap.[asciiCode] <- LanguagePrimitives.Int32WithMeasure idx

        // Read AlphabetSize
        let alphabetSize : int<alphabetIndex> =
            LanguagePrimitives.Int32WithMeasure (reader.ReadInt32 ())

        // Read PatternPriorities array
        let patternPrioritiesLength = reader.ReadInt32 ()

        let patternPriorities =
            Array.init
                patternPrioritiesLength
                (fun _ ->
                    let len = int (reader.ReadByte ())
                    if len = 0 then None else Some (reader.ReadBytes len)
                )

        {
            Data = trieData
            Bases = bases
            CharMap = charMap
            AlphabetSize = alphabetSize
            PatternPriorities = patternPriorities
        }

    /// Serialize a PackedTrie to a GZip-compressed byte array, suitable for later deserialization with
    /// `PackedTrieSerialization.deserialize`.
    let serialize (trie : PackedTrie) : byte array =
        use output = new MemoryStream ()
        use gzip = new GZipStream (output, CompressionLevel.Optimal)
        serializeToStream trie gzip
        gzip.Close ()
        output.ToArray ()

    /// Deserialize a PackedTrie from a GZip-compressed byte array, e.g. as was originally written with the
    /// `PackedTrieSerialization.serialize` function.
    let deserialize (data : byte array) : PackedTrie =
        use input = new MemoryStream (data)
        use gzip = new GZipStream (input, CompressionMode.Decompress)
        deserializeFromStream gzip

    /// Load a PackedTrie from a GZip-compressed embedded resource in the given assembly (e.g. as was originally
    /// written with the `PackedTrieSerialization.serialize` function).
    ///
    /// `resourceName` is the fully-qualified name, like "WoofWare.LiangHyphenation.Data.en-gb.bin".
    let loadFromEmbeddedResource (assembly : Assembly) (resourceName : string) : PackedTrie =
        use stream = assembly.GetManifestResourceStream resourceName

        if isNull stream then
            let available = assembly.GetManifestResourceNames () |> String.concat ", "
            failwith $"Embedded resource '%s{resourceName}' not found. Available: %s{available}"

        use gzip = new GZipStream (stream, CompressionMode.Decompress)
        deserializeFromStream gzip

/// A language for which WoofWare.LiangHyphenation ships with hyphenation data.
/// Pass this to `LanguageData.load`.
type KnownLanguage =
    /// UK English.
    | EnGb

/// Module containing methods to interact with the precomputed hyphenation data that ships with WoofWare.LiangHyphenation.
[<RequireQualifiedAccess>]
module LanguageData =
    /// Gets the trailing fragment of the EmbeddedResource name specifying which embedded resource contains the data
    /// for this language.
    ///
    /// You're not expected to interact with this directly.
    let inline getResourceNameFragment (language : KnownLanguage) : string =
        match language with
        | KnownLanguage.EnGb -> "en-gb.bin"

    /// Gets the name of the EmbeddedResource within WoofWare.LiangHyphenation containing the given language's
    /// precomputed PackedTrie.
    ///
    /// You probably want to use `LanguageData.load` instead, to obtain the PackedTrie directly.
    let inline getResourceName (language : KnownLanguage) : string =
        "WoofWare.LiangHyphenation.Data." + getResourceNameFragment language

    /// Load the data for the given language embedded within the WoofWare.LiangHyphenation assembly.
    let load (language : KnownLanguage) : PackedTrie =
        let path = getResourceName language

        PackedTrieSerialization.loadFromEmbeddedResource typeof<PackedTrie>.Assembly path
