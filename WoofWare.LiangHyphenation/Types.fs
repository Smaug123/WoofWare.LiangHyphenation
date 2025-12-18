namespace WoofWare.LiangHyphenation

/// Index into the packed trie array
[<Measure>]
type trieIndex

/// A state in the trie (identifies a node)
[<Measure>]
type trieState

/// A dense index into the alphabet (0-based)
[<Measure>]
type alphabetIndex

/// A single packed trie entry (64 bits):
/// - Bits 0-15: character code (supports full BMP Unicode)
/// - Bits 16-19: hyphenation priority (0-9, only 4 bits needed)
/// - Bits 20-63: link to next state's base index
[<Struct>]
type PackedTrieEntry =
    {
        /// The raw 64-bit packed representation.
        Value : uint64
    }

    /// Construct from a raw 64-bit packed value.
    static member OfValue (value : uint64) =
        {
            Value = value
        }

    /// Construct from individual components: character, hyphenation priority (0-9), and link to next state.
    static member OfComponents (char : char) (priority : byte) (link : int<trieState>) =
        let charBits = uint64 (uint16 char)
        let priorityBits = (uint64 priority &&& 0xFUL) <<< 16
        let linkBits = (uint64 (int link) &&& 0xFFFFFFFFFFFUL) <<< 20

        {
            Value = charBits ||| priorityBits ||| linkBits
        }

    /// The character stored in this entry (bits 0-15).
    member inline this.Char : char = char (uint16 (this.Value &&& 0xFFFFUL))
    /// The hyphenation priority at this position (bits 16-19, range 0-9).
    member inline this.Priority : byte = byte ((this.Value >>> 16) &&& 0xFUL)

    /// The link to the next trie state (bits 20-63).
    member inline this.Link : int<trieState> =
        LanguagePrimitives.Int32WithMeasure (int (this.Value >>> 20))

    /// An empty entry (all zeros), representing no transition.
    static member Empty = PackedTrieEntry.OfValue 0UL

/// The packed trie data structure for efficient pattern matching.
/// You will normally extract a pre-computed one of these from the library using `LanguageData.load`,
/// and consume it with the methods in the `Hyphenation` module.
type PackedTrie =
    {
        /// The packed array of transitions (indexed by base + alphabetIndex)
        Data : PackedTrieEntry array
        /// Base index in Data for each state (state 0 is root)
        Bases : int<trieIndex> array
        /// Character-to-index mapping for the alphabet (-1 means not in alphabet)
        CharMap : int<alphabetIndex> array
        /// Size of the dense alphabet
        AlphabetSize : int<alphabetIndex>
    }

/// The packed trie data structure for efficient pattern matching.
/// You will normally extract a pre-computed one of these from the library using `LanguageData.load`,
/// and consume it with the methods in the `Hyphenation` module.
[<RequireQualifiedAccess>]
module PackedTrie =
    /// The root state
    let root : int<trieState> = 0<trieState>

    /// Try to transition from a state on a character
    let inline tryTransition
        (trie : PackedTrie)
        (state : int<trieState>)
        (c : char)
        : struct (int<trieState> * byte) voption
        =
        let charIdx = trie.CharMap.[int c]

        if charIdx < 0<alphabetIndex> then
            ValueNone
        else
            let baseIdx = trie.Bases.[int state]
            let slot = int baseIdx + int charIdx

            if slot >= trie.Data.Length then
                ValueNone
            else
                let entry = trie.Data.[slot]

                if entry.Char = c then
                    ValueSome (struct (entry.Link, entry.Priority))
                else
                    ValueNone
