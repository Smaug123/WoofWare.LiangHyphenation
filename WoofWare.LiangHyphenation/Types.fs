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

/// A single packed trie entry (32 bits):
/// - Bits 0-15: character code (supports full BMP Unicode)
/// - Bits 16-31: link to next state
[<Struct>]
type PackedTrieEntry =
    {
        /// The raw 32-bit packed representation.
        Value : uint32
    }

    /// Construct from a raw 32-bit packed value.
    static member OfValue (value : uint32) =
        {
            Value = value
        }

    /// Construct from individual components: character and link to next state.
    static member OfComponents (char : char) (link : int<trieState>) =
        let charBits = uint32 (uint16 char)
        let linkBits = (uint32 (int link)) <<< 16

        {
            Value = charBits ||| linkBits
        }

    /// The character stored in this entry (bits 0-15).
    member inline this.Char : char = char (uint16 (this.Value &&& 0xFFFFu))

    /// The link to the next trie state (bits 16-31).
    member inline this.Link : int<trieState> =
        LanguagePrimitives.Int32WithMeasure (int (this.Value >>> 16))

    /// An empty entry (all zeros), representing no transition.
    static member Empty = PackedTrieEntry.OfValue 0u

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
        /// Priority vectors for pattern-end states, indexed by state.
        /// None = not a pattern-end; Some = the full priority vector for patterns ending here.
        PatternPriorities : byte array option array
    }

/// The packed trie data structure for efficient pattern matching.
/// You will normally extract a pre-computed one of these from the library using `LanguageData.load`,
/// and consume it with the methods in the `Hyphenation` module.
[<RequireQualifiedAccess>]
module PackedTrie =
    /// The root state
    let root : int<trieState> = 0<trieState>

    /// Try to transition from a state on a character.
    /// Returns the next state if successful.
    let inline tryTransition (trie : PackedTrie) (state : int<trieState>) (c : char) : int<trieState> voption =
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

                if entry.Char = c then ValueSome entry.Link else ValueNone

    /// Get the pattern-end priority vector for a state, if any.
    let inline getPatternPriorities (trie : PackedTrie) (state : int<trieState>) : byte array option =
        trie.PatternPriorities.[int state]
