namespace WoofWare.LiangHyphenation

open System
open System.Text

/// Module containing methods for performing hyphenation.
/// You probably want to use `LanguageData.load` to acquire the PackedTrie.
[<RequireQualifiedAccess>]
module Hyphenation =
    /// Hyphenate a word using the packed trie.
    /// Returns an array of priorities at each inter-letter position.
    /// Odd priority = can hyphenate, even = cannot.
    ///
    /// You probably want to use `LanguageData.load` to acquire the PackedTrie.
    let hyphenate (trie : PackedTrie) (word : string) : byte array =
        if word.Length < 2 then
            trace (fun () -> $"Hyphenate '%s{word}': too short")
            [||]
        else
            // Priorities at inter-letter positions (word.Length - 1 positions)
            let priorities = Array.zeroCreate<byte> (word.Length - 1)

            // Extended word with boundary markers.
            // PERF: in future, stop allocating here
            let extended = "." + word + "."
            trace (fun () -> $"Hyphenate '%s{word}' -> extended '%s{extended}'")

            // For each starting position
            for start = 0 to extended.Length - 1 do
                let mutable state = PackedTrie.root
                let mutable pos = start
                let mutable continue' = true

                trace (fun () -> $"  Start=%d{start} (char '%c{extended.[start]}'):")

                while continue' && pos < extended.Length do
                    let c = Char.ToLowerInvariant extended.[pos]

                    match PackedTrie.tryTransition trie state c with
                    | ValueSome (struct (nextState, priority)) ->
                        trace (fun () ->
                            $"    pos=%d{pos} char='%c{c}' -> state=%d{int nextState} priority=%d{int priority}"
                        )
                        // Priority at character position `pos` in extended word applies to
                        // the inter-letter position BEFORE that character.
                        // Extended word has leading '.', so:
                        //   - Extended inter-letter position = pos - 1
                        //   - Original inter-letter position = (pos - 1) - 1 = pos - 2
                        let interLetterPos = pos - 2

                        if interLetterPos >= 0 && interLetterPos < priorities.Length then
                            if priority > priorities.[interLetterPos] then
                                priorities.[interLetterPos] <- priority

                        state <- nextState
                        pos <- pos + 1
                    | ValueNone ->
                        trace (fun () -> $"    pos=%d{pos} char='%c{c}' -> no transition")
                        continue' <- false

            trace (fun () ->
                let priorityStr =
                    priorities |> Array.map (fun b -> b.ToString ()) |> String.concat ", "

                $"  Result: [|%s{priorityStr}|]"
            )

            priorities

    /// Get hyphenation points for a word.
    /// Returns indices where hyphenation is allowed (between word.[i] and word.[i+1]).
    ///
    /// You probably want to use `LanguageData.load` to acquire the PackedTrie.
    let getHyphenationPoints (trie : PackedTrie) (word : string) : int array =
        let priorities = hyphenate trie word

        priorities
        |> Array.mapi (fun i p -> if p % 2uy = 1uy then Some i else None)
        |> Array.choose id

    /// Insert hyphens into a word at all allowed positions.
    /// You shouldn't use this function in production: it simply shows you all the allowable breaks,
    /// without regard to their priority.
    ///
    /// You probably want to use `LanguageData.load` to acquire the PackedTrie.
    let hyphenateWord (trie : PackedTrie) (word : string) (hyphen : string) : string =
        let points = getHyphenationPoints trie word

        if points.Length = 0 then
            word
        else
            let sb = StringBuilder ()
            let mutable lastIdx = 0

            for point in points do
                sb.Append (word.Substring (lastIdx, point + 1 - lastIdx))
                |> ignore<StringBuilder>

                sb.Append hyphen |> ignore<StringBuilder>
                lastIdx <- point + 1

            sb.Append (word.Substring lastIdx) |> ignore<StringBuilder>
            sb.ToString ()
