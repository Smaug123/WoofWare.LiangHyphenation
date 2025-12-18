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

            // For each starting position, try to match patterns
            for start = 0 to extended.Length - 1 do
                let mutable state = PackedTrie.root
                let mutable pos = start
                let mutable continue' = true

                trace (fun () -> $"  Start=%d{start} (char '%c{extended.[start]}'):")

                while continue' && pos < extended.Length do
                    let c = Char.ToLowerInvariant extended.[pos]

                    match PackedTrie.tryTransition trie state c with
                    | ValueSome nextState ->
                        trace (fun () -> $"    pos=%d{pos} char='%c{c}' -> state=%d{int nextState}")

                        // Check if we've reached a pattern-end state
                        match PackedTrie.getPatternPriorities trie nextState with
                        | Some patternPriorities ->
                            trace (fun () ->
                                let prioStr = patternPriorities |> Array.map string |> String.concat ","

                                $"      Pattern end! Priorities=[%s{prioStr}]"
                            )
                            // Apply the pattern's priority vector at the appropriate positions.
                            // patternPriorities[i] applies at extended inter-letter position (start + i).
                            // Extended inter-letter pos k maps to original inter-letter pos (k - 2).
                            // So patternPriorities[i] applies at original pos (start + i - 2).
                            for i = 0 to patternPriorities.Length - 1 do
                                let origPos = start + i - 2

                                if origPos >= 0 && origPos < priorities.Length then
                                    if patternPriorities.[i] > priorities.[origPos] then
                                        priorities.[origPos] <- patternPriorities.[i]
                        | None -> ()

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
