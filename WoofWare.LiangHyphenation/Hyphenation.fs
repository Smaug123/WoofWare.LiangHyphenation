namespace WoofWare.LiangHyphenation

[<RequireQualifiedAccess>]
module Hyphenation =
    /// Hyphenate a word using the packed trie.
    /// Returns an array of priorities at each inter-letter position.
    /// Odd priority = can hyphenate, even = cannot.
    let hyphenate (trie: PackedTrie) (word: string) : byte array =
        if word.Length < 2 then
            trace (fun () -> $"Hyphenate '%s{word}': too short")
            [||]
        else
            // Priorities at inter-letter positions (word.Length - 1 positions)
            let priorities = Array.zeroCreate<byte> (word.Length - 1)

            // Extended word with boundary markers
            let extended = "." + word.ToLowerInvariant() + "."
            trace (fun () -> $"Hyphenate '%s{word}' -> extended '%s{extended}'")

            // For each starting position
            for start = 0 to extended.Length - 1 do
                let mutable state = PackedTrie.root
                let mutable pos = start
                let mutable continue' = true

                trace (fun () -> $"  Start=%d{start} (char '%c{extended.[start]}'):")

                while continue' && pos < extended.Length do
                    let c = extended.[pos]

                    match PackedTrie.tryTransition trie state c with
                    | ValueSome(struct (nextState, priority)) ->
                        trace (fun () ->
                            $"    pos=%d{pos} char='%c{c}' -> state=%d{int nextState} priority=%d{int priority}")
                        // Priority applies to position (start + offset) in the extended word
                        // which maps to (start + offset - 1) in the original word
                        // Inter-letter position i is between word.[i] and word.[i+1]
                        let interLetterPos = start + (pos - start) - 1

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
                    priorities |> Array.map (fun b -> b.ToString()) |> String.concat ", "

                $"  Result: [|%s{priorityStr}|]")

            priorities

    /// Get hyphenation points for a word.
    /// Returns indices where hyphenation is allowed (between word.[i] and word.[i+1]).
    let getHyphenationPoints (trie: PackedTrie) (word: string) : int array =
        let priorities = hyphenate trie word

        priorities
        |> Array.mapi (fun i p -> if p % 2uy = 1uy then Some i else None)
        |> Array.choose id

    /// Insert hyphens into a word at allowed positions.
    let hyphenateWord (trie: PackedTrie) (word: string) (hyphen: string) : string =
        let points = getHyphenationPoints trie word

        if points.Length = 0 then
            word
        else
            let sb = System.Text.StringBuilder()
            let mutable lastIdx = 0

            for point in points do
                sb.Append(word.Substring(lastIdx, point + 1 - lastIdx)) |> ignore
                sb.Append(hyphen) |> ignore
                lastIdx <- point + 1

            sb.Append(word.Substring(lastIdx)) |> ignore
            sb.ToString()
