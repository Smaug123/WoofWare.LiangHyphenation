namespace WoofWare.LiangHyphenation.Construction

open WoofWare.LiangHyphenation
open System.Collections.Generic

/// Module containing methods for packing the expanded LinkedTrieNode-based structure into an efficient PackedTrie.
///
/// These methods are wrapped by the `PackedTrieBuilder` type, which you may find more ergonomic.
[<RequireQualifiedAccess>]
module TriePacking =
    /// Collect all unique nodes and the alphabet from the trie
    let collectNodesAndAlphabet (root : LinkedTrieNode option) : LinkedTrieNode array * int<alphabetIndex> array =
        let nodes = HashSet<LinkedTrieNode> HashIdentity.Reference
        let chars = HashSet<char> ()

        let rec visit (node : LinkedTrieNode option) =
            match node with
            | None -> ()
            | Some n ->
                if nodes.Add n then
                    chars.Add n.Char |> ignore
                    visit n.Left
                    visit n.Right

        visit root

        // Build char map: char -> dense index
        let sortedChars = chars |> Seq.sort |> Seq.toArray
        let charMap = Array.create 65536 -1<alphabetIndex>

        for i = 0 to sortedChars.Length - 1 do
            charMap.[int sortedChars.[i]] <- LanguagePrimitives.Int32WithMeasure i

        (nodes |> Seq.toArray, charMap)

    /// Enumerate transitions (children) for a node by walking its Right child's sibling chain
    let enumerateTransitions (node : LinkedTrieNode) : struct (char * LinkedTrieNode) seq =
        let rec loop (child : LinkedTrieNode option) =
            seq {
                match child with
                | None -> ()
                | Some c ->
                    yield struct (c.Char, c)
                    yield! loop c.Left
            }

        loop node.Right

    /// Count transitions for a node
    let countTransitions (node : LinkedTrieNode) : int =
        let rec count (child : LinkedTrieNode option) acc =
            match child with
            | None -> acc
            | Some c -> count c.Left (acc + 1)

        count node.Right 0

    /// Pack the trie using first-fit algorithm
    let pack (root : LinkedTrieNode option) (charMap : int<alphabetIndex> array) (alphabetSize : int) : PackedTrie =
        match root with
        | None ->
            {
                Data = [||]
                Bases = [||]
                CharMap = charMap
                AlphabetSize = LanguagePrimitives.Int32WithMeasure alphabetSize
                PatternPriorities = [||]
            }
        | Some rootNode ->
            // Collect all unique nodes
            let nodes, _ = collectNodesAndAlphabet (Some rootNode)

            // Assign state IDs (rootNode = state 0)
            let nodeToState =
                Dictionary<LinkedTrieNode, int<trieState>> (HashIdentity.Reference)

            nodeToState.[rootNode] <- 0<trieState>
            let mutable nextState = 1<trieState>

            for node in nodes do
                if not (nodeToState.ContainsKey (node)) then
                    nodeToState.[node] <- nextState
                    nextState <- nextState + 1<trieState>

            let stateCount = int nextState

            if stateCount > 65535 then
                failwithf "Trie has %d states, but PackedTrieEntry only supports up to 65535" stateCount

            // Precompute transitions and sort by count (descending) for better packing
            let sortedNodesWithTransitions =
                nodes
                |> Array.map (fun n -> n, enumerateTransitions n |> Seq.toArray)
                |> Array.sortByDescending (fun (_, t) -> t.Length)

            // Packing state
            let mutable dataSize = stateCount * alphabetSize // Initial estimate
            let mutable data = Array.create dataSize PackedTrieEntry.Empty
            let occupied = HashSet<int> ()
            let usedBases = HashSet<int> ()
            let bases = Array.create stateCount 0<trieIndex>

            // Pattern priorities indexed by state
            let patternPriorities = Array.create stateCount None

            let ensureCapacity (needed : int) =
                if needed > data.Length then
                    let newSize = max needed (data.Length * 2)
                    let newData = Array.create newSize PackedTrieEntry.Empty
                    Array.blit data 0 newData 0 data.Length
                    data <- newData

            // Track minimum possibly-free base to avoid re-scanning used bases
            let mutable searchStartBase = 0

            // Pack each node
            for node, transitions in sortedNodesWithTransitions do
                let stateId = nodeToState.[node]

                // Store pattern priorities if this is a pattern-end node
                patternPriorities.[int stateId] <- node.PatternPriorities

                if transitions.Length = 0 then
                    // No transitions - just need a valid base that's not used
                    let mutable baseIdx = searchStartBase

                    while usedBases.Contains baseIdx do
                        baseIdx <- baseIdx + 1

                    usedBases.Add baseIdx |> ignore
                    bases.[int stateId] <- LanguagePrimitives.Int32WithMeasure baseIdx

                    // Advance search start past known-used bases
                    while usedBases.Contains searchStartBase do
                        searchStartBase <- searchStartBase + 1
                else
                    // Find a base where all transitions fit
                    let mutable baseIdx = searchStartBase
                    let mutable found = false

                    while not found do
                        if usedBases.Contains baseIdx then
                            baseIdx <- baseIdx + 1
                        else
                            // Check if all transitions fit
                            let mutable fits = true

                            for struct (c, _) in transitions do
                                let charIdx = int charMap.[int c]
                                let slot = baseIdx + charIdx

                                if occupied.Contains slot then
                                    fits <- false

                            if fits then found <- true else baseIdx <- baseIdx + 1

                    // Place transitions at this base
                    usedBases.Add baseIdx |> ignore
                    bases.[int stateId] <- LanguagePrimitives.Int32WithMeasure baseIdx

                    for struct (c, childNode) in transitions do
                        let charIdx = int charMap.[int c]
                        let slot = baseIdx + charIdx
                        ensureCapacity (slot + 1)
                        let childState = nodeToState.[childNode]
                        data.[slot] <- PackedTrieEntry.OfComponents c childState
                        occupied.Add slot |> ignore

                    // Advance search start past known-used bases
                    while usedBases.Contains searchStartBase do
                        searchStartBase <- searchStartBase + 1

            // Trim data array
            let maxUsed = if occupied.Count = 0 then 0 else (occupied |> Seq.max) + 1
            let trimmedData = Array.sub data 0 maxUsed

            {
                Data = trimmedData
                Bases = bases
                CharMap = charMap
                AlphabetSize = LanguagePrimitives.Int32WithMeasure alphabetSize
                PatternPriorities = patternPriorities
            }
