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
    let enumerateTransitions (node : LinkedTrieNode) : struct (char * byte * LinkedTrieNode) seq =
        let rec loop (child : LinkedTrieNode option) =
            seq {
                match child with
                | None -> ()
                | Some c ->
                    yield struct (c.Char, c.Priority, c)
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

            // Sort nodes by transition count (descending) for better packing
            let sortedNodes = nodes |> Array.sortByDescending countTransitions

            // Packing state
            let mutable dataSize = stateCount * alphabetSize // Initial estimate
            let mutable data = Array.create dataSize PackedTrieEntry.Empty
            let occupied = HashSet<int> ()
            let usedBases = HashSet<int> ()
            let bases = Array.create stateCount 0<trieIndex>

            let ensureCapacity (needed : int) =
                if needed > data.Length then
                    let newSize = max needed (data.Length * 2)
                    let newData = Array.create newSize PackedTrieEntry.Empty
                    Array.blit data 0 newData 0 data.Length
                    data <- newData

            // Pack each node
            for node in sortedNodes do
                let stateId = nodeToState.[node]
                let transitions = enumerateTransitions node |> Seq.toArray

                if transitions.Length = 0 then
                    // No transitions - just need a valid base that's not used
                    let mutable baseIdx = 0

                    while usedBases.Contains (baseIdx) do
                        baseIdx <- baseIdx + 1

                    usedBases.Add (baseIdx) |> ignore
                    bases.[int stateId] <- LanguagePrimitives.Int32WithMeasure baseIdx
                else
                    // Find a base where all transitions fit
                    let mutable baseIdx = 0
                    let mutable found = false

                    while not found do
                        if usedBases.Contains (baseIdx) then
                            baseIdx <- baseIdx + 1
                        else
                            // Check if all transitions fit
                            let mutable fits = true

                            for struct (c, _, _) in transitions do
                                let charIdx = int charMap.[int c]
                                let slot = baseIdx + charIdx

                                if occupied.Contains (slot) then
                                    fits <- false

                            if fits then found <- true else baseIdx <- baseIdx + 1

                    // Place transitions at this base
                    usedBases.Add (baseIdx) |> ignore
                    bases.[int stateId] <- LanguagePrimitives.Int32WithMeasure baseIdx

                    for struct (c, priority, childNode) in transitions do
                        let charIdx = int charMap.[int c]
                        let slot = baseIdx + charIdx
                        ensureCapacity (slot + 1)
                        let childState = nodeToState.[childNode]
                        data.[slot] <- PackedTrieEntry.OfComponents c priority childState
                        occupied.Add (slot) |> ignore

            // Trim data array
            let maxUsed = if occupied.Count = 0 then 0 else (occupied |> Seq.max) + 1
            let trimmedData = Array.sub data 0 maxUsed

            {
                Data = trimmedData
                Bases = bases
                CharMap = charMap
                AlphabetSize = LanguagePrimitives.Int32WithMeasure alphabetSize
            }
