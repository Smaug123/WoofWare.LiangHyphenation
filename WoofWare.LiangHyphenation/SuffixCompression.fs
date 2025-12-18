namespace WoofWare.LiangHyphenation.Construction

open WoofWare.LiangHyphenation
open System.Collections.Generic
open System.Runtime.CompilerServices

/// Module containing methods for compressing the trie.
/// Suffix compression takes an existing trie and merges common subtries together, starting from the leaves
/// and working upwards. (See p16 of Liang's thesis.)
[<RequireQualifiedAccess>]
module SuffixCompression =
    /// Hash a byte array option for use in node hashing
    let private hashPriorities (priorities : byte array option) : int =
        match priorities with
        | None -> 0
        | Some arr ->
            let mutable h = arr.Length

            for b in arr do
                h <- h * 31 + int b

            h

    /// Check if two priority arrays are equal
    let private prioritiesEqual (a : byte array option) (b : byte array option) : bool =
        match a, b with
        | None, None -> true
        | Some arrA, Some arrB when arrA.Length = arrB.Length ->
            let mutable equal = true
            let mutable i = 0

            while equal && i < arrA.Length do
                if arrA.[i] <> arrB.[i] then
                    equal <- false

                i <- i + 1

            equal
        | _ -> false

    /// Hash a node based on its structure (for identifying equivalent nodes).
    /// Uses identity hash for children since they've already been compressed to canonical nodes.
    let private hashNode (node : LinkedTrieNode) : int =
        let mutable h = hash node.Char
        h <- h * 31 + hashPriorities node.PatternPriorities

        h <-
            h * 31
            + (
                match node.Left with
                | Some n -> RuntimeHelpers.GetHashCode n
                | None -> 0
            )

        h <-
            h * 31
            + (
                match node.Right with
                | Some n -> RuntimeHelpers.GetHashCode n
                | None -> 0
            )

        h

    /// Compare two option nodes by the identity of their contained nodes (not the option wrappers).
    let private optionNodesSame (a : LinkedTrieNode option) (b : LinkedTrieNode option) : bool =
        match a, b with
        | None, None -> true
        | Some nodeA, Some nodeB -> Object.referenceEquals nodeA nodeB
        | _ -> false

    /// Check if two nodes are structurally equivalent.
    /// Children are compared by reference identity since they've already been compressed to canonical nodes.
    let private nodesEqual (a : LinkedTrieNode) (b : LinkedTrieNode) : bool =
        a.Char = b.Char
        && prioritiesEqual a.PatternPriorities b.PatternPriorities
        && optionNodesSame a.Left b.Left
        && optionNodesSame a.Right b.Right

    /// Reference equality comparer for LinkedTrieNode (avoids expensive structural comparison)
    let private referenceComparer =
        { new IEqualityComparer<LinkedTrieNode> with
            member _.Equals (x, y) = Object.referenceEquals x y
            member _.GetHashCode x = RuntimeHelpers.GetHashCode x
        }

    /// Perform suffix compression on the trie, merging equivalent subtries.
    /// Returns a mapping from original nodes to canonical nodes.
    let compress (root : LinkedTrieNode option) : Dictionary<LinkedTrieNode, LinkedTrieNode> =
        let canonical = Dictionary<int, ResizeArray<LinkedTrieNode>> ()
        let mapping = Dictionary<LinkedTrieNode, LinkedTrieNode> referenceComparer

        let rec compressNode (node : LinkedTrieNode) : LinkedTrieNode =
            // First, recursively compress children
            node.Left <- node.Left |> Option.map compressNode
            node.Right <- node.Right |> Option.map compressNode

            // Now find or create canonical version
            let h = hashNode node

            match canonical.TryGetValue h with
            | false, _ ->
                let bucket = ResizeArray<LinkedTrieNode> ()
                bucket.Add node
                canonical.[h] <- bucket
                mapping.[node] <- node
                node
            | true, bucket ->
                match bucket |> Seq.tryFind (fun n -> nodesEqual n node) with
                | Some existing ->
                    mapping.[node] <- existing
                    existing
                | None ->
                    bucket.Add node
                    mapping.[node] <- node
                    node

        root |> Option.iter (compressNode >> ignore)
        mapping
