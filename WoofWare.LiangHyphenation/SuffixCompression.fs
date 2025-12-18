namespace WoofWare.LiangHyphenation.Construction

open System
open System.Collections.Generic

[<RequireQualifiedAccess>]
module SuffixCompression =
    /// Hash a node based on its structure (for identifying equivalent nodes)
    let private hashNode (node : LinkedTrieNode) : int =
        let mutable h = hash node.Char
        h <- h * 31 + int node.Priority

        h <-
            h * 31
            + (
                match node.Left with
                | Some n -> hash (n :> obj)
                | None -> 0
            )

        h <-
            h * 31
            + (
                match node.Right with
                | Some n -> hash (n :> obj)
                | None -> 0
            )

        h

    /// Check if two nodes are structurally equivalent
    let private nodesEqual (a : LinkedTrieNode) (b : LinkedTrieNode) : bool =
        a.Char = b.Char
        && a.Priority = b.Priority
        && Object.ReferenceEquals (a.Left, b.Left)
        && Object.ReferenceEquals (a.Right, b.Right)

    /// Perform suffix compression on the trie, merging equivalent subtries.
    /// Returns a mapping from original nodes to canonical nodes.
    let compress (root : LinkedTrieNode option) : Dictionary<LinkedTrieNode, LinkedTrieNode> =
        let canonical = Dictionary<int, ResizeArray<LinkedTrieNode>> ()
        let mapping = Dictionary<LinkedTrieNode, LinkedTrieNode> ()

        let rec compressNode (node : LinkedTrieNode) : LinkedTrieNode =
            // First, recursively compress children
            node.Left <- node.Left |> Option.map compressNode
            node.Right <- node.Right |> Option.map compressNode

            // Now find or create canonical version
            let h = hashNode node

            match canonical.TryGetValue (h) with
            | false, _ ->
                let bucket = ResizeArray<LinkedTrieNode> ()
                bucket.Add (node)
                canonical.[h] <- bucket
                mapping.[node] <- node
                node
            | true, bucket ->
                match bucket |> Seq.tryFind (fun n -> nodesEqual n node) with
                | Some existing ->
                    mapping.[node] <- existing
                    existing
                | None ->
                    bucket.Add (node)
                    mapping.[node] <- node
                    node

        root |> Option.iter (compressNode >> ignore)
        mapping
