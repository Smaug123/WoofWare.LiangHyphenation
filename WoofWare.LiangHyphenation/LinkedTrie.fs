namespace WoofWare.LiangHyphenation.Construction

/// A node in the linked trie used during construction.
/// Uses a binary tree encoding: Left = next sibling, Right = first child.
type LinkedTrieNode =
    {
        /// The character this node represents in the trie.
        mutable Char : char
        /// If this node is a pattern-end, the full priority vector for that pattern.
        /// Multiple patterns may end at the same node; we take element-wise max.
        mutable PatternPriorities : byte array option
        /// Next sibling (different char at same depth).
        mutable Left : LinkedTrieNode option
        /// First child (next char in pattern).
        mutable Right : LinkedTrieNode option
    }

/// A node in the linked trie used during construction.
/// Uses a binary tree encoding: Left = next sibling, Right = first child.
[<RequireQualifiedAccess>]
module LinkedTrieNode =
    /// Create a new node
    let create (c : char) : LinkedTrieNode =
        {
            Char = c
            PatternPriorities = None
            Left = None
            Right = None
        }

    /// Find or create a child node for the given character
    let rec findOrAddChild (c : char) (node : LinkedTrieNode) : LinkedTrieNode =
        match node.Right with
        | None ->
            let child = create c
            node.Right <- Some child
            child
        | Some child -> findOrAddSibling c child

    /// Find or create a sibling node for the given character
    and findOrAddSibling (c : char) (node : LinkedTrieNode) : LinkedTrieNode =
        if node.Char = c then
            node
        else
            match node.Left with
            | None ->
                let sibling = create c
                node.Left <- Some sibling
                sibling
            | Some left -> findOrAddSibling c left

    /// Merge a priority vector into a node (element-wise max)
    let mergePriorities (priorities : byte array) (node : LinkedTrieNode) : unit =
        match node.PatternPriorities with
        | None -> node.PatternPriorities <- Some (Array.copy priorities)
        | Some existing ->
            for i = 0 to min existing.Length priorities.Length - 1 do
                if priorities.[i] > existing.[i] then
                    existing.[i] <- priorities.[i]

/// Builder for constructing a linked trie from patterns.
type LinkedTrieBuilder () =
    // Root is the conceptual "start" state; its children are the first pattern characters
    let root = LinkedTrieNode.create '\000'

    /// Insert a parsed pattern into the trie.
    /// Time: O(m * s) where m is pattern length and s is max sibling count at each trie level.
    /// In the worst case s is bounded by alphabet size, but typically s is small because patterns share prefixes.
    member _.Insert (pattern : ParsedPattern) : unit =
        if pattern.Chars.Length <> 0 then
            let mutable current = root

            for i = 0 to pattern.Chars.Length - 1 do
                let c = pattern.Chars.[i]
                current <- LinkedTrieNode.findOrAddChild c current

            // Store the full priority vector on the pattern-end node
            LinkedTrieNode.mergePriorities pattern.Priorities current

    /// Get the root node (the conceptual start state).
    /// This is constant-time.
    member _.Root : LinkedTrieNode option = Some root
