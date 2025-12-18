namespace WoofWare.LiangHyphenation.Construction

/// A node in the linked trie used during construction.
/// Uses a binary tree encoding: Left = next sibling, Right = first child.
type LinkedTrieNode =
    {
        /// The character this node represents in the trie.
        mutable Char : char
        /// The hyphenation priority value at this position (0 means no priority set).
        mutable Priority : byte
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
    let create (c : char) (priority : byte) : LinkedTrieNode =
        {
            Char = c
            Priority = priority
            Left = None
            Right = None
        }

    /// Find or create a child node for the given character
    let rec findOrAddChild (c : char) (node : LinkedTrieNode) : LinkedTrieNode =
        match node.Right with
        | None ->
            let child = create c 0uy
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
                let sibling = create c 0uy
                node.Left <- Some sibling
                sibling
            | Some left -> findOrAddSibling c left

/// Builder for constructing a linked trie from patterns.
type LinkedTrieBuilder () =
    // Root is the conceptual "start" state; its children are the first pattern characters
    let root = LinkedTrieNode.create '\000' 0uy

    /// Insert a parsed pattern into the trie.
    /// Time: O(m * s) where m is pattern length and s is max sibling count at each trie level.
    /// In the worst case s is bounded by alphabet size, but typically s is small because patterns share prefixes.
    member _.Insert (pattern : struct (char * byte) array) : unit =
        if pattern.Length <> 0 then
            let mutable current = root

            for i = 0 to pattern.Length - 1 do
                let struct (c, priority) = pattern.[i]
                current <- LinkedTrieNode.findOrAddChild c current
                // Update priority if higher (take max)
                if priority > current.Priority then
                    current.Priority <- priority

    /// Get the root node (the conceptual start state).
    /// This is constant-time.
    member _.Root : LinkedTrieNode option = Some root
