namespace WoofWare.LiangHyphenation.Construction

/// A node in the linked trie used during construction.
/// Uses a binary tree encoding: Left = next sibling, Right = first child.
type LinkedTrieNode =
    { mutable Char: char
      mutable Priority: byte
      mutable Left: LinkedTrieNode option // next sibling (different char at same depth)
      mutable Right: LinkedTrieNode option } // first child (next char in pattern)

[<RequireQualifiedAccess>]
module LinkedTrieNode =
    /// Create a new node
    let create (c: char) (priority: byte) : LinkedTrieNode =
        { Char = c
          Priority = priority
          Left = None
          Right = None }

    /// Find or create a child node for the given character
    let rec findOrAddChild (c: char) (node: LinkedTrieNode) : LinkedTrieNode =
        match node.Right with
        | None ->
            let child = create c 0uy
            node.Right <- Some child
            child
        | Some child -> findOrAddSibling c child

    and findOrAddSibling (c: char) (node: LinkedTrieNode) : LinkedTrieNode =
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
type LinkedTrieBuilder() =
    // Root is the conceptual "start" state; its children are the first pattern characters
    let root = LinkedTrieNode.create '\000' 0uy

    /// Insert a parsed pattern into the trie
    member _.Insert(pattern: struct (char * byte) array) : unit =
        if pattern.Length = 0 then
            ()
        else

            let mutable current = root

            for i = 0 to pattern.Length - 1 do
                let struct (c, priority) = pattern.[i]
                current <- LinkedTrieNode.findOrAddChild c current
                // Update priority if higher (take max)
                if priority > current.Priority then
                    current.Priority <- priority

    /// Get the root node (the conceptual start state)
    member _.Root: LinkedTrieNode option = Some root
