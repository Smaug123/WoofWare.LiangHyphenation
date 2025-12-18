namespace WoofWare.LiangHyphenation.Construction

[<RequireQualifiedAccess>]
module Pattern =
    /// Parse a Liang hyphenation pattern (e.g., ".hy3p" or "4ab1c")
    /// Returns a sequence of (char, priority) pairs.
    /// Priority applies to the position *before* the character.
    let parse (pattern: string) : struct (char * byte) array =
        let result = ResizeArray<struct (char * byte)>()
        let mutable pendingPriority = 0uy

        for c in pattern do
            if c >= '0' && c <= '9' then
                pendingPriority <- byte (int c - int '0')
            else
                result.Add(struct (c, pendingPriority))
                pendingPriority <- 0uy

        // Handle trailing priority (applies after last char)
        if pendingPriority > 0uy && result.Count > 0 then
            // Trailing priority - we need to handle this specially
            // For now, store as a special marker or handle in insertion
            ()

        result.ToArray()
