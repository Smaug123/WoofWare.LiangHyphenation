namespace WoofWare.LiangHyphenation.Construction

open System.Text

/// Module for parsing the standard Liang hyphenation patterns like ".hy3p".
[<RequireQualifiedAccess>]
module Pattern =
    /// Parse a Liang hyphenation pattern (e.g., ".hy3p" or "4ab1c")
    /// Returns a sequence of (char, priority) pairs.
    /// Priority applies to the position *before* the character.
    let parse (pattern : string) : struct (char * byte) array =
        let result = ResizeArray<struct (char * byte)> ()
        let mutable pendingPriority = 0uy

        for c in pattern do
            if c >= '0' && c <= '9' then
                pendingPriority <- byte (int c - int '0')
            else
                result.Add (struct (c, pendingPriority))
                pendingPriority <- 0uy

        // Handle trailing priority (applies after last char)
        if pendingPriority > 0uy && result.Count > 0 then
            // Trailing priority - we need to handle this specially
            // For now, store as a special marker or handle in insertion
            ()

        result.ToArray ()

    /// Convert an exception (e.g., "uni-ver-sity") to a pattern string.
    /// Exceptions use hyphens to mark allowed hyphenation points.
    /// The resulting pattern uses priority 9 (odd, forces hyphenation) at hyphen positions
    /// and priority 8 (even, suppresses hyphenation) at non-hyphen positions.
    /// Word boundary markers (.) are added.
    ///
    /// Example: "uni-ver-sity" → ".u8n8i9v8e8r9s8i8t8y."
    /// The 9s appear before 'v' and 's', allowing hyphenation at uni-ver-sity.
    let exceptionToPattern (exception' : string) : string =
        let sb = StringBuilder ()
        sb.Append '.' |> ignore<StringBuilder>

        let mutable isFirst = true
        let mutable nextPriorityIs9 = false

        for c in exception' do
            if c = '-' then
                // The hyphen means the NEXT inter-letter position allows hyphenation
                nextPriorityIs9 <- true
            else
                // Add priority before each letter (except the first)
                if not isFirst then
                    if nextPriorityIs9 then
                        sb.Append '9' |> ignore<StringBuilder>
                        nextPriorityIs9 <- false
                    else
                        sb.Append '8' |> ignore<StringBuilder>

                sb.Append c |> ignore<StringBuilder>
                isFirst <- false

        sb.Append '.' |> ignore
        sb.ToString ()
