namespace WoofWare.LiangHyphenation.Construction

open System.Text

/// A parsed Liang hyphenation pattern.
/// The priority vector has length = Chars.Length + 1, covering all inter-character positions.
[<Struct>]
type ParsedPattern =
    {
        /// The characters in the pattern (e.g., ".hy3p" -> [|'.'; 'h'; 'y'; 'p'|])
        Chars : char array
        /// Priority values at each inter-character position.
        /// Length = Chars.Length + 1.
        /// Priorities[i] = priority before Chars[i]; Priorities[Chars.Length] = priority after last char.
        Priorities : byte array
    }

/// Module for parsing the standard Liang hyphenation patterns like ".hy3p".
[<RequireQualifiedAccess>]
module Pattern =
    /// Parse a Liang hyphenation pattern (e.g., ".hy3p" or "4ab1c" or ".ace4")
    /// Returns the characters and a priority vector of length Chars.Length + 1.
    let parse (pattern : string) : ParsedPattern =
        let chars = ResizeArray<char> ()
        let priorities = ResizeArray<byte> ()
        let mutable pendingPriority = 0uy

        for c in pattern do
            if c >= '0' && c <= '9' then
                pendingPriority <- byte (int c - int '0')
            else
                priorities.Add pendingPriority
                chars.Add c
                pendingPriority <- 0uy

        // Add trailing priority (after last char)
        priorities.Add pendingPriority

        {
            Chars = chars.ToArray ()
            Priorities = priorities.ToArray ()
        }

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
