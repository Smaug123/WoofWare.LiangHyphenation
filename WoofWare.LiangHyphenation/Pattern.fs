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
    ///
    /// The exception is lower-cased (as TeX does for \hyphenation entries), because `Hyphenation.hyphenate`
    /// lower-cases the word before matching; an exception that kept its case would compile to a pattern that
    /// could never match. Each hyphen must sit strictly between two letters: a leading, trailing, or doubled
    /// hyphen marks a break at (or before, or after) a word boundary, which is meaningless, so it is rejected
    /// rather than silently dropped or relocated.
    ///
    /// The remaining characters become word content. They must not be pattern metacharacters: ASCII digits
    /// ('0'..'9') are priority markers and '.' is the word-boundary marker. Because this function builds a
    /// pattern *string* that is re-parsed (by `Pattern.parse`), a digit or '.' left in the content would not
    /// survive the round-trip -- a digit would be re-read as a priority (so "a-1b" would compile a pattern
    /// for the word "ab") and a '.' would inject a spurious interior word boundary. Such characters are
    /// rejected rather than silently corrupting the compiled exception.
    let exceptionToPattern (exception' : string) : string =
        // Lower-case to match the lower-casing that `hyphenate` applies to the word.
        let exception' = exception'.ToLowerInvariant ()

        let sb = StringBuilder ()
        sb.Append '.' |> ignore<StringBuilder>

        let mutable isFirst = true
        let mutable nextPriorityIs9 = false

        for c in exception' do
            if c = '-' then
                // The hyphen means the NEXT inter-letter position allows hyphenation. It must follow a
                // letter (not be the first character, and not follow another hyphen): otherwise it marks
                // a break at a word boundary, which a pattern cannot express.
                if isFirst then
                    failwith
                        $"Hyphenation exception '%s{exception'}' must not begin with a hyphen: a hyphen marks a break between two letters."

                if nextPriorityIs9 then
                    failwith
                        $"Hyphenation exception '%s{exception'}' must not contain consecutive hyphens: a hyphen marks a break between two letters."

                nextPriorityIs9 <- true
            else
                // This character becomes word content, so it must not collide with a pattern metacharacter.
                // Digits are priority markers and '.' is the word-boundary marker; either would be silently
                // misinterpreted when the generated pattern string is re-parsed (see the doc comment).
                if c >= '0' && c <= '9' then
                    failwith
                        $"Hyphenation exception '%s{exception'}' must not contain the digit '%c{c}': digits are reserved as priority markers in hyphenation patterns."

                if c = '.' then
                    failwith
                        $"Hyphenation exception '%s{exception'}' must not contain '.': it is reserved as the word-boundary marker in hyphenation patterns."

                // Add priority before each letter (except the first)
                if not isFirst then
                    if nextPriorityIs9 then
                        sb.Append '9' |> ignore<StringBuilder>
                        nextPriorityIs9 <- false
                    else
                        sb.Append '8' |> ignore<StringBuilder>

                sb.Append c |> ignore<StringBuilder>
                isFirst <- false

        // A hyphen with no following letter marks a break after the last letter, i.e. at the trailing
        // word boundary; reject it rather than silently dropping it.
        if nextPriorityIs9 then
            failwith
                $"Hyphenation exception '%s{exception'}' must not end with a hyphen: a hyphen marks a break between two letters."

        sb.Append '.' |> ignore
        sb.ToString ()
