namespace WoofWare.LiangHyphenation

open System

[<AutoOpen>]
module internal Tracing =
#if DEBUG
    /// Debug tracing controlled by WOOFWARE_LIANG_HYPHENATION_DEBUG environment variable.
    /// Set to "1" or "true" to enable tracing to stderr.
    /// Only available in DEBUG builds.
    let private debugEnabled =
        match Environment.GetEnvironmentVariable "WOOFWARE_LIANG_HYPHENATION_DEBUG" with
        | null -> false
        | s -> s = "1" || String.Equals(s, "true", StringComparison.OrdinalIgnoreCase)

    let inline trace (msgFn: unit -> string) =
        if debugEnabled then
            Console.Error.WriteLine(msgFn ())
#else
    let inline trace (_msgFn: unit -> string) = ()
#endif
