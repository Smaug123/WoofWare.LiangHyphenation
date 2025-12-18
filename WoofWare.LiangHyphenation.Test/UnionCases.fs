namespace WoofWare.LiangHyphenation.Test

open FSharp.Reflection

[<RequireQualifiedAccess>]
module UnionCases =

    /// Reflectively obtains all cases of a discriminated union.
    /// Throws if any case contains data (only supports DUs with no data in any case).
    [<RequiresExplicitTypeArguments>]
    let all<'a> () : 'a list =
        let ty = typeof<'a>

        if not (FSharpType.IsUnion ty) then
            failwithf "Type %s is not a discriminated union" ty.FullName

        let cases = FSharpType.GetUnionCases ty

        cases
        |> Array.map (fun case ->
            let fields = case.GetFields ()

            if fields.Length > 0 then
                failwithf
                    "Union case %s.%s has %d field(s), but only fieldless cases are supported"
                    ty.FullName
                    case.Name
                    fields.Length

            FSharpValue.MakeUnion (case, [||]) :?> 'a
        )
        |> Array.toList
