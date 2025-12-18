namespace WoofWare.LiangHyphenation

open System

[<RequireQualifiedAccess>]
module internal Object =

    let referenceEquals<'a when 'a : not struct> (x : 'a) (y : 'a) : bool =
        // fsharpanalyzer: ignore-line-next WOOF-REFEQUALS
        Object.ReferenceEquals (x, y)
