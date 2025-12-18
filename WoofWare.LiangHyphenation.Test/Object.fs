namespace WoofWare.LiangHyphenation.Test

open System

[<RequireQualifiedAccess>]
module internal Object =

    let referenceEquals<'a when 'a : not struct> (x : 'a) (y : 'a) : bool = Object.ReferenceEquals (x, y)
