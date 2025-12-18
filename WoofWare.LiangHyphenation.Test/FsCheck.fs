namespace WoofWare.LiangHyphenation.Test

open System
open FsCheck

[<RequireQualifiedAccess>]
module FsCheckConfig =
    let config =
        Config.QuickThrowOnFailure.WithMaxTest(10000).WithQuietOnSuccess(true).WithParallelRunConfig
            {
                MaxDegreeOfParallelism = max 1 (Environment.ProcessorCount / 2)
            }
