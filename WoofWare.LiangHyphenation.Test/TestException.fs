namespace WoofWare.LiangHyphenation.Test

open NUnit.Framework
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open WoofWare.LiangHyphenation
open WoofWare.LiangHyphenation.Construction

[<TestFixture>]
module TestException =

    [<Test>]
    let ``Exception compiles to the expected pattern`` () =
        // "uni-ver-sity" allows breaks at uni-ver and ver-sity (priority 9, odd) and suppresses
        // every other inter-letter position (priority 8, even).
        Pattern.exceptionToPattern "uni-ver-sity" |> shouldEqual ".u8n8i9v8e8r9s8i8t8y."

    [<Test>]
    let ``Exception without hyphens suppresses all breaks`` () =
        // As in TeX, \hyphenation{foo} with no hyphens means "never break this word".
        Pattern.exceptionToPattern "foo" |> shouldEqual ".f8o8o."

    [<Test>]
    let ``Exception is lowercased like TeX hyphenation entries`` () =
        // hyphenate lowercases the word before matching, so a title-case exception that kept its case
        // would compile to a pattern that can never match -- a silent no-op. We lowercase to match.
        Pattern.exceptionToPattern "Uni-Ver"
        |> shouldEqual (Pattern.exceptionToPattern "uni-ver")

        Pattern.exceptionToPattern "Uni-Ver" |> shouldEqual ".u8n8i9v8e8r."

    [<Test>]
    let ``Leading hyphen is rejected`` () =
        // A leading hyphen marks a break before the first letter, i.e. at the word boundary. Previously
        // this silently relocated the break inside the word ("-ab" produced ".a9b.", same as "a-b").
        let exn =
            Assert.Throws<exn> (fun () -> Pattern.exceptionToPattern "-ab" |> ignore<string>)

        exn.Message.Contains "-ab" |> shouldEqual true

    [<Test>]
    let ``Trailing hyphen is rejected`` () =
        // A trailing hyphen marks a break after the last letter, at the word boundary. Previously this
        // was silently dropped ("ab-" produced ".a8b.").
        let exn =
            Assert.Throws<exn> (fun () -> Pattern.exceptionToPattern "ab-" |> ignore<string>)

        exn.Message.Contains "ab-" |> shouldEqual true

    [<Test>]
    let ``Consecutive hyphens are rejected`` () =
        // A doubled hyphen marks two breaks at one inter-letter position; the second was silently
        // collapsed into the first ("a--b" produced ".a9b.", same as "a-b").
        let exn =
            Assert.Throws<exn> (fun () -> Pattern.exceptionToPattern "a--b" |> ignore<string>)

        exn.Message.Contains "a--b" |> shouldEqual true

    /// An exception forces breaks at exactly the positions it marks with a hyphen and suppresses every
    /// other inter-letter position, regardless of the case in which it is supplied. This is the contract
    /// of `AddException`, and it pins down both the lowercasing fix (uppercase exceptions must still take
    /// effect) and the positional correctness of `exceptionToPattern`.
    [<Test>]
    let ``Exception forces breaks at exactly its hyphen positions, case-insensitively`` () =
        let gen =
            gen {
                let! length = Gen.choose (2, 12)
                let! letters = Gen.listOfLength length (Gen.choose (int 'a', int 'z') |> Gen.map char)
                // One flag per inter-letter position (0 .. length-2): is there a hyphen there?
                let! markFlags = Gen.listOfLength (length - 1) (Gen.elements [ true ; false ])
                // One flag per letter: do we supply it upper-cased?
                let! upperFlags = Gen.listOfLength length (Gen.elements [ true ; false ])
                return (letters, markFlags, upperFlags)
            }
            |> Arb.fromGen

        let property (letters : char list, markFlags : bool list, upperFlags : bool list) =
            let lettersArr = List.toArray letters
            let word = System.String lettersArr

            let marks =
                markFlags
                |> List.mapi (fun i m -> i, m)
                |> List.filter snd
                |> List.map fst
                |> Set.ofList

            // Build the exception string, inserting a hyphen after letter i when position i is marked,
            // and upper-casing letters according to upperFlags.
            let sb = System.Text.StringBuilder ()

            for i = 0 to lettersArr.Length - 1 do
                let c =
                    if upperFlags.[i] then
                        System.Char.ToUpperInvariant lettersArr.[i]
                    else
                        lettersArr.[i]

                sb.Append c |> ignore<System.Text.StringBuilder>

                if i < markFlags.Length && markFlags.[i] then
                    sb.Append '-' |> ignore<System.Text.StringBuilder>

            let trie =
                let builder = PackedTrieBuilder ()
                builder.AddException (sb.ToString ())
                builder.Build ()

            let points = Hyphenation.getHyphenationPoints trie word |> Set.ofArray
            points |> shouldEqual marks

        Check.One (FsCheckConfig.config, Prop.forAll gen property)
