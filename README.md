# WoofWare.LiangHyphenation

[![NuGet version](https://img.shields.io/nuget/v/WoofWare.LiangHyphenation.svg?style=flat-square)](https://www.nuget.org/packages/WoofWare.LiangHyphenation)
[![GitHub Actions status](https://github.com/Smaug123/WoofWare.LiangHyphenation/actions/workflows/dotnet.yaml/badge.svg)](https://github.com/Smaug123/WoofWare.LiangHyphenation/actions?query=branch%3Amain)
[![License file](https://img.shields.io/github/license/Smaug123/WoofWare.LiangHyphenation)](./LICENSE.md)

<picture>
  <source media="(prefers-color-scheme: dark)" srcset="logos/dark.svg">
  <source media="(prefers-color-scheme: light)" srcset="logos/light.svg">
  <img alt="Project logo: minimalistic black-and-white face of a cartoon Shiba Inu, composed almost entirely of horizontal lines." src="logos/light.svg" width="300">
</picture>

The Liang hyphenation algorithm.

# Structure

The library provides two namespaces: `WoofWare.LiangHyphenation`, which contains functions for performing hyphenation,
and `WoofWare.LiangHyphenation.Construction`, which contains functions for manipulating and creating the efficient data structures holding hyphenation data.

As a consumer, using hyphenation out of the box, you should simply call methods from `WoofWare.LiangHyphenation`'s `Hyphenation` module.

## Usage

```fsharp
open WoofWare.LiangHyphenation

// Load the hyphenation data for your language (cache this yourself if desired).
let trie : PackedTrie = LanguageData.load KnownLanguage.EnGb

// Get the priority array for a word.
let priorities : byte[] = Hyphenation.hyphenate trie "hyphenation"
// priorities = [| 2; 3; 0; 4; 4; 1; 1; 2; 4; 2 |]

// Or get just the indices where hyphenation is allowed.
let points : int[] = Hyphenation.getHyphenationPoints trie "hyphenation"
// points = [| 1; 5; 6 |]
// This corresponds to: hy-phen-a-tion
```

### Algorithm overview

The Liang algorithm works by overlaying a dictionary of *patterns* over each word, each pattern identifying candidate word breaks.
A pattern defines some collection of candidate breaks within a string, and a priority for each candidate (the priority can also specify "do *not* break here" with some strength).
Most of the patterns in the dictionary won't match any given word, but it's a big dictionary, so most words are covered by multiple patterns.

The convention is that odd-valued priorities indicate "break here", and even-valued priorities indicate "don't break here".
Higher-magnitude priorities win over lower-magnitude ones.

Example patterns:

* `ain5ders`: consider breaking `ain-ders`
* `a4ju`: consider *not* breaking `a-ju`

In the en-gb dictionary, for example, two of the patterns are `aleg4` and `leg1a`.
(That is, "don't break after `aleg`, priority 4", and "do break `leg-a`, priority 1".)
Imagine we had only those two rules in the dictionary; then the dictionary would choose *not* to break `paralegal`, because the priority `4` on "don't break after `aleg`" beats the priority `1` on "do break `leg-a`".

The `hyphenate` function returns an array of priority values, one for each inter-letter position in the word (so it returns an array of length `string.Length - 1`).

For example, in `"hyphenation"`:

| Position | Letters | Priority | Hyphenate? |
|----------|---------|----------|------------|
| 0 | h-y | 2 | No (even) |
| 1 | y-p | 3 | **Yes** (odd) |
| 2 | p-h | 0 | No (even) |
| 3 | h-e | 4 | No (even) |
| 4 | e-n | 4 | No (even) |
| 5 | n-a | 1 | **Yes** (odd) |
| 6 | a-t | 1 | **Yes** (odd) |
| 7 | t-i | 2 | No (even) |
| 8 | i-o | 4 | No (even) |
| 9 | o-n | 2 | No (even) |

The odd positions (1, 5, 6) give: **hy-phen-a-tion**.

# Licence

The patterns defining hyphenation patterns are derived from the [hyph-utf8 package](https://ctan.org/pkg/hyph-utf8?lang=en), and are in [WoofWare.LiangHyphenation.Test/Patterns](./WoofWare.LiangHyphenation.Test/Patterns).
The text files which we consume are a bit unclear about the licenses which apply to them; I've taken them to be MIT licensed just as are the `.tex` files defining the same patterns.
Copies can be found in the [LICENSES](./LICENSES/) folder.

WoofWare.LiangHyphenation is licenced to you under the MIT licence; see [LICENSE.md](./LICENSE.md).
