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

# Licence

The patterns defining hyphenation patterns are derived from the [hyph-utf8 package](https://ctan.org/pkg/hyph-utf8?lang=en), and are in [WoofWare.LiangHyphenation.Test/Patterns](./WoofWare.LiangHyphenation.Test/Patterns).
The text files which we consume are a bit unclear about the licenses which apply to them; I've taken them to be MIT licensed just as are the `.tex` files defining the same patterns.
Copies can be found in the [LICENSES](./LICENSES/) folder.

WoofWare.LiangHyphenation is licenced to you under the MIT licence; see [LICENSE.md](./LICENSE.md).
