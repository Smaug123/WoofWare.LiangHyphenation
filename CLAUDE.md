# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build Commands

```bash
# Build (uses Nix for reproducible environment)
nix develop --command dotnet build

# Build release
nix develop --command dotnet build --configuration Release

# Run tests
nix develop --command dotnet test

# Run a single test (see "NUnit bugs" for rationale)
nix develop --command dotnet build && nix develop --command dotnet woofware.nunittestrunner WoofWare.LiangHyphenation.Test/bin/Debug/net9.0/WoofWare.LiangHyphenation.Test.dll --filter "FullyQualifiedName~TestHyphenation"

# Format F# code
nix run .#fantomas -- .

# Check F# formatting
nix run .#fantomas -- --check .

# Format Nix code
nix develop --command alejandra .

# Run analyzers
./analyzers/run.sh

# Pack NuGet
nix develop --command dotnet pack --configuration Release
```

### NUnit bugs

NUnit's filtering is pretty borked.
You can't apply filters that contain special characters in the test name (like a space character).
You have to do e.g. `FullyQualifiedName~singleword` rather than `FullyQualifiedName~single word test`, but this only works on tests whose names are single words to begin with.

Instead of running `dotnet test`, you can perform a build (`dotnet build`) and then run the custom test runner.
The test runner accepts an optional `--filter` arg that takes the same filter syntax as `dotnet test`, but actually parses it correctly: test names can contain spaces.
(The most foolproof way to provide test names to WoofWare.NUnitTestRunner is by XML-encoding: e.g. `FullyQualifiedName="MyNamespace.MyTestsClass&lt;ParameterType1%2CParameterType2&gt;.MyTestMethod"`. The `~` query operator is also supported.)

## Architecture

This is an F# implementation of the Liang hyphenation algorithm.

### Namespaces

- **`WoofWare.LiangHyphenation`**: Consumer-facing hyphenation API
  - `Hyphenation` module: `hyphenate`, `getHyphenationPoints`, `hyphenateWord`
  - `LanguageData.load`: Loads precomputed packed trie for a `KnownLanguage`
  - `PackedTrie`: The efficient trie data structure for runtime lookup

- **`WoofWare.LiangHyphenation.Construction`**: Pattern compilation (not needed for typical use)
  - `Pattern.parse`: Parses Liang patterns like `.hy3p`
  - `LinkedTrieBuilder`: Mutable trie for construction
  - `PackedTrieBuilder`: Compiles patterns into `PackedTrie`

### Data Structures

The core data structure is `PackedTrie`, a compact trie using:
- 64-bit entries encoding char (16 bits), priority (4 bits), and link (44 bits)
- Dense alphabet mapping for O(1) character lookup
- Serialized with GZip compression as embedded resources (`.bin` files in `Data/`)

### Test Organization

Tests use NUnit with FsCheck for property-based testing. Key test files:
- `TestHyphenationProperty.fs`: Property tests comparing packed trie against naive reference implementation
- `TestHyphenation.fs`: Unit tests for specific patterns
- `TestSurface.fs`: API surface baseline tests

### Measure Types

The codebase uses F# units of measure to distinguish indices:
- `trieIndex`: Index into packed array
- `trieState`: State identifier in trie
- `alphabetIndex`: Dense alphabet index

## Code Style

- Warnings are treated as errors (`TreatWarningsAsErrors`)
- F# formatting via Fantomas (config in `.editorconfig`)
