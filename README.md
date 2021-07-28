# slugger

Clean URI slugs for Haskell

Convert multi-language text to a US-ASCII, lowercase, hyphenated, URI-friendly "slug".

[![built with nix](https://builtwithnix.org/badge.svg)](https://builtwithnix.org)

## Installation

WIP

## Usage

### Library

There are `Data.Text` and `Data.String` library interfaces to `slugger` that
have plenty of examples in [`the test file`](./test/SluggerTest.hs), and here
are some simple examples.

Example of `Data.String.Slugger`:

```haskell
import qualified Data.String.Slugger as SluggerString

SluggerString.toSlug "Hey there,   world!"
-- "hey-there-world"

SluggerString.toSlug "GARÇON - déjà , Forêt — Zoë"
-- "garcon-deja-foret-zoe"
```

Example of `Data.Text.Slugger`:

```haskell
import qualified Data.Text as T
import qualified Data.Text.Slugger as SluggerText

SluggerText.toSlug (T.pack "Hey there,   world!")
-- "hey-there-world"

SluggerText.toSlug (T.pack "GARÇON - déjà , Forêt — Zoë")
-- "garcon-deja-foret-zoe"
```

### Executable

```sh
λ slugger "Hey there,   world!"
hey-there-world

λ slugger "Pijamalı hasta yağız şoföre çabucak güvendi"
pijamali-hasta-yagiz-sofore-cabucak-guvendi
```

## Language Support

These are the languages that are currently tested and therefore marked as
supported. Contributions are welcome for more extensive tests or tests for
additional languages.

- [x] Dansk     (Danish)
- [x] Deutsche  (German)
- [x] English
- [x] Español   (Spanish)
- [x] Français  (French)
- [x] Íslenskur (Icelandic)
- [x] Polskie   (Polish)
- [x] Svenska   (Swedish)
- [x] Türk      (Turkish)

## Development

Try the project executable via a nix flake app:

```sh
λ nix run . "Testing 1,2,3"
testing-1-2-3
```

Get into a nix dev environment:

```sh
λ nix develop
[nix]λ
```

Build the project:

```sh
[nix]λ nix build
```

Run the tests from the shell:

```sh
[nix]λ cabal test --test-show-details=streaming --test-option=--color
```

Run the tests from GHCi:

```sh
[nix]λ cabal repl slugger-test
[ghci]λ :main
# test output prints here

# make some changes, then...
[ghci]λ :reload
[ghci]λ :main
```
