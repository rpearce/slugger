# slugger

Clean URI slugs for Haskell

[![built with nix](https://builtwithnix.org/badge.svg)](https://builtwithnix.org)

## Installation

WIP

## Usage

### Library

WIP

### Executable

WIP

## Development

Get into a nix dev environment:

```sh
λ nix develop
[nix]λ
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
