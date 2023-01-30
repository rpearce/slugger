{
  description = "slugger: Clean URI slugs for Haskell";

  nixConfig = {
    allow-import-from-derivation = "true";
    bash-prompt = "[nix]λ ";
    extra-substituters = [
      "https://cache.iog.io"
      "https://cache.zw3rk.com" # https://github.com/input-output-hk/haskell.nix/issues/1408
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk="
    ];
  };

  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [ haskellNix.overlay
          (final: prev: {
            sluggerProject = final.haskell-nix.project' {
              src = ./.;
              compiler-nix-name = "ghc925";
              shell.buildInputs = [
                slugger-cli
              ];
              #shell.tools = {
              #  cabal = "latest";
              #  hlint = "latest";
              #  haskell-language-server = "latest";
              #};
            };
          })
        ];

        pkgs = import nixpkgs {
          inherit overlays system;
          inherit (haskellNix) config;
        };

        flake = pkgs.sluggerProject.flake {};

        executable = "slugger:exe:slugger";

        slugger-cli = flake.packages.${executable};

      in flake // rec {
        apps = {
          default = flake-utils.lib.mkApp {
            drv = slugger-cli;
          };
        };

        packages = {
          default = slugger-cli;
        };
      }
    );
}
