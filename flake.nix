{
  description = "slugger: Clean URI slugs for Haskell";

  nixConfig.bash-prompt = "[nix]\\e\[38;5;172mλ \\e\[m";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-21.05";

    flake-utils = {
      url = "github:numtide/flake-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { flake-utils, nixpkgs, self }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        compiler = "ghc884";
        config = {};
        overlays = [
          (final: prev: {
            myHaskellPackages = prev.haskell.packages.${compiler}.override {
              overrides = hpFinal: hpPrev: {
                slugger = hpPrev.callCabal2nix "slugger" ./. {};
              };
            };
          })
        ];
        pkgs = import nixpkgs { inherit config overlays system; };
      in rec {
        packages = with pkgs.myHaskellPackages; [ slugger ];

        devShell = pkgs.myHaskellPackages.shellFor {
          packages = p: [
            p.slugger
          ];

          buildInputs = with pkgs.myHaskellPackages; [
            cabal-install
            slugger

            # Helpful tools for `nix develop` shells
            #
            #ghcid                   # https://github.com/ndmitchell/ghcid
            haskell-language-server # https://github.com/haskell/haskell-language-server
            hlint                   # https://github.com/ndmitchell/hlint
            #ormolu                  # https://github.com/tweag/ormolu
          ];

          #withHoogle = true;
        };
      }
    );
}
