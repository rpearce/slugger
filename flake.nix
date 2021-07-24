{
  description = "slugger: Clean URI slugs for Haskell";

  nixConfig.bash-prompt = "[nix]λ ";

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
        config = {};

        compilerVersion = "884";
        compiler = "ghc" + compilerVersion;
        overlays = [
          (final: prev: {
            haskell-language-server = prev.haskell-language-server.override {
              supportedGhcVersions = [ compilerVersion ];
            };

            myHaskellPackages = prev.haskell.packages.${compiler}.override {
              overrides = hpFinal: hpPrev: {
                slugger = hpPrev.callCabal2nix "slugger" ./. {};
              };
            };
          })
        ];

        pkgs = import nixpkgs { inherit config overlays system; };
      in rec {
        defaultPackage = packages.slugger;
        defaultApp = apps.slugger;

        packages = with pkgs.myHaskellPackages; { inherit slugger; };

        apps.slugger = flake-utils.lib.mkApp {
          drv = packages.slugger;
        };

        devShell = pkgs.myHaskellPackages.shellFor {
          packages = p: [
            p.slugger
          ];

          buildInputs = with pkgs.myHaskellPackages; [
            slugger

            cabal-install
            hlint                        # https://github.com/ndmitchell/hlint
            #ormolu                       # https://github.com/tweag/ormolu
            pkgs.haskell-language-server # https://github.com/haskell/haskell-language-server
          ];

          withHoogle = true;
        };
      }
    );
}
