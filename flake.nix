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

                hspec = hpPrev.callHackage "hspec" "2.8.1" {}; # see slugger.cabal build-depends
                hspec-core = hpPrev.callHackage "hspec-core" "2.8.1" {}; # because hspec
                hspec-discover = hpPrev.callHackage "hspec-discover" "2.8.1" {}; # because hspec
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
            slugger

            cabal-install
            hlint                        # https://github.com/ndmitchell/hlint
            ormolu                       # https://github.com/tweag/ormolu
            pkgs.haskell-language-server # https://github.com/haskell/haskell-language-server
          ];

          withHoogle = true;
        };
      }
    );
}


#{
#  description = "slugger: Clean URI slugs for Haskell";

#  nixConfig.bash-prompt = "[nix]\\e\[38;5;172mλ \\e\[m";

#  inputs = {
#    haskellNix.url = "github:input-output-hk/haskell.nix";

#    nixpkgs.follows = "haskellNix/nixpkgs-unstable";

#    flake-utils = {
#      url = "github:numtide/flake-utils";
#      inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
#    };
#  };

#  outputs = { flake-utils, haskellNix, nixpkgs, self }:
#    flake-utils.lib.eachDefaultSystem (system:
#      let
#        projectOverlay = (final: prev: {
#          haskell-language-server = prev.haskell-language-server.override {
#            supportedGhcVersions = [ "884" ];
#          };

#          sluggerProject = prev.haskell-nix.project' {
#            src = ./.;
#            compiler-nix-name = "ghc884";

#            shell.tools = {
#              cabal = {};
#              hlint = {};
#              haskell-language-server = {};
#            };
#          };
#        });

#        config = {};
#        overlays = [ haskellNix.overlay projectOverlay ];
#        pkgs = import nixpkgs { inherit config overlays system; };
#        flake = pkgs.sluggerProject.flake {};
#      in flake // {
#      }
#    );
#}
