# Provide a custom-built doctest that's compatible with haskell.nix

{ config, nixpkgs }:

let
  doctest = nixpkgs.haskell-nix.hackage-package {
    name = "doctest";
    version = "0.24.3";
    inherit (config) compiler-nix-name;
    modules = [{
      packages = {
        # This enables doctest to find ghc etc. via environment variables
        ghc-paths.patches = nixpkgs.haskellPackages.ghc-paths.patches;
      };
    }];
  };
in { doctest = doctest.getComponent "exe:doctest"; }
