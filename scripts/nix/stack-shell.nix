{ system ? builtins.currentSystem
, config ? {}
, pkgs ? import (import ./fetch-nixpkgs.nix) { inherit system config; }
}:

with pkgs;

haskell.lib.buildStackProject {
  name = "cardano-chain-env";
  buildInputs = [ zlib openssl git ];
  ghc = haskell.packages.ghc865.ghc;
}
