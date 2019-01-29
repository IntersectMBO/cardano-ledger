{ system ? builtins.currentSystem
, config ? {}
, pkgs ? import (import ./fetch-nixpkgs.nix) { inherit system config; }
}:

with pkgs;

haskell.lib.buildStackProject {
  name = "cardano-chain-env";
  buildInputs = [ zlib openssl git git-lfs ];
  ghc = haskell.packages.ghc863.ghc;
  GIT_LFS_SKIP_SMUDGE = "1";
}
