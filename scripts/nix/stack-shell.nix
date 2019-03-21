with import ../../lib.nix;
with pkgs;

haskell.lib.buildStackProject {
  name = "cardano-ledger-env";
  buildInputs = [ zlib openssl git ];
  ghc = haskell.packages.ghc863.ghc;
}
