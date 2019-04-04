with import ../../lib.nix;
with pkgs;

let
  stack-pkgs = import ../../nix/.stack.nix;
  compiler = (stack-pkgs.extras {}).compiler.nix-name;

in haskell.lib.buildStackProject {
  name = "cardano-ledger-env";
  buildInputs = [ zlib openssl git ];
  ghc = haskell.packages.${compiler}.ghc;
}
