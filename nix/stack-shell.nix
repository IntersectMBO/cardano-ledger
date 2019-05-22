with import ../lib.nix;
with pkgs;

let
  stack-pkgs = import ./.stack.nix;
  compiler = (stack-pkgs.extras {}).compiler.nix-name;

in haskell.lib.buildStackProject {
  name = "cardano-ledger-specs-env";
  buildInputs = [ git ];
  ghc = haskell.packages.${compiler}.ghc;
}
