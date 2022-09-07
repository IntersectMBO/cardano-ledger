let
  pkgs = import ../../../default.nix {};
in
  pkgs.libs.cardano-ledger-binary
