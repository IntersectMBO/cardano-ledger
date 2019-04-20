let
  pkgs = import ../../../default.nix {};
in
  pkgs.nix-tools.libs.cs-ledger
