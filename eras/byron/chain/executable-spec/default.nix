let
  pkgs = import ../../../default.nix {};
in
  pkgs.libs.byron-spec-chain
