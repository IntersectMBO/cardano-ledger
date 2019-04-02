let
  pkgs = import ../../../default.nix {};
in
  pkgs.nix-tools.libs.small-steps
