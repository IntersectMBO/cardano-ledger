let
  pkgs = import ../../../pkgs.nix;
in
  pkgs.haskell.packages.ghc861.cs-ledger
