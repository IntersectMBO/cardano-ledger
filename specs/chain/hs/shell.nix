let
  pkgs = import ../../../pkgs.nix;
in pkgs.lib.overrideDerivation ((import ./.).env) (old: {
  nativeBuildInputs = old.nativeBuildInputs ++ [ pkgs.cabal-install pkgs.haskell.packages.ghc861.ghcid ];
})
