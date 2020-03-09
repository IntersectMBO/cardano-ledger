# our packages overlay
pkgs: _: with pkgs; {
  cardanoLedgerSpecsHaskellPackages = import ./haskell.nix {
    inherit config
      lib
      stdenv
      pkgs
      haskell-nix
      buildPackages
      ;
  };
}
