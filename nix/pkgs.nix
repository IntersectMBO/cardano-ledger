# our packages overlay
pkgs: _: with pkgs; {
  cardanoLedgerSpecsHaskellPackages = import ./haskell.nix {
    inherit config
      lib
      stdenv
      haskell-nix
      buildPackages
      ;
  };
}
