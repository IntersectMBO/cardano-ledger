{ sources }:
# our packages overlay
pkgs: _:
with pkgs; {
  cardanoLedgerSpecsHaskellPackages = import ./haskell.nix {
    inherit config lib stdenv pkgs haskell-nix buildPackages;
    CHaP = sources.cardano-haskell-packages;
  };

  cbor-diag = pkgs.callPackage ./pkgs/cbor-diag { };
  cddl = pkgs.callPackage ./pkgs/cddl { };
}
