{ pkgs ? import <nixpkgs> {} }:

with pkgs.lib;

{
  # Function that identifies packages from your project.
  isCardanoLedgerSpecs = package:
    (elem package.identifier.name ["cs-blockchain" "cs-ledger" "small-steps" "non-integer" "delegation"]);

  # fixme: upstream to iohk-nix
  collectComponents = group: packageSel: haskellPackages:
    (mapAttrs (_: package: package.components.${group} // { recurseForDerivations = true; })
     (filterAttrs (name: package: (package.isHaskell or false) && packageSel package) haskellPackages))
    // { recurseForDerivations = true; };
}
