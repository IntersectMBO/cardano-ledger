{ iohkCardanoLedgerSpecsPackages ? import ../default.nix {}
, pkgs ? iohkCardanoLedgerSpecsPackages.pkgs
, iohkLib ? iohkCardanoLedgerSpecsPackages.iohkLib
}:

iohkLib.haskellBuildUtils.stackRebuild {
  script = ./rebuild.hs;
  buildTools = [];
  libs = ps: [];
  shell = import ../nix/stack-shell.nix {};
}
