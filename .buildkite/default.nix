{ iohkSkeletonPackages ? import ../default.nix {}
, pkgs ? iohkSkeletonPackages.pkgs
, iohkLib ? iohkSkeletonPackages.iohkLib
}:

iohkLib.haskellBuildUtils.stackRebuild {
  script = ./rebuild.hs;
  buildTools = [];
  libs = ps: [];
  shell = import ../nix/stack-shell.nix {};
}
