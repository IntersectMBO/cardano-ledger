{ pkgs ? import ../nix {}
, commonLib ? pkgs.commonLib
}:

commonLib.haskellBuildUtils.stackRebuild {
  script = ./rebuild.hs;
  buildTools = [];
  libs = ps: [];
  shell = import ../nix/stack-shell.nix;
}
