############################################################################
# Extra Haskell packages which we build with haskell.nix, but which aren't
# part of our project's package set themselves.
#
# Cribbed from https://github.com/input-output-hk/plutus/blob/master/nix/haskell-extra.nix
############################################################################

{ pkgs, index-state, checkMaterialization }:
let compiler-nix-name = "ghc8102";
in {
  inherit (
    let hspkgs = pkgs.haskell-nix.cabalProject {
        src = pkgs.fetchFromGitHub {
          name = "haskell-language-server";
          owner = "haskell";
          repo = "haskell-language-server";
          rev = "0.5.0";
          sha256 = "0vkh5ff6l5wr4450xmbki3cfhlwf041fjaalnwmj7zskd72s9p7p";
          fetchSubmodules = true;
        };
        lookupSha256 = { location, tag, ... } : {
          "https://github.com/bubba/brittany.git"."c59655f10d5ad295c2481537fc8abf0a297d9d1c" = "1rkk09f8750qykrmkqfqbh44dbx1p8aq1caznxxlw8zqfvx39cxl";
          }."${location}"."${tag}";
        inherit compiler-nix-name index-state checkMaterialization;
        # Plan issues with the benchmarks, can try removing later
        configureArgs = "--disable-benchmarks";
        # Invalidate and update if you change the version
        plan-sha256 = "1vyriqi905kl2yrx1xg04cy11wfm9nq1wswny7xm1cwv03gyj6y8";
        modules = [{
          # Tests don't pass for some reason, but this is a somewhat random revision.
          packages.haskell-language-server.doCheck = false;
        }];
      };
    in { haskell-language-server = hspkgs.haskell-language-server; hie-bios = hspkgs.hie-bios; })
  hie-bios haskell-language-server;
}
