{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = {};
    package = {
      specVersion = "2.0";
      identifier = { name = "cardano-mainnet-mirror"; version = "0.1.0.0"; };
      license = "MIT";
      copyright = "";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "";
      url = "";
      synopsis = "A convenient wrapper for the mirror of Cardano mainnet";
      description = "This package provides a list of FilePaths to the Cardano mainnet data files";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [ (hsPkgs.base) (hsPkgs.directory) (hsPkgs.filepath) ];
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/cardano-mainnet-mirror";
      rev = "0232971dddd5cd235c5a0c18d7b4f342a887276f";
      sha256 = "16yqqwypl4y6hzagsxz0k1hbw1lkk2bdiyj0kl32g24ydwp12ggd";
      });
    }