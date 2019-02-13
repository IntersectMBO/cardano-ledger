{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = { development = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "cs-blockchain"; version = "0.1.0.0"; };
      license = "MIT";
      copyright = "";
      maintainer = "formal.methods@iohk.io";
      author = "IOHK Formal Methods Team";
      homepage = "https://github.com/input-output-hk/cardano-chain";
      url = "";
      synopsis = "Executable specification of the Cardano blockchain";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.base)
          (hsPkgs.bytestring)
          (hsPkgs.containers)
          (hsPkgs.cryptonite)
          (hsPkgs.cs-ledger)
          (hsPkgs.hedgehog)
          (hsPkgs.lens)
          (hsPkgs.memory)
          (hsPkgs.small-steps)
          ];
        };
      tests = {
        "chain-spec-test" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.data-ordlist)
            (hsPkgs.hedgehog)
            (hsPkgs.lens)
            (hsPkgs.tasty)
            (hsPkgs.tasty-hunit)
            (hsPkgs.tasty-hedgehog)
            (hsPkgs.cs-blockchain)
            (hsPkgs.small-steps)
            ];
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/fm-ledger-rules";
      rev = "bf059d1d593e7ef9f3b983a0c904e7bb81362af9";
      sha256 = "0rdyb0bfk69ndb86m91m9fzcfnhaj4q5yw0cgfj0ap4vmirz92cs";
      });
    postUnpack = "sourceRoot+=/specs/chain/hs; echo source root reset to \$sourceRoot";
    }