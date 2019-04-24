{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = { name = "bulk-chain-validation"; version = "0.1.0.0"; };
      license = "MIT";
      copyright = "2019 IOHK";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "";
      url = "";
      synopsis = "";
      description = "Bulk chain validation";
      buildType = "Simple";
      };
    components = {
      exes = {
        "bulk-chain-validation-exe" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.containers)
            (hsPkgs.cardano-crypto-wrapper)
            (hsPkgs.cardano-ledger)
            (hsPkgs.cardano-mainnet-mirror)
            (hsPkgs.cardano-prelude)
            (hsPkgs.cardano-shell)
            (hsPkgs.formatting)
            (hsPkgs.iohk-monitoring)
            ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../.././validate-mainnet; }