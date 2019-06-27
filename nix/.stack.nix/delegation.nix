{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = { development = false; };
    package = {
      specVersion = "1.8";
      identifier = { name = "delegation"; version = "0.1.0.0"; };
      license = "NONE";
      copyright = "";
      maintainer = "formal.methods@iohk.io";
      author = "IOHK Formal Metheds Team";
      homepage = "";
      url = "";
      synopsis = "";
      description = "Delegation Executable Model";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.base)
          (hsPkgs.bytestring)
          (hsPkgs.containers)
          (hsPkgs.cryptonite)
          (hsPkgs.memory)
          (hsPkgs.small-steps)
          (hsPkgs.microlens)
          (hsPkgs.microlens-th)
          (hsPkgs.non-integer)
          (hsPkgs.cs-ledger)
          (hsPkgs.cardano-binary)
          (hsPkgs.cardano-crypto-class)
          ];
        };
      tests = {
        "delegation-test" = {
          depends = (pkgs.lib).optionals (!flags.development) [
            (hsPkgs.base)
            (hsPkgs.tasty)
            (hsPkgs.tasty-hunit)
            (hsPkgs.tasty-hedgehog)
            (hsPkgs.hedgehog)
            (hsPkgs.delegation)
            (hsPkgs.containers)
            (hsPkgs.multiset)
            (hsPkgs.text)
            (hsPkgs.microlens)
            (hsPkgs.cs-ledger)
            (hsPkgs.cardano-crypto-class)
            ];
          };
        };
      };
    } // rec {
    src = (pkgs.lib).mkDefault ../.././shelley/chain-and-ledger/executable-spec;
    }