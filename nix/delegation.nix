{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = {};
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
          (hsPkgs.multiset)
          (hsPkgs.tasty)
          (hsPkgs.tasty-hunit)
          (hsPkgs.hedgehog)
          (hsPkgs.tasty-hedgehog)
          (hsPkgs.tasty-hedgehog-coverage)
          (hsPkgs.text)
          (hsPkgs.small-steps)
          (hsPkgs.microlens)
          (hsPkgs.microlens-th)
          (hsPkgs.non-integer)
          ];
        };
      tests = {
        "delegation-test" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.tasty)
            (hsPkgs.tasty-hunit)
            (hsPkgs.tasty-hedgehog)
            (hsPkgs.tasty-hedgehog-coverage)
            (hsPkgs.hedgehog)
            (hsPkgs.delegation)
            (hsPkgs.cryptonite)
            (hsPkgs.containers)
            (hsPkgs.multiset)
            (hsPkgs.text)
            (hsPkgs.microlens)
            ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault .././shelley/chain-and-ledger/executable-spec; }
