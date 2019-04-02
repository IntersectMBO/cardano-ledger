{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = {};
    package = {
      specVersion = "1.8";
      identifier = { name = "non-integer"; version = "0.1.0.0"; };
      license = "NONE";
      copyright = "";
      maintainer = "formal.methods@iohk.io";
      author = "IOHK Formal Methods Team";
      homepage = "";
      url = "";
      synopsis = "";
      description = "Implementation decision for non-integer calculations";
      buildType = "Simple";
      };
    components = {
      "library" = { depends = [ (hsPkgs.base) ]; };
      exes = {
        "nonInt" = { depends = [ (hsPkgs.base) (hsPkgs.non-integer) ]; };
        };
      tests = {
        "non-integer-test" = {
          depends = [ (hsPkgs.base) (hsPkgs.non-integer) (hsPkgs.QuickCheck) ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault .././shelley/chain-and-ledger/dependencies/non-integer; }
