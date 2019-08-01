{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = { development = false; };
    package = {
      specVersion = "2.0";
      identifier = { name = "cs-ledger"; version = "0.1.0.0"; };
      license = "MIT";
      copyright = "";
      maintainer = "formal.methods@iohk.io";
      author = "IOHK Formal Methods Team";
      homepage = "https://github.com/input-output-hk/cardano-chain";
      url = "";
      synopsis = "Executable specification of Cardano ledger";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.base)
          (hsPkgs.bimap)
          (hsPkgs.containers)
          (hsPkgs.hashable)
          (hsPkgs.hedgehog)
          (hsPkgs.lens)
          (hsPkgs.Unique)
          (hsPkgs.small-steps)
          ];
        };
      tests = {
        "doctests" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.doctest)
            (hsPkgs.containers)
            (hsPkgs.hedgehog)
            (hsPkgs.lens)
            (hsPkgs.memory)
            (hsPkgs.text)
            (hsPkgs.small-steps)
            (hsPkgs.cs-ledger)
            ];
          build-tools = [
            (hsPkgs.buildPackages.doctest-discover or (pkgs.buildPackages.doctest-discover))
            ];
          };
        "ledger-rules-test" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.bimap)
            (hsPkgs.containers)
            (hsPkgs.lens)
            (hsPkgs.hedgehog)
            (hsPkgs.tasty)
            (hsPkgs.tasty-hunit)
            (hsPkgs.tasty-hedgehog)
            (hsPkgs.Unique)
            (hsPkgs.cs-ledger)
            (hsPkgs.small-steps)
            ];
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/cardano-ledger-specs";
      rev = "2f68ce26c684b16dc4fb6f08d3e44cf981d502bb";
      sha256 = "1ym7wj8cvd22wra2h7mk1qsgbp453iq8wrjslkwxcwifm7bncrrp";
      });
    postUnpack = "sourceRoot+=/byron/ledger/executable-spec; echo source root reset to \$sourceRoot";
    }