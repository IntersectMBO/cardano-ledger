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
          (hsPkgs.bimap)
          (hsPkgs.bytestring)
          (hsPkgs.containers)
          (hsPkgs.cs-ledger)
          (hsPkgs.hashable)
          (hsPkgs.hedgehog)
          (hsPkgs.lens)
          (hsPkgs.small-steps)
          ];
        };
      tests = {
        "chain-rules-test" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.containers)
            (hsPkgs.data-ordlist)
            (hsPkgs.hedgehog)
            (hsPkgs.lens)
            (hsPkgs.tasty)
            (hsPkgs.tasty-hedgehog)
            (hsPkgs.tasty-hunit)
            (hsPkgs.cs-blockchain)
            (hsPkgs.cs-ledger)
            (hsPkgs.small-steps)
            ];
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/cardano-ledger-specs";
      rev = "09ca95901d6b1690f10886e690d1adb74df40923";
      sha256 = "1aqv3ghhjg046j5b40wkg5g242bhwiymppx0xz67al1wmwxr8iab";
      });
    postUnpack = "sourceRoot+=/byron/chain/executable-spec; echo source root reset to \$sourceRoot";
    }