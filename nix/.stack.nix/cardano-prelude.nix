{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = { development = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "cardano-prelude"; version = "0.1.0.0"; };
      license = "MIT";
      copyright = "2018 IOHK";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "";
      url = "";
      synopsis = "A Prelude replacement for the Cardano project";
      description = "A Prelude replacement for the Cardano project";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.base)
          (hsPkgs.aeson)
          (hsPkgs.array)
          (hsPkgs.base16-bytestring)
          (hsPkgs.bytestring)
          (hsPkgs.canonical-json)
          (hsPkgs.cborg)
          (hsPkgs.containers)
          (hsPkgs.formatting)
          (hsPkgs.ghc-heap)
          (hsPkgs.ghc-prim)
          (hsPkgs.hashable)
          (hsPkgs.integer-gmp)
          (hsPkgs.mtl)
          (hsPkgs.nonempty-containers)
          (hsPkgs.protolude)
          (hsPkgs.tagged)
          (hsPkgs.text)
          (hsPkgs.time)
          (hsPkgs.vector)
          ];
        };
      tests = {
        "cardano-prelude-test" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.aeson)
            (hsPkgs.aeson-pretty)
            (hsPkgs.attoparsec)
            (hsPkgs.base16-bytestring)
            (hsPkgs.bytestring)
            (hsPkgs.canonical-json)
            (hsPkgs.cardano-prelude)
            (hsPkgs.containers)
            (hsPkgs.cryptonite)
            (hsPkgs.formatting)
            (hsPkgs.ghc-heap)
            (hsPkgs.ghc-prim)
            (hsPkgs.hedgehog)
            (hsPkgs.hspec)
            (hsPkgs.pretty-show)
            (hsPkgs.QuickCheck)
            (hsPkgs.quickcheck-instances)
            (hsPkgs.random)
            (hsPkgs.text)
            (hsPkgs.template-haskell)
            (hsPkgs.time)
            ];
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/cardano-prelude";
      rev = "e5d847e8ba5a3ce87c5f8010e50c783f700343ec";
      sha256 = "14cz13vx1y7cyy1zhcb6dgam71rlnfj4x3hg8cs9c9rfkj2q968a";
      });
    }