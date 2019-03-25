{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = { development = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "cardano-ledger"; version = "0.1.0.0"; };
      license = "MIT";
      copyright = "2018 IOHK";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "";
      url = "";
      synopsis = "The blockchain layer of Cardano";
      description = "The blockchain layer of Cardano";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.base)
          (hsPkgs.aeson)
          (hsPkgs.base16-bytestring)
          (hsPkgs.base58-bytestring)
          (hsPkgs.base64-bytestring-type)
          (hsPkgs.binary)
          (hsPkgs.bytestring)
          (hsPkgs.canonical-json)
          (hsPkgs.cardano-binary)
          (hsPkgs.cardano-crypto-wrapper)
          (hsPkgs.cardano-prelude)
          (hsPkgs.cborg)
          (hsPkgs.containers)
          (hsPkgs.cryptonite)
          (hsPkgs.Cabal)
          (hsPkgs.directory)
          (hsPkgs.filepath)
          (hsPkgs.formatting)
          (hsPkgs.lens)
          (hsPkgs.memory)
          (hsPkgs.mtl)
          (hsPkgs.resourcet)
          (hsPkgs.streaming)
          (hsPkgs.streaming-binary)
          (hsPkgs.streaming-bytestring)
          (hsPkgs.text)
          (hsPkgs.time)
          (hsPkgs.vector)
          ];
        };
      tests = {
        "cardano-ledger-test" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.base16-bytestring)
            (hsPkgs.bytestring)
            (hsPkgs.canonical-json)
            (hsPkgs.cardano-binary)
            (hsPkgs.cardano-binary-test)
            (hsPkgs.cardano-ledger)
            (hsPkgs.cardano-crypto)
            (hsPkgs.cardano-crypto-test)
            (hsPkgs.cardano-crypto-wrapper)
            (hsPkgs.cardano-mainnet-mirror)
            (hsPkgs.cardano-prelude)
            (hsPkgs.cardano-prelude-test)
            (hsPkgs.containers)
            (hsPkgs.cryptonite)
            (hsPkgs.cs-blockchain)
            (hsPkgs.cs-ledger)
            (hsPkgs.directory)
            (hsPkgs.filepath)
            (hsPkgs.formatting)
            (hsPkgs.hedgehog)
            (hsPkgs.lens)
            (hsPkgs.optparse-applicative)
            (hsPkgs.resourcet)
            (hsPkgs.small-steps)
            (hsPkgs.streaming)
            (hsPkgs.text)
            (hsPkgs.time)
            (hsPkgs.vector)
            ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault .././../.; }