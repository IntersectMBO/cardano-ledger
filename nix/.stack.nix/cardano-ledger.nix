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
          (hsPkgs.base58-bytestring)
          (hsPkgs.base64-bytestring-type)
          (hsPkgs.binary)
          (hsPkgs.bytestring)
          (hsPkgs.canonical-json)
          (hsPkgs.cardano-binary)
          (hsPkgs.cardano-crypto-wrapper)
          (hsPkgs.cardano-prelude)
          (hsPkgs.containers)
          (hsPkgs.concurrency)
          (hsPkgs.cryptonite)
          (hsPkgs.Cabal)
          (hsPkgs.deepseq)
          (hsPkgs.directory)
          (hsPkgs.filepath)
          (hsPkgs.formatting)
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
      exes = {
        "cardano-ledger-validation-exe" = {
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
      tests = {
        "cardano-ledger-test" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.base16-bytestring)
            (hsPkgs.bytestring)
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
            (hsPkgs.cs-blockchain)
            (hsPkgs.cs-ledger)
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
    } // rec { src = (pkgs.lib).mkDefault ../.././.; }