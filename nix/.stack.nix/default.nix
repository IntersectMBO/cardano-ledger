{
  extras = hackage:
    {
      packages = {
        "sequence" = (((hackage.sequence)."0.9.8").revisions).default;
        "tasty-hedgehog" = (((hackage.tasty-hedgehog)."0.2.0.0").revisions).default;
        "haskell-src-exts" = (((hackage.haskell-src-exts)."1.21.0").revisions).default;
        "base58-bytestring" = (((hackage.base58-bytestring)."0.1.0").revisions).default;
        "half" = (((hackage.half)."0.2.2.3").revisions).default;
        "micro-recursion-schemes" = (((hackage.micro-recursion-schemes)."5.0.2.2").revisions).default;
        "streaming-binary" = (((hackage.streaming-binary)."0.3.0.1").revisions).default;
        "pretty-show" = (((hackage.pretty-show)."1.8.2").revisions).default;
        } // {
        cardano-ledger = ./cardano-ledger.nix;
        cardano-ledger-test = ./cardano-ledger-test.nix;
        cardano-crypto-wrapper = ./cardano-crypto-wrapper.nix;
        cardano-crypto-test = ./cardano-crypto-test.nix;
        cardano-prelude = ./cardano-prelude.nix;
        cardano-prelude-test = ./cardano-prelude-test.nix;
        cardano-binary = ./cardano-binary.nix;
        cardano-binary-test = ./cardano-binary-test.nix;
        small-steps = ./small-steps.nix;
        cs-ledger = ./cs-ledger.nix;
        cs-blockchain = ./cs-blockchain.nix;
        cardano-mainnet-mirror = ./cardano-mainnet-mirror.nix;
        cborg = ./cborg.nix;
        cardano-crypto = ./cardano-crypto.nix;
        hedgehog = ./hedgehog.nix;
        canonical-json = ./canonical-json.nix;
        };
      compiler.version = "8.6.3";
      compiler.nix-name = "ghc863";
      };
  resolver = "lts-13.4";
  compiler = "ghc-8.6.3";
  }