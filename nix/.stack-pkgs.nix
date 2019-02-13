{
  overlay = hackage:
    {
      packages = {
        "sequence" = (((hackage.sequence)."0.9.8").revisions).default;
        "tasty-hedgehog" = (((hackage.tasty-hedgehog)."0.2.0.0").revisions).default;
        "ekg" = (((hackage.ekg)."0.4.0.15").revisions).default;
        "ekg-json" = (((hackage.ekg-json)."0.1.0.6").revisions).default;
        "time-units" = (((hackage.time-units)."1.0.0").revisions).default;
        "base58-bytestring" = (((hackage.base58-bytestring)."0.1.0").revisions).default;
        "half" = (((hackage.half)."0.2.2.3").revisions).default;
        "micro-recursion-schemes" = (((hackage.micro-recursion-schemes)."5.0.2.2").revisions).default;
        "streaming-binary" = (((hackage.streaming-binary)."0.3.0.1").revisions).default;
        "pretty-show" = (((hackage.pretty-show)."1.8.2").revisions).default;
        } // {
        cardano-ledger = ./.stack.nix/cardano-ledger.nix;
        cardano-ledger-test = ./.stack.nix/cardano-ledger-test.nix;
        cardano-crypto-wrapper = ./.stack.nix/cardano-crypto-wrapper.nix;
        cardano-crypto-test = ./.stack.nix/cardano-crypto-test.nix;
        cardano-prelude = ./.stack.nix/cardano-prelude.nix;
        cardano-prelude-test = ./.stack.nix/cardano-prelude-test.nix;
        cardano-binary = ./.stack.nix/cardano-binary.nix;
        cardano-binary-test = ./.stack.nix/cardano-binary-test.nix;
        small-steps = ./.stack.nix/small-steps.nix;
        cs-ledger = ./.stack.nix/cs-ledger.nix;
        cs-blockchain = ./.stack.nix/cs-blockchain.nix;
        cardano-mainnet-mirror = ./.stack.nix/cardano-mainnet-mirror.nix;
        cardano-shell = ./.stack.nix/cardano-shell.nix;
        cborg = ./.stack.nix/cborg.nix;
        cardano-crypto = ./.stack.nix/cardano-crypto.nix;
        hedgehog = ./.stack.nix/hedgehog.nix;
        canonical-json = ./.stack.nix/canonical-json.nix;
        cardano-sl-x509 = ./.stack.nix/cardano-sl-x509.nix;
        };
      compiler.version = "8.6.3";
      compiler.nix-name = "ghc863";
      };
  resolver = "lts-13.4";
  compiler = "ghc-8.6.3";
  }
