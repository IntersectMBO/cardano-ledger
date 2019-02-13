{
  overlay = hackage:
    {
      packages = {
        "sequence" = (((hackage.sequence)."0.9.8").revisions).default;
        "tasty-hedgehog-coverage" = (((hackage.tasty-hedgehog-coverage)."0.1.0.0").revisions).default;
        "aeson-options" = (((hackage.aeson-options)."0.0.0").revisions).default;
        "base58-bytestring" = (((hackage.base58-bytestring)."0.1.0").revisions).default;
        "half" = (((hackage.half)."0.2.2.3").revisions).default;
        "micro-recursion-schemes" = (((hackage.micro-recursion-schemes)."5.0.2.2").revisions).default;
        "streaming-binary" = (((hackage.streaming-binary)."0.3.0.1").revisions).default;
        } // {
        delegation = ./.stack.nix/delegation.nix;
        cs-blockchain = ./.stack.nix/cs-blockchain.nix;
        cs-ledger = ./.stack.nix/cs-ledger.nix;
        small-steps = ./.stack.nix/small-steps.nix;
        cborg = ./.stack.nix/cborg.nix;
        cardano-crypto = ./.stack.nix/cardano-crypto.nix;
        plutus-prototype = ./.stack.nix/plutus-prototype.nix;
        hedgehog = ./.stack.nix/hedgehog.nix;
        canonical-json = ./.stack.nix/canonical-json.nix;
        };
      compiler.version = "8.6.3";
      compiler.nix-name = "ghc863";
      };
  resolver = "nightly-2018-12-17";
  compiler = "ghc-8.6.3";
  }
