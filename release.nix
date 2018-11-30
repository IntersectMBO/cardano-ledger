{ chain ? { outPath = ./.; rev = "abcdef"; } }:
let
  pkgs = import ./pkgs.nix;
in pkgs.lib.fix (self: {
  forceNewEval = pkgs.writeText "forceNewEval" chain.rev;

  required = pkgs.lib.hydraJob (pkgs.releaseTools.aggregate {
    name = "cardano-chain-required-checks";
    constituents = with self; [ byronLedgerSpec byronChainSpec semanticsSpec forceNewEval ];
  });

  byronLedgerSpec = import ./specs/ledger/latex {};
  byronChainSpec = import ./specs/chain/latex {};
  semanticsSpec = import ./specs/semantics/latex {};
})
