let
  pkgs = import ./pkgs.nix;
in pkgs.lib.fix (self: {
  required = pkgs.lib.hydraJob (pkgs.releaseTools.aggregate {
    name = "cardano-chain-required-checks";
    constituents = [ self.byronLedgerSpec ];
  });
  byronLedgerSpec = import ./specs/ledger/latex {};
  byronChainSpec = import ./specs/chain/latex {};
  semanticsSpec = import ./specs/semantics/latex {};
})
