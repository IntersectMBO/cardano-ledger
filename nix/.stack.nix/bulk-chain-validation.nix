{ mkDerivation, base, cardano-crypto-wrapper, cardano-ledger
, cardano-mainnet-mirror, cardano-prelude, cardano-shell
, containers, formatting, iohk-monitoring, stdenv
}:
mkDerivation {
  pname = "bulk-chain-validation";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base cardano-crypto-wrapper cardano-ledger cardano-mainnet-mirror
    cardano-prelude cardano-shell containers formatting iohk-monitoring
  ];
  license = stdenv.lib.licenses.mit;
}
