{ system }:
let
  type = "github";
  owner = "input-output-hk";
  repo = "cardano-ledger-release-tool";
  rev = "f52262644af02e6eae54cf5d6021ecebf7f5c1d1";
in
(builtins.getFlake "${type}:${owner}/${repo}/${rev}").packages.${system}.default
