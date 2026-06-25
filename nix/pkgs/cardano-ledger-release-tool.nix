{ system }:
let
  type = "github";
  owner = "input-output-hk";
  repo = "cardano-ledger-release-tool";
  rev = "98b7da19a6e385c62292125a5cede79f444dd9b9"; # git rev-parse 0.1.1.0
in
(builtins.getFlake "${type}:${owner}/${repo}/${rev}").packages.${system}.default
