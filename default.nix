{ system ? builtins.currentSystem
, crossSystem ? null
# allows to cutomize haskellNix (ghc and profiling, see ./nix/haskell.nix)
, config ? {}
# allows to override dependencies of the project without modifications,
# eg. to test build against local checkout of iohk-nix:
# nix build -f default.nix cardano-node --arg sourcesOverride '{
#   iohk-nix = ../iohk-nix;
# }'
, sourcesOverride ? {}
# pinned version of nixpkgs augmented with overlays (iohk-nix and our packages).
, pkgs ? import ./nix { inherit system crossSystem config sourcesOverride; }
, gitrev ? pkgs.iohkNix.commitIdFromGitRepoOrZero ./.git
}:
with pkgs; with commonLib;
let

  haskellPackages = recRecurseIntoAttrs
    # we are only intersted in listing the project packages:
    (selectProjectPackages cardanoLedgerSpecsHaskellPackages);

  self = {
    inherit haskellPackages check-hydra;

    # `tests` are the test suites which have been built.
    tests = collectComponents' "tests" haskellPackages;
    # `benchmarks` (only built, not run).
    benchmarks = collectComponents' "benchmarks" haskellPackages;

    libs = collectComponents' "library" haskellPackages;

    exes = collectComponents' "exes" haskellPackages;

    checks = recurseIntoAttrs {
      # `checks.tests` collect results of executing the tests:
      tests = collectChecks haskellPackages;
    };

    shell = import ./shell.nix {
      inherit pkgs;
      withHoogle = true;
    };


    # Attributes of PDF builds of LaTeX documentation.
    byronLedgerSpec = import ./byron/ledger/formal-spec { inherit pkgs; };
    byronChainSpec = import ./byron/chain/formal-spec { inherit pkgs; };
    semanticsSpec = import ./byron/semantics/formal-spec { inherit pkgs; };
    shelleyLedgerSpec = import ./shelley/chain-and-ledger/formal-spec { inherit pkgs; };
    delegationDesignSpec = import ./shelley/design-spec { inherit pkgs; };
    nonIntegerCalculations = import ./shelley/chain-and-ledger/dependencies/non-integer/doc {inherit pkgs; };
    blocksCDDLSpec = import ./byron/cddl-spec {inherit pkgs; };
};
in self
