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
    # we are only interested in listing the project packages:
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

    #
    # PDF builds of LaTeX documentation.
    #
    # To download the latest PDF build from Hydra, use this link:
    #   https://hydra.iohk.io/job/Cardano/cardano-ledger-specs/specs.NAME/latest/download/1/NAME.pdf
    #
    # To get a shell where you can run pdflatex to build it yourself, use:
    #   nix-shell default.nix -A specs.NAME
    #
    # To build all specs locally with Nix:
    #  nix-build -A specs -o spec
    #
    specs = recurseIntoAttrs {
      byron-ledger = pkgs.callPackage ./byron/ledger/formal-spec {};
      byron-chain = pkgs.callPackage ./byron/chain/formal-spec {};
      small-step-semantics = pkgs.callPackage ./semantics/formal-spec {};
      shelley-ledger = pkgs.callPackage ./shelley/chain-and-ledger/formal-spec {};
      pool-ranking = pkgs.callPackage ./shelley/pool-ranking {};
      shelley-ma = pkgs.callPackage ./shelley-ma/formal-spec {};
      delegation-design = pkgs.callPackage ./shelley/design-spec {};
      non-integer-calculations = pkgs.callPackage ./shelley/chain-and-ledger/dependencies/non-integer/doc {};
      blocks-cddl = pkgs.callPackage ./byron/cddl-spec {};
    };
  };

in
  self
