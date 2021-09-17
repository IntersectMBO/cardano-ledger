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

    roots = cardanoLedgerSpecsHaskellPackages.roots;

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
      byron-ledger = pkgs.callPackage ./eras/byron/ledger/formal-spec/default.nix {};
      byron-chain = pkgs.callPackage ./eras/byron/chain/formal-spec/default.nix {};
      shelley-ledger = pkgs.callPackage ./eras/shelley/formal-spec/default.nix {};
      shelley-ma = pkgs.callPackage ./eras/shelley-ma/formal-spec/default.nix {};
      alonzo-ledger = pkgs.callPackage ./eras/alonzo/formal-spec/default.nix {};
      delegation-design = pkgs.callPackage ./eras/shelley/design-spec/default.nix {};
      small-step-semantics = pkgs.callPackage ./docs/small-step-semantics/default.nix {};
      pool-ranking = pkgs.callPackage ./docs/pool-ranking/default.nix {};
      non-integer-calculations = pkgs.callPackage ./docs/non-integer-calculations/default.nix {};
      blocks-cddl = pkgs.callPackage ./eras/byron/cddl-spec/default.nix {};
    };

    doc = {
      site =
        let
          sphinx-markdown-tables = pkgs.python3Packages.callPackage ./nix/python/sphinx-markdown-tables.nix {};
          sphinxemoji = pkgs.python3Packages.callPackage ./nix/python/sphinxemoji.nix {};
        in pkgs.callPackage ./doc { inherit sphinx-markdown-tables sphinxemoji; pythonPackages = pkgs.python3Packages; };
    };
  };

in
  self
