############################################################################
# Builds Haskell packages with Haskell.nix
############################################################################
{ lib
, stdenv
, pkgs
, haskell-nix
, buildPackages
, config ? {}
# GHC attribute name
, compiler ? config.haskellNix.compiler or "ghc865"
# Enable profiling
, profiling ? config.haskellNix.profiling or false
}:
let
  # The cardano-mainnet-mirror used during testing
  cardano-mainnet-mirror = import ./cardano-mainnet-mirror.nix {inherit pkgs;};

  # This creates the Haskell package set.
  # https://input-output-hk.github.io/haskell.nix/user-guide/projects/
  pkgSet = haskell-nix.cabalProject {
    src = haskell-nix.haskellLib.cleanGit {
      name = "cardano-ledger-specs" ;
      src = ../. ;
      };
    compiler-nix-name = compiler;
    modules = [
      {
        packages.byron-spec-chain.configureFlags = [ "--ghc-option=-Werror" ];
        packages.byron-spec-ledger.configureFlags = [ "--ghc-option=-Werror" ];
        packages.delegation.configureFlags = [ "--ghc-option=-Werror" ];
        packages.shelley-spec-non-integral.configureFlags = [ "--ghc-option=-Werror" ];
        packages.small-steps.configureFlags = [ "--ghc-option=-Werror" ];
        packages.shelley-spec-ledger.components.tests.shelley-spec-ledger-test.build-tools = [pkgs.cddl pkgs.cbor-diag];
        enableLibraryProfiling = profiling;
        # Disable doctests for now (waiting for https://github.com/input-output-hk/haskell.nix/pull/427):
        packages.small-steps.components.tests.doctests.buildable = lib.mkForce false;
        packages.small-steps-test.components.tests.doctests.buildable = lib.mkForce false;
        packages.byron-spec-ledger.components.tests.doctests.buildable = lib.mkForce false;

        packages.cardano-ledger = {
          configureFlags = [ "--ghc-option=-Werror" ];
          components = {
            all.postInstall = pkgs.lib.mkForce "";
            tests.cardano-ledger-test = {
              preCheck = ''
                export CARDANO_MAINNET_MIRROR="${cardano-mainnet-mirror}/epochs"
                cp ${../byron/ledger/impl/mainnet-genesis.json} ./mainnet-genesis.json
              '';
              build-tools = [ pkgs.makeWrapper ];
              testFlags = [ "--scenario=ContinuousIntegration" ];
            };
          };
        };

      }
    ];
  };
in
  pkgSet
