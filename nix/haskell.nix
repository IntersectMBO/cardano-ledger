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

  # This creates the Haskell package set.
  # https://input-output-hk.github.io/haskell.nix/user-guide/projects/
  pkgSet = haskell-nix.cabalProject {
    src = haskell-nix.haskellLib.cleanGit { src = ../. ; };
    ghc = buildPackages.haskell-nix.compiler.${compiler};
    modules = [
      {
        packages.cs-blockchain.configureFlags = [ "--ghc-option=-Werror" ];
        packages.cs-ledger.configureFlags = [ "--ghc-option=-Werror" ];
        packages.delegation.configureFlags = [ "--ghc-option=-Werror" ];
        packages.shelley-spec-non-integral.configureFlags = [ "--ghc-option=-Werror" ];
        packages.small-steps.configureFlags = [ "--ghc-option=-Werror" ];
        packages.shelley-spec-ledger.components.tests.shelley-spec-ledger-test.build-tools = [pkgs.cddl pkgs.cbor-diag];
        enableLibraryProfiling = profiling;
        # Disable doctests for now (waiting for https://github.com/input-output-hk/haskell.nix/pull/427):
        packages.small-steps.components.tests.doctests.buildable = lib.mkForce false;
        packages.cs-ledger.components.tests.doctests.buildable = lib.mkForce false;
      }
    ];
  };
in
  pkgSet
