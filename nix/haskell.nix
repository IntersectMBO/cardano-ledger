############################################################################
# Builds Haskell packages with Haskell.nix
############################################################################
{ lib
, stdenv
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
        # Allow reinstallation of Win32
        { nonReinstallablePkgs =
          [ "rts" "ghc-heap" "ghc-prim" "integer-gmp" "integer-simple" "base"
            "deepseq" "array" "ghc-boot-th" "pretty" "template-haskell"
            # ghcjs custom packages
            "ghcjs-prim" "ghcjs-th"
            "array" "binary" "bytestring" "containers"
            "filepath" "ghc-compact" "ghc-prim"
            # "ghci" "haskeline"
            "mtl" "parsec" "text" "transformers"
            "xhtml"
            # "stm" "terminfo"
          ];
        }
        {
          packages.cs-blockchain.configureFlags = [ "--ghc-option=-Werror" ];
          packages.cs-ledger.configureFlags = [ "--ghc-option=-Werror" ];
          packages.delegation.configureFlags = [ "--ghc-option=-Werror" ];
          packages.delegation.components.tests.delegation-test.build-tools = [buildPackages.cddl buildPackages.cbor-diag];
          packages.non-integer.configureFlags = [ "--ghc-option=-Werror" ];
          packages.small-steps.configureFlags = [ "--ghc-option=-Werror" ];
          enableLibraryProfiling = profiling;
          # necessary for doctests (https://github.com/input-output-hk/haskell.nix/issues/221)
          reinstallableLibGhc = true;
          # Make sure we use a buildPackages version of alex
          packages.small-steps.components.tests.doctests.build-tools = [ buildPackages.haskell-nix.haskellPackages.alex ];
          packages.cs-ledger.components.tests.doctests.build-tools = [ buildPackages.haskell-nix.haskellPackages.alex ];
        }
    ];
  };
in
  pkgSet
