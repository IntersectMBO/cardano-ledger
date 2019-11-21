############################################################################
# Builds Haskell packages with Haskell.nix
############################################################################

{ pkgs

# haskell.nix
, haskell

# Filtered sources of this project
, src

# Test dependencies of cardano-ledger-specs.
# Change these to what's required for your project.
# , jormungandr
# , cowsay

# Customisations for cross-compiling
, iohk-extras ? {}
, iohk-module ? {}

}:

let
  # our packages
  stack-pkgs = import ./.stack.nix/default.nix;

  # Grab the compiler name from stack-to-nix output.
  compiler = (stack-pkgs.extras {}).compiler.nix-name;

  # Chop out a subdirectory of the source, so that the package is only
  # rebuilt when something in the subdirectory changes.
  filterSubDir = dir:  with pkgs.lib; let
      isFiltered = src ? _isLibCleanSourceWith;
      origSrc = if isFiltered then src.origSrc else src;
    in cleanSourceWith {
      inherit src;
      filter = path: type:
        type == "directory" ||
        hasPrefix (toString origSrc + toString dir) path;
    } + dir;

  # This creates the Haskell package set.
  # https://input-output-hk.github.io/haskell.nix/user-guide/projects/
  pkgSet = haskell.mkStackPkgSet {
    inherit stack-pkgs;
    modules = [
      # Add source filtering to local packages
      {
        packages.small-steps.src = filterSubDir /byron/semantics/executable-spec;
        packages.cs-ledger.src = filterSubDir /byron/ledger/executable-spec;
        packages.cs-blockchain.src = filterSubDir /byron/chain/executable-spec;
        packages.delegation.src = filterSubDir /shelley/chain-and-ledger/executable-spec;
        packages.non-integer.src = filterSubDir /shelley/chain-and-ledger/dependencies/non-integer;
      }

      # The iohk-module will supply us with the necessary
      # cross compilation plumbing to make Template Haskell
      # work when cross compiling.
      iohk-module
    ];

    pkg-def-extras = [
      # iohk-extras contains package overrides and patches per ghc version.
      iohk-extras.${compiler}
    ];
  };

in
  pkgSet.config.hsPkgs // { _config = pkgSet.config; }
