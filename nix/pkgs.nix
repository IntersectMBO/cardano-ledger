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

  # This creates the Haskell package set.
  # https://input-output-hk.github.io/haskell.nix/user-guide/projects/
  pkgSet = haskell.mkStackPkgSet {
    inherit stack-pkgs;
    modules = [
      # Add source filtering to local packages
      {
        packages.cardano-ledger-specs.small-steps = src + byron/semantics/executable-spec;
        packages.cardano-ledger-specs.cs-ledger = src + byron/ledger/executable-spec;
        packages.cardano-ledger-specs.cs-blockchain = src + byron/chain/executable-spec;
        packages.cardano-ledger-specs.delegation = src + shelley/chain-and-ledger/executable-spec;
        packages.cardano-ledger-specs.non-integer = src + shelley/chain-and-ledger/dependencies/non-integer;
        # packages.another-package = src + /another-package;
      }

      # Add dependencies
      {
        packages.cardano-ledger-specs.components.tests = {
          # unit.build-tools = [ jormungandr ];
        };

        # How to set environment variables for builds
        # packages.cardano-ledger-specs.preBuild = "export NETWORK=testnet";

        # fixme: Workaround for https://github.com/input-output-hk/haskell.nix/issues/207
        packages.cardano-ledger-specs.components.all.postInstall = pkgs.lib.mkForce "";
      }

      # Misc. build fixes for dependencies
      {
        # Cut down iohk-monitoring deps
        # Note that this reflects flags set in stack.yaml.
        # There is an open ticket to automate this in stack-to-nix.
        # https://github.com/input-output-hk/haskell.nix/issues/141
        packages.iohk-monitoring.flags = {
          disable-ekg = true;
          disable-examples = true;
          disable-graylog = true;
          disable-gui = true;
          disable-prometheus = true;
          disable-systemd = true;
        };

        # Katip has Win32 (>=2.3 && <2.6) constraint
        packages.katip.doExactConfig = true;
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
