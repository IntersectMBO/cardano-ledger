#
# The default.nix file. This will generate targets for all
# buildables.  These include anything from stack.yaml
# (via nix-tools:stack-to-nix) or cabal.project (via
# nix-tools:plan-to-nix). As well as custom definitions
# on top.
#
# nix-tools stack-to-nix or plan-to-nix will generate
# the `nix/plan.nix` file. Where further customizations
# outside of the ones in stack.yaml/cabal.project can
# be specified as needed for nix/ci.
#

# We will need to import the local lib, which will
# give us the iohk-nix tooling, which also includes
# the nix-tools tooling.
let
  localLib = import ./nix/lib.nix;
in
# This file needs to export a function that takes
# the arguments it is passed and forwards them to
# the default-nix template from iohk-nix. This is
# important so that the release.nix file can properly
# parameterize this file when targetting different
# hosts.
{ withHoogle ? true
, ... }@args:
# We will instantiate the default-nix template with the
# nix/pkgs.nix file...
let
  defaultNix = localLib.nix-tools.default-nix ./nix/pkgs.nix args;
in defaultNix //
{
  shell = defaultNix.nix-tools.shellFor {
    inherit withHoogle;
    # env will provide the dependencies of cardano-shell
    packages = ps: with ps; [ cardano-ledger ];
    # This adds git to the shell, which is used by stack.
    buildInputs = [
      defaultNix.nix-tools._raw.cabal-install.components.exes.cabal
      localLib.iohkNix.cardano-repo-tool
    ];
  };
}
