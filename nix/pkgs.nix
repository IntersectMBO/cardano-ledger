{ pkgs ? import <nixpkgs> {}
, iohk-extras ? {}
, iohk-module ? {}
, haskell
, ...
}:
let
  # our packages
  stack-pkgs = import ./.stack.nix;

  # The cardano-mainnet-mirror used during testing
  cardano-mainnet-mirror = import ./cardano-mainnet-mirror.nix {inherit pkgs;};

  # Build the packageset with module support.
  # We can essentially override anything in the modules
  # section.
  #
  #  packages.cbors.patches = [ ./one.patch ];
  #  packages.cbors.flags.optimize-gmp = false;
  #
  compiler = (stack-pkgs.extras {}).compiler.nix-name;
  pkgSet = haskell.mkStackPkgSet {
    inherit stack-pkgs;
    # The overlay allows extension or restriction of the set of
    # packages we are interested in. By using the stack-pkgs.overlay
    # we restrict our package set to the ones provided in stack.yaml.
    pkg-def-extras = [
      stack-pkgs.extras
      iohk-extras.${compiler}
    ];
    # package customizations
    modules = [
      # the iohk-module will supply us with the necessary
      # cross compilation plumbing to make Template Haskell
      # work when cross compiling.  For now we need to
      # list the packages that require template haskell
      # explicity here.
      iohk-module
      {
        # katip has an version bound of Win32 < 2.6; this however
        # implies that it's incompatible with ghc-8.6 (on windows).
        # Let's force it to accept out packageset.
        packages.katip.doExactConfig = true;

        packages.cardano-ledger.preBuild =
          "export CARDANO_MAINNET_MIRROR=${cardano-mainnet-mirror}/epochs";
      }
    ];
  };
in
pkgSet.config.hsPkgs // { _config = pkgSet.config; }
