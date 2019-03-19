{ pkgs ? import <nixpkgs> {}
, iohk-extras ? {}
, iohk-module ? {}
, haskell
, ...
}:
let
  # our packages
  stack-pkgs = import ./pkgs.nix;

  # packages which will require TH and thus
  # will need -fexternal-interpreter treatment
  # when cross compiling.
  th-packages = [
    "hedgehog"
    "small-steps" "cs-ledger" "cs-blockchain"
  ];

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

    # The extras allow extension or restriction of the set of
    # packages we are interested in. By using the stack-pkgs.overlay
    # we restrict our package set to the ones provided in stack.yaml.
    pkg-def-extras = [
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
      (import ./config.nix)
    ];
  };
in
  pkgSet.config.hsPkgs // { _config = pkgSet.config; }
