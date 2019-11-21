############################################################################
# Based on iohk-skeleton project at https://github.com/input-output-hk/iohk-nix/
############################################################################

{ system ? builtins.currentSystem
, crossSystem ? null
, config ? {}
# Import IOHK common nix lib
, iohkLib ? import ./nix/iohk-common.nix { inherit system crossSystem config; }
# Use nixpkgs pin from iohkLib
, pkgs ? iohkLib.pkgs
}:

let
  haskell = pkgs.callPackage iohkLib.nix-tools.haskell {};
  src = iohkLib.cleanSourceHaskell ./.;
  util = pkgs.callPackage ./nix/util.nix {};

  # Import the Haskell package set.
  haskellPackages = import ./nix/pkgs.nix {
    inherit pkgs haskell src;
    # Provide cross-compiling secret sauce
    inherit (iohkLib.nix-tools) iohk-extras iohk-module;
  };

in {
  inherit pkgs iohkLib src haskellPackages;

  # Grab the executable components of our packages.
  inherit (haskellPackages.non-integer.components.exes) nonInt;

  tests = util.collectComponents "tests" util.isCardanoLedgerSpecs haskellPackages;
  benchmarks = util.collectComponents "benchmarks" util.isCardanoLedgerSpecs haskellPackages;

  # This provides a development environment that can be used with nix-shell or
  # lorri. See https://input-output-hk.github.io/haskell.nix/user-guide/development/
  shell = haskellPackages.shellFor {
    name = "cardano-ledger-specs-shell";
    # List all local packages in the project.
    packages = ps: with ps; [
        small-steps
        cs-ledger
        cs-blockchain
        delegation
        non-integer
    ];
    # These programs will be available inside the nix-shell.
    buildInputs =
      with pkgs.haskellPackages; [ hlint stylish-haskell weeder ]
      # Add your own packages to the shell.
      ++ [ ];
  };

  # Attributes of PDF builds of LaTeX documentation.
  byronLedgerSpec = import ./byron/ledger/formal-spec { inherit pkgs; };
  byronChainSpec = import ./byron/chain/formal-spec { inherit pkgs; };
  semanticsSpec = import ./byron/semantics/formal-spec { inherit pkgs; };
  shelleyLedgerSpec = import ./shelley/chain-and-ledger/formal-spec { inherit pkgs; };
  delegationDesignSpec = import ./shelley/design-spec { inherit pkgs; };
  nonIntegerCalculations = import ./shelley/chain-and-ledger/dependencies/non-integer/doc {inherit pkgs; };
}
