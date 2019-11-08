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

  # Example of using a package from iohk-nix
  # inherit (iohkLib.rust-packages.pkgs) jormungandr;

  # Import the Haskell package set.
  haskellPackages = import ./nix/pkgs.nix {
    inherit pkgs haskell src;
    # Pass in any extra programs necessary for the build as function arguments.
    # Examples:
    # inherit jormungandr;
    # inherit (pkgs) cowsay;
    # Provide cross-compiling secret sauce
    inherit (iohkLib.nix-tools) iohk-extras iohk-module;
  };

in {
  inherit pkgs iohkLib src haskellPackages;
  inherit (haskellPackages.cardano-ledger-specs.identifier) version;

  # Grab the executable component of our package.
  inherit (haskellPackages.cardano-ledger-specs.components.exes)
    cardano-ledger-specs;

  tests = util.collectComponents "tests" util.isCardanoLedgerSpecs haskellPackages;
  benchmarks = util.collectComponents "benchmarks" util.isCardanoLedgerSpecs haskellPackages;

  # This provides a development environment that can be used with nix-shell or
  # lorri. See https://input-output-hk.github.io/haskell.nix/user-guide/development/
  shell = haskellPackages.shellFor {
    name = "cardano-ledger-specs-shell";
    # List all local packages in the project.
    packages = ps: with ps; [
        small-steps
        cs-leddger
        cs-blockchain
        delegation
        non-integer
    ];
    # These programs will be available inside the nix-shell.
    buildInputs =
      with pkgs.haskellPackages; [ hlint stylish-haskell weeder ghcid lentil ]
      # Add your own packages to the shell.
      ++ [ ];
  };

  # Attributes of PDF builds of LaTeX documentation.
  byronLedgerSpec = import ./byron/ledger/formal-spec {};
  byronChainSpec = import ./byron/chain/formal-spec {};
  semanticsSpec = import ./byron/semantics/formal-spec {};
  shelleyLedgerSpec = import ./shelley/chain-and-ledger/formal-spec {};
  delegationDesignSpec = import ./shelley/design-spec {};
  nonIntegerCalculations = import ./shelley/chain-and-ledger/dependencies/non-integer/doc {};
}
