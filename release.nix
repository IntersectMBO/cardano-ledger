let
  localLib = import ./lib.nix;
in
localLib.nix-tools.release-nix {
  package-set-path = ./.;

  # packages from our stack.yaml or plan file (via nix/pkgs.nix) we
  # are intereted in building on CI via nix-tools.
  packages = [ "small-steps" "cs-ledger" "cs-blockchain" "delegation" ];

  # The set of jobs we consider crutial for each CI run.
  # if a single one of these fails, the build will be marked
  # as failed.
  #
  # The names can be looked up on hydra when in doubt.
  #
  # custom jobs will follow their name as set forth in
  # other-packages.
  #
  # nix-tools packages withh be prefixed with nix-tools and
  # follow the following naming convention:
  #
  #   namespace                      optional cross compilation prefix                  build machine
  #   .-------.                              .-----------------.                 .--------------------------.
  #   nix-tools.{libs,exes,tests,benchmarks}.{x86_64-pc-mingw-,}.$pkg.$component.{x86_64-linux,x86_64-darwin}
  #             '--------------------------'                     '-------------'
  #                 component type                           cabal pkg and component*
  #
  # * note that for libs, $component is empty, as cabal only
  # provides a single library for packages right now.
  #
  # Example:
  #
  #   libs.cardano-chain.x86_64-darwin -- will build the cardano-chain library on and for macOS
  #   libs.cardano-chain.x86_64-linux -- will build the cardano-chain library on and for linux
  #   libs.x86_64-pc-mingw32-cardano-chain.x86_64-linux -- will build the cardano-chain library on linux for windows.
  #   tests.cs-ledger.ledger-delegation-test.x86_64-linux -- will build and run the ledger-delegation-test from the
  #                                                          cs-ledger package on linux.
  #
  required-name = "cardano-ledger-specs-required-checks";
  required-targets = jobs: [
    jobs.byronLedgerSpec
    jobs.byronChainSpec
    jobs.semanticsSpec
    jobs.shelleyLedgerSpec
    jobs.delegationDesignSpec
    jobs.nonIntegerCalculations

    jobs.nix-tools.libs.cs-blockchain.x86_64-darwin
    jobs.nix-tools.libs.cs-blockchain.x86_64-linux
    jobs.nix-tools.libs.small-steps.x86_64-darwin
    jobs.nix-tools.libs.small-steps.x86_64-linux
    jobs.nix-tools.libs.cs-ledger.x86_64-darwin
    jobs.nix-tools.libs.cs-ledger.x86_64-linux

    jobs.nix-tools.tests.cs-ledger.ledger-delegation-test.x86_64-darwin
    jobs.nix-tools.tests.cs-ledger.ledger-delegation-test.x86_64-linux

    # Windows cross-compilation targets

    jobs.nix-tools.libs.x86_64-pc-mingw32-cs-blockchain.x86_64-linux
    jobs.nix-tools.libs.x86_64-pc-mingw32-cs-ledger.x86_64-linux
    jobs.nix-tools.libs.x86_64-pc-mingw32-small-steps.x86_64-linux

    jobs.nix-tools.tests.x86_64-pc-mingw32-cs-ledger.ledger-delegation-test.x86_64-linux
    ];

}
