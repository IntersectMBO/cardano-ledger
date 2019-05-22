let
  localLib = import ./nix/lib.nix;
  # Path of nix-tools jobs that we want to evict from release.nix:
  disabled = [
    # FIXME: those tests freeze on darwin hydra agents:
    ["nix-tools" "tests" "cardano-ledger" "cardano-ledger-test" "x86_64-darwin"]
  ];
in
{ cardano-ledger ? { outPath = ./.; rev = "abcdef"; } ,... }@args:
localLib.pkgs.lib.mapAttrsRecursiveCond
(as: !(as ? "type" && as.type == "derivation"))
(path: v: if (builtins.elem path disabled) then null else v)
(localLib.nix-tools.release-nix {
  _this = cardano-ledger;
  package-set-path = ./.;

  # packages from our stack.yaml or plan file (via nix/pkgs.nix) we
  # are intereted in building on CI via nix-tools.
  packages = [ "cardano-ledger" ];

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
  #   libs.cardano-ledger.x86_64-darwin -- will build the cardano-ledger library on and for macOS
  #   libs.cardano-ledger.x86_64-linux -- will build the cardano-ledger library on and for linux
  #   libs.x86_64-pc-mingw32-cardano-ledger.x86_64-linux -- will build the cardano-ledger library on linux for windows.
  #   tests.cs-ledger.ledger-delegation-test.x86_64-linux -- will build and run the ledger-delegation-test from the
  #                                                          cs-ledger package on linux.
  #
  required-name = "cardano-ledger-required-checks";
  required-targets = jobs: [

    jobs.nix-tools.libs.cardano-ledger.x86_64-darwin
    jobs.nix-tools.libs.cardano-ledger.x86_64-linux
    jobs.nix-tools.tests.cardano-ledger.cardano-ledger-test.x86_64-linux
    jobs.nix-tools.tests.cardano-ledger.epoch-validation-normal-form-test.x86_64-linux

    # windows cross compilation targets
    jobs.nix-tools.libs.x86_64-pc-mingw32-cardano-ledger.x86_64-linux
    jobs.nix-tools.tests.x86_64-pc-mingw32-cardano-ledger.cardano-ledger-test.x86_64-linux
  ];

} (builtins.removeAttrs args ["cardano-ledger"]))
