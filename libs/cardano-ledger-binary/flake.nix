{
  outputs = inputs: let
    main = inputs.main or (import ../../nix/flake-compat.nix).defaultNix;
  in {
    packages =
      builtins.mapAttrs (_: {cabalProject, ...}: {
        default = cabalProject.hsPkgs.cardano-ledger-binary.components.library;
        testlib = cabalProject.hsPkgs.cardano-ledger-binary.components.sublibs.testlib;
        tests = cabalProject.hsPkgs.cardano-ledger-binary.components.tests.tests;
      })
      main.legacyPackages;
    inherit (main) devShells;
  };
}
