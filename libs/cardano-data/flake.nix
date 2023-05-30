{
  outputs = inputs: let
    main = inputs.main or (import ../../nix/flake-compat.nix).defaultNix;
  in {
    packages =
      builtins.mapAttrs (_: {cabalProject, ...}: {
        default = cabalProject.hsPkgs.cardano-data.components.library;
        testlib = cabalProject.hsPkgs.cardano-data.components.sublibs.testlib;
        tests = cabalProject.hsPkgs.cardano-data.components.tests.cardano-data-tests;
      })
      main.legacyPackages;
    inherit (main) devShells;
  };
}
