{
  outputs = inputs: let
    main = inputs.main or (import ../../../../nix/flake-compat.nix).defaultNix;
  in {
    packages =
      builtins.mapAttrs (_: {cabalProject, ...}: {
        default = cabalProject.hsPkgs.byron-spec-ledger.components.library;
        test = cabalProject.hsPkgs.byron-spec-ledger.components.tests.byron-spec-ledger-test;
      })
      main.legacyPackages;
    inherit (main) devShells;
  };
}
