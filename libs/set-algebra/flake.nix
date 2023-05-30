{
  outputs = inputs: let
    main = inputs.main or (import ../../nix/flake-compat.nix).defaultNix;
  in {
    packages =
      builtins.mapAttrs (_: {cabalProject, ...}: {
        default = cabalProject.hsPkgs.set-algebra.components.library;
        tests = cabalProject.hsPkgs.set-algebra.components.tests.tests;
      })
      main.legacyPackages;
    inherit (main) devShells;
  };
}
