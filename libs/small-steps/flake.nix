{
  outputs = inputs: let
    main = inputs.main or (import ../../nix/flake-compat.nix).defaultNix;
  in {
    packages =
      builtins.mapAttrs (_: {cabalProject, ...}: {
        default = cabalProject.hsPkgs.small-steps.components.library;
      })
      main.legacyPackages;
    inherit (main) devShells;
  };
}
