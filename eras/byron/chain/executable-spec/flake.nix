{
  outputs = inputs: let
    main = inputs.main or (import ../../../../nix/flake-compat.nix).defaultNix;
  in {
    packages =
      builtins.mapAttrs (_: {cabalProject, ...}: {
        default = cabalProject.hsPkgs.byron-spec-chain.components.library;
        test = cabalProject.hsPkgs.byron-spec-chain.components.tests.chain-rules-test;
      })
      main.legacyPackages;
    inherit (main) devShells;
  };
}
