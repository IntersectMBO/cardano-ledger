{
  description = "cardano-ledger";

  nixConfig = {
    extra-substituters = ["https://cache.iog.io"];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];
    allow-import-from-derivation = true;
  };

  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/nixos-22.05;
    tullia = {
      url = github:input-output-hk/tullia;
      inputs.nixpkgs.follows = "nixpkgs";
    };
    parts = {
      url = github:hercules-ci/flake-parts;
      inputs.nixpkgs-lib.follows = "nixpkgs";
    };
  };

  outputs = {
    self,
    parts,
    ...
  } @ inputs:
    parts.lib.mkFlake {
      inherit self;
      specialArgs = {inherit inputs;};
    } ({
      config,
      lib,
      ...
    }: {
      systems = ["x86_64-linux" "x86_64-darwin"];

      imports = [nix/tullia.nix];

      flake.hydraJobs = lib.genAttrs config.systems (
        system:
          import ./release.nix {
            cardano-ledger-specs = self;
            supportedSystems = [system];
          }
      );

      perSystem = {pkgs, ...}: {
        formatter = pkgs.alejandra;
      };
    });
}
