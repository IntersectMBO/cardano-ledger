# Docs for this file: https://github.com/input-output-hk/iogx/blob/main/doc/api.md#flakenix
{
  description = "cardano-ledger";

  inputs = {
    iogx = {
      url = "github:input-output-hk/iogx";
      inputs.hackage.follows = "hackage";
      inputs.CHaP.follows = "CHaP";
      inputs.haskell-nix.follows = "haskell-nix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.iohk-nix.follows = "iohk-nix";
    };

    nixpkgs.follows = "haskell-nix/nixpkgs-unstable";

    hackage = {
      url = "github:input-output-hk/hackage.nix";
      flake = false;
    };

    CHaP = {
      url = "github:intersectmbo/cardano-haskell-packages?ref=repo";
      flake = false;
    };

    haskell-nix = {
      url = "github:input-output-hk/haskell.nix";
      inputs.hackage.follows = "hackage";
    };

    cardano-mainnet-mirror = {
      url = "github:input-output-hk/cardano-mainnet-mirror";
      flake = false;
    };

    formal-ledger-specifications = {
      url = "github:IntersectMBO/formal-ledger-specifications";
      flake = false;
    };

    iohk-nix.url = "github:input-output-hk/iohk-nix";
  };

  # Docs for mkFlake: https://github.com/input-output-hk/iogx/blob/main/doc/api.md#mkflake
  outputs = inputs:
    inputs.iogx.lib.mkFlake {

      inherit inputs;

      repoRoot = ./.;

      outputs = import ./nix/outputs.nix;

      systems = [
        "x86_64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
        # "aarch64-linux"
      ];

      # TODO: remove before merge
      debug = true;

      nixpkgsArgs = {
        config = {};
        overlays = [
          (import ./nix/pkgs.nix)
        ];
      };

      # flake = { repoRoot, inputs }: {};
    };

  nixConfig = {
    extra-substituters = [ "https://cache.iog.io" ];
    extra-trusted-public-keys =
      [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
    allow-import-from-derivation = true;
  };
}
