{
  description = "cardano-ledger";

  inputs = {

    hackageNix = {
      url = "github:input-output-hk/hackage.nix";
      flake = false;
    };

    haskellNix = {
      url = "github:input-output-hk/haskell.nix";
      inputs.hackage.follows = "hackageNix";
    };

    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    iohkNix.url = "github:input-output-hk/iohk-nix";
    flake-utils.url = "github:hamishmack/flake-utils/hkm/nested-hydraJobs";

    CHaP = {
      url = "github:intersectmbo/cardano-haskell-packages?ref=repo";
      flake = false;
    };

    cardano-mainnet-mirror = {
      url = "github:input-output-hk/cardano-mainnet-mirror";
      flake = false;
    };

    # non-flake nix compatibility
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };

    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";

    cardano-ledger-release-tool = {
      # Tag should match the ones used in .github/workflows/*.yml
      url = "github:input-output-hk/cardano-ledger-release-tool?ref=0.5.0.0";
      inputs.haskell-nix.follows = "haskellNix";
      inputs.hackage.follows = "hackageNix";
      inputs.pre-commit-hooks.follows = "pre-commit-hooks";
      inputs.flake-utils.follows = "flake-utils";
    };

    formal-ledger-specifications = {
      url = "github:IntersectMBO/formal-ledger-specifications";
      flake = false;
    };
  };

  outputs = inputs:
    let supportedSystems = [ "x86_64-linux" "aarch64-darwin" ];
    in inputs.flake-utils.lib.eachSystem supportedSystems
    (system: import ./nix/outputs.nix { inherit inputs system; });

  nixConfig = {
    extra-substituters = [ "https://cache.iog.io" ];
    extra-trusted-public-keys =
      [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
    allow-import-from-derivation = true;
  };
}
