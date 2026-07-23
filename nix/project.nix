{ inputs, system, pkgs, lib, }:

let
  inherit (import ./utils.nix { inherit pkgs lib; }) defaultCompiler;
  # We use cabalProject' to ensure we don't build the plan for
  # all systems.
in pkgs.haskell-nix.cabalProject' ({ config, ... }: {
  src = ../.;
  name = "cardano-ledger";
  compiler-nix-name = lib.mkDefault defaultCompiler;

  # CHaP input map, so we can find CHaP packages (needs to be more
  # recent than the index-state we set!). Can be updated with
  #
  #  nix flake lock --update-input CHaP
  #
  inputMap = {
    "https://chap.intersectmbo.org/" = inputs.CHaP;
    "https://github.com/IntersectMBO/formal-ledger-specifications.git" =
      inputs.formal-ledger-specifications;
  };
  cabalProjectLocal = ''
    repository cardano-haskell-packages-local
      url: file:${inputs.CHaP}
      secure: True
    active-repositories: hackage.haskell.org, cardano-haskell-packages-local
  '';

  shell = import ./shell.nix {
    inherit inputs system lib config;
    nixpkgs = pkgs;
  };

  # package customizations as needed. Where cabal.project is not
  # specific enough, or doesn't allow setting these.
  modules = [
    ({ pkgs, ... }: {
      # packages.plutus-core.components.library.ghcOptions = [ "-fexternal-interpreter" ];
      # uncomment if necessary when profiling
      packages.byron-spec-chain.configureFlags = [ "--ghc-option=-Werror" ];
      packages.byron-spec-ledger.configureFlags = [ "--ghc-option=-Werror" ];
      packages.non-integral.configureFlags = [ "--ghc-option=-Werror" ];
      packages.cardano-ledger-shelley.configureFlags =
        [ "--ghc-option=-Werror" ];
      packages.cardano-ledger-shelley-ma-test.configureFlags =
        [ "--ghc-option=-Werror" ];
      packages.small-steps.configureFlags = [ "--ghc-option=-Werror" ];
      packages.cardano-ledger-byron = {
        configureFlags = [ "--ghc-option=-Werror" ];
        components = {
          tests.tests = {
            preCheck = ''
              export CARDANO_MAINNET_MIRROR="${inputs.cardano-mainnet-mirror}/epochs"
            '';
            testFlags = [ "--scenario=ContinuousIntegration" ];
          };
        };
      };
    })
    ({ pkgs, ... }:
      lib.mkIf pkgs.stdenv.hostPlatform.isWindows {
        packages.plutus-preprocessor.buildable = lib.mkForce false;
        packages.cardano-ledger-test.buildable = lib.mkForce false;
        packages.cardano-ledger-shelley.buildable = lib.mkForce false;
        packages.cardano-ledger-allegra.buildable = lib.mkForce false;
        packages.cardano-ledger-mary.buildable = lib.mkForce false;
        packages.cardano-ledger-alonzo.buildable = lib.mkForce false;
        packages.cardano-ledger-babbage.buildable = lib.mkForce false;
        packages.cardano-ledger-conway.buildable = lib.mkForce false;
        packages.cardano-ledger-dijkstra.buildable = lib.mkForce false;
        packages.cardano-protocol-tpraos.buildable = lib.mkForce false;
      })
  ];
})
