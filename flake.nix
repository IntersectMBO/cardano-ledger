{
  description = "cardano-ledger";

  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    iohkNix.url = "github:input-output-hk/iohk-nix";
    flake-utils.url = "github:hamishmack/flake-utils/hkm/nested-hydraJobs";

    CHaP.url = "github:input-output-hk/cardano-haskell-packages?ref=repo";
    CHaP.flake = false;

    cardano-mainnet-mirror.url = "github:input-output-hk/cardano-mainnet-mirror";
    cardano-mainnet-mirror.flake = false;

    # cicero
    tullia.url = "github:input-output-hk/tullia";

    # non-flake nix compatibility
    flake-compat.url = "github:edolstra/flake-compat";
    flake-compat.flake = false;
  };

  outputs = inputs: let
    supportedSystems = [
      "x86_64-linux"
      "x86_64-darwin"
      "aarch64-linux"
      "aarch64-darwin"
    ];
  in
    inputs.flake-utils.lib.eachSystem supportedSystems (
      system: let
        # setup our nixpkgs with the haskell.nix overlays, and the iohk-nix
        # overlays...
        nixpkgs = import inputs.nixpkgs {
          overlays = [
            # our local packages.
            (import ./nix/pkgs.nix)
            # iohkNix.overlays.crypto provide libsodium-vrf, libblst and libsecp256k1.
            inputs.iohkNix.overlays.crypto
            # haskellNix.overlay can be configured by later overlays, so need to come before them.
            inputs.haskellNix.overlay
            # configure haskell.nix to use iohk-nix crypto librairies.
            inputs.iohkNix.overlays.haskell-nix-crypto
          ];
          inherit system;
          inherit (inputs.haskellNix) config;
        };
        inherit (nixpkgs) lib;

        # We use cabalProject' to ensure we don't build the plan for
        # all systems.
        cabalProject = nixpkgs.haskell-nix.cabalProject' {
          src = ./.;
          name = "cardano-ledger";
          compiler-nix-name = lib.mkDefault "ghc928";

          # CHaP input map, so we can find CHaP packages (needs to be more
          # recent than the index-state we set!). Can be updated with
          #
          #  nix flake lock --update-input CHaP
          #
          inputMap = {
            "https://input-output-hk.github.io/cardano-haskell-packages" = inputs.CHaP;
          };

          # tools we want in our shell, from hackage
          shell.tools = {
            cabal = "3.10.1.0";
            ghcid = "0.8.8";
            haskell-language-server = "latest";
            hlint = {};
            fourmolu = "0.12.0.0";
          };
          # and from nixpkgs or other inputs
          shell.nativeBuildInputs = with nixpkgs;
            [
              (python3.withPackages (ps: with ps; [sphinx sphinx_rtd_theme recommonmark sphinx-markdown-tables sphinxemoji]))
              haskellPackages.implicit-hie
            ]
            ++ (with inputs.tullia.packages.${system}; [
              tullia
              nix-systems
            ]);
          # disable Hoogle until someone request it
          shell.withHoogle = false;

          # package customizations as needed. Where cabal.project is not
          # specific enough, or doesn't allow setting these.
          modules = [
            ({pkgs, ...}: {
              packages.byron-spec-chain.configureFlags = ["--ghc-option=-Werror"];
              packages.byron-spec-ledger.configureFlags = ["--ghc-option=-Werror"];
              packages.delegation.configureFlags = ["--ghc-option=-Werror"];
              packages.non-integral.configureFlags = ["--ghc-option=-Werror"];
              packages.cardano-ledger-shelley.configureFlags = ["--ghc-option=-Werror"];
              packages.cardano-ledger-shelley-ma.configureFlags = ["--ghc-option=-Werror"];
              packages.cardano-ledger-shelley-ma-test.configureFlags = ["--ghc-option=-Werror"];
              packages.small-steps.configureFlags = ["--ghc-option=-Werror"];
              packages.cardano-ledger-byron = {
                configureFlags = ["--ghc-option=-Werror"];
                components = {
                  tests.cardano-ledger-byron-test = {
                    preCheck = ''
                      export CARDANO_MAINNET_MIRROR="${inputs.cardano-mainnet-mirror}/epochs"
                      cp ${./eras/byron/ledger/impl/mainnet-genesis.json} ./mainnet-genesis.json
                    '';
                    testFlags = ["--scenario=ContinuousIntegration"];
                  };
                };
              };
            })
            ({pkgs, ...}:
              lib.mkIf pkgs.stdenv.hostPlatform.isUnix {
                packages.cardano-ledger-shelley-ma-test.components.tests.cardano-ledger-shelley-ma-test.build-tools = [pkgs.cddl pkgs.cbor-diag];
                packages.cardano-ledger-shelley-test.components.tests.cardano-ledger-shelley-test.build-tools = [pkgs.cddl pkgs.cbor-diag];
                packages.cardano-ledger-alonzo-test.components.tests.cardano-ledger-alonzo-test.build-tools = [pkgs.cddl pkgs.cbor-diag];
                packages.cardano-ledger-babbage-test.components.tests.cardano-ledger-babbage-test.build-tools = [pkgs.cddl pkgs.cbor-diag];
                packages.cardano-ledger-conway-test.components.tests.cardano-ledger-conway-test.build-tools = [pkgs.cddl pkgs.cbor-diag];
              })
            ({pkgs, ...}:
              lib.mkIf pkgs.stdenv.hostPlatform.isWindows {
                packages.set-algebra.components.tests.tests.buildable = lib.mkForce false;
                packages.plutus-preprocessor.package.buildable = lib.mkForce false;
                packages.cardano-ledger-test.package.buildable = lib.mkForce false;
                packages.cardano-ledger-shelley-ma-test.package.buildable = lib.mkForce false;
                packages.cardano-ledger-shelley-test.package.buildable = lib.mkForce false;
                packages.cardano-ledger-alonzo-test.package.buildable = lib.mkForce false;
                packages.cardano-ledger-babbage-test.package.buildable = lib.mkForce false;
                packages.cardano-ledger-conway-test.package.buildable = lib.mkForce false;
              })
          ];
        };
        # ... and construct a flake from the cabal project
        flake =
          cabalProject.flake (
            # we also want cross compilation to windows.
            lib.optionalAttrs (system == "x86_64-linux") {
              crossPlatforms = p: [p.mingwW64];
            }
          )
          # add cicero logic.
          // (let
            actionCiInputName = "GitHub event";
          in
            inputs.tullia.fromSimple system {
              tasks = {
                ci = {
                  config,
                  lib,
                  ...
                }: {
                  preset = {
                    nix.enable = true;
                    github.ci = {
                      # Tullia tasks can run locally or on Cicero.
                      # When no facts are present we know that we are running locally and vice versa.
                      # When running locally, the current directory is already bind-mounted into the container,
                      # so we don't need to fetch the source from GitHub and we don't want to report a GitHub status.
                      enable = config.actionRun.facts != {};
                      repository = "input-output-hk/cardano-ledger";
                      remote = config.preset.github.lib.readRepository actionCiInputName null;
                      revision = config.preset.github.lib.readRevision actionCiInputName null;
                    };
                  };

                command.text = ''
                  # filter out systems that we cannot build for:
                  systems=$(nix eval .#packages --apply builtins.attrNames --json |
                    nix-systems -i |
                    jq -r 'with_entries(select(.value)) | keys | .[]')
                  targets=$(for s in $systems; do echo .#hydraJobs."$s".required; done)
                  # shellcheck disable=SC2086
                  nix build --keep-going $targets || nix build --keep-going -L $targets
                '';

                  memory = 1024 * 8;
                  nomad.driver = "exec";
                  nomad.resources.cpu = 10000;
                };
              };

              actions = {
                "cardano-ledger/ci" = {
                  task = "ci";
                  io = ''
                    // This is a CUE expression that defines what events trigger a new run of this action.
                    // There is no documentation for this yet. Ask SRE if you have trouble changing this.
                    let github = {
                      #input: "${actionCiInputName}"
                      #repo: "input-output-hk/cardano-ledger"
                    }

                    #lib.merge
                    #ios: [
                      {#lib.io.github_push, github, #default_branch: true},
                      {#lib.io.github_pr,   github},
                    ]
                  '';
                };
              };
            });
      in
        lib.recursiveUpdate flake rec {
          # add a required job, that's basically all hydraJobs.
          hydraJobs =
            nixpkgs.callPackages inputs.iohkNix.utils.ciJobsAggregates
            {
              ciJobs =
                flake.hydraJobs
                // {
                  inherit (legacyPackages) doc specs;
                  # This ensure hydra send a status for the required job (even if no change other than commit hash)
                  revision = nixpkgs.writeText "revision" inputs.self.rev;
                };
            };
          legacyPackages = rec {
            inherit cabalProject nixpkgs;
            # also provide hydraJobs through legacyPackages to allow building without system prefix:
            inherit hydraJobs;
            doc.site = ((import ./doc/flake.nix).outputs {main = inputs.self;}).packages.${system}.default;
            #
            # PDF builds of LaTeX documentation.
            #
            # To download the latest PDF build from Hydra, use this link:
            #   https://github.com/input-output-hk/cardano-ledger/releases/latest/download/NAME.pdf
            #
            # To get a shell where you can run pdflatex to build it yourself, use:
            #   cd <spec directory>
            #   nix develop
            #
            # To build all specs locally with Nix:
            #  nix build .#specs -o spec
            #
            specs = nixpkgs.symlinkJoin {
              name = "cardano-ledger-specs";
              # XXX: make use of flake relative path inputs once this is fixed: https://github.com/NixOS/nix/issues/6352
              # (so that we can ditch flake-compat.nix)
              paths = map (d: ((import "${./.}/${d}/flake.nix").outputs {main = inputs.self;}).packages.${system}.default) [
                "eras/byron/ledger/formal-spec"
                "eras/byron/chain/formal-spec"
                "eras/shelley/formal-spec"
                "eras/shelley-ma/formal-spec"
                "eras/alonzo/formal-spec"
                "eras/babbage/formal-spec"
                "eras/shelley/design-spec"
                "docs/small-step-semantics"
                "docs/pool-ranking"
                "docs/non-integer-calculations"
                "eras/byron/cddl-spec"
              ];
            };
          };
          devShells = let
            profillingShell = p: {
              # `nix develop .#profiling` (or `.#ghc927.profiling): a shell with profiling enabled
              profiling = (p.appendModule {modules = [{enableLibraryProfiling = true;}];}).shell;
            };
          in
            profillingShell cabalProject
            # Additional shells for every GHC version supported by haskell.nix, eg. `nix develop .#ghc927`
            // lib.mapAttrs (c: _: let
              p = cabalProject.appendModule {
                compiler-nix-name = c;
                # Remove tools that work or should be used only with default compiler
                shell.tools = lib.mkForce (removeAttrs cabalProject.args.shell.tools ["fourmolu" "hlint"]);
              };
            in
              p.shell // (profillingShell p))
            nixpkgs.haskell-nix.compiler;
          # formatter used by nix fmt
          formatter = nixpkgs.alejandra;
        }
    );

  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
      # drop this, once we stop needing it; when we have stable aarch64-darwin
      # builds
      "https://cache.zw3rk.com"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk="
    ];
    allow-import-from-derivation = true;
  };
}
