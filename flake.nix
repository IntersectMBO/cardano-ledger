{
  description = "cardano-ledger";

  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    iohkNix.url = "github:input-output-hk/iohk-nix";
    flake-utils.url = "github:hamishmack/flake-utils/hkm/nested-hydraJobs";

    CHaP.url = "github:intersectmbo/cardano-haskell-packages?ref=repo";
    CHaP.flake = false;

    cardano-mainnet-mirror.url = "github:input-output-hk/cardano-mainnet-mirror";
    cardano-mainnet-mirror.flake = false;

    # non-flake nix compatibility
    flake-compat.url = "github:edolstra/flake-compat";
    flake-compat.flake = false;

    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
  };

  outputs = inputs: let
    supportedSystems = [
      "x86_64-linux"
      "x86_64-darwin"
      # "aarch64-linux" - disable these temporarily because the build is broken
      "aarch64-darwin"
    ];
  in
    inputs.flake-utils.lib.eachSystem supportedSystems (
      system: let
        pre-commit-check = inputs.pre-commit-hooks.lib.${system}.run {
          src = ./.;
          hooks = {
            fourmolu.enable = true;
          };
        };

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

        # see flake `variants` below for alternative compilers
        defaultCompiler = "ghc928";
        # We use cabalProject' to ensure we don't build the plan for
        # all systems.
        cabalProject = nixpkgs.haskell-nix.cabalProject' ({config, ...}: {
          src = ./.;
          name = "cardano-ledger";
          compiler-nix-name = lib.mkDefault defaultCompiler;

          # CHaP input map, so we can find CHaP packages (needs to be more
          # recent than the index-state we set!). Can be updated with
          #
          #  nix flake lock --update-input CHaP
          #
          inputMap = {
            "https://chap.intersectmbo.org/" = inputs.CHaP;
          };
          cabalProjectLocal = ''
            repository cardano-haskell-packages-local
              url: file:${inputs.CHaP}
              secure: True
            active-repositories: hackage.haskell.org, cardano-haskell-packages-local
          '';

          # force LANG to be UTF-8, otherwise GHC might choke on UTF encoded data.
          shell.shellHook = ''
            # Disable running `fourmolu` via `pre-commit` by default.
            # If you want to opt-in and run it before each commit,
            # then `unset` the `SKIP` environment variable in your shell.
            # If you use `direnv`, `lorri`, `nix-direnv` etc.,
            # then you can add `unset SKIP` to your `.envrc`
            # after the nix shell is loaded (so after `use nix` / `use flake`).
            export SKIP=fourmolu
            export LANG=en_US.UTF-8
            export LC_ALL=en_US.UTF-8
            export CARDANO_MAINNET_MIRROR="${inputs.cardano-mainnet-mirror}/epochs"
          '' + lib.optionalString (nixpkgs.glibcLocales != null && nixpkgs.stdenv.hostPlatform.libc == "glibc") ''
            export LOCALE_ARCHIVE="${nixpkgs.glibcLocales}/lib/locale/locale-archive"
            DEFAULT_PS1="\n\[\033[1;32m\][nix-shell:\w]\$\[\033[0m\] "
            prompt() {
              local EXIT="$?"
              if [ $EXIT != 0 ]; then
                PS1="$DEFAULT_PS1\[\033[1;31m\]($EXIT)\[\033[00m\] "
              else
                PS1="$DEFAULT_PS1"
              fi
            }
            PROMPT_COMMAND=prompt
            ${pre-commit-check.shellHook}
          '';

          # tools we want in our shell, from hackage
          shell.tools =
            {
              cabal = "3.10.2.1";
              ghcid = "0.8.9";
            }
            // lib.optionalAttrs (config.compiler-nix-name == defaultCompiler) {
              # tools that work only with default compiler
              fourmolu = "0.14.0.0";
              hlint = "3.6.1";
              haskell-language-server = "2.4.0.0";
            };

          # and from nixpkgs or other inputs
          shell.nativeBuildInputs = with nixpkgs;
            [
              (python3.withPackages (ps: with ps; [sphinx sphinx_rtd_theme recommonmark sphinx-markdown-tables sphinxemoji]))
              haskellPackages.implicit-hie
            ];
          # disable Hoogle until someone request it
          shell.withHoogle = false;
          # Skip cross compilers for the shell
          shell.crossPlatforms = _: [];

          # package customizations as needed. Where cabal.project is not
          # specific enough, or doesn't allow setting these.
          modules = [
            ({pkgs, ...}: {
              # packages.plutus-core.components.library.ghcOptions = [ "-fexternal-interpreter" ];
              # uncomment if necessary when profiling
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
                packages.cardano-ledger-shelley.components.tests.tests.build-tools = [pkgs.cddl pkgs.cbor-diag];
                packages.cardano-ledger-allegra.components.tests.tests.build-tools = [pkgs.cddl pkgs.cbor-diag];
                packages.cardano-ledger-mary.components.tests.tests.build-tools = [pkgs.cddl pkgs.cbor-diag];
                packages.cardano-ledger-alonzo.components.tests.tests.build-tools = [pkgs.cddl pkgs.cbor-diag];
                packages.cardano-ledger-babbage.components.tests.tests.build-tools = [pkgs.cddl pkgs.cbor-diag];
                packages.cardano-ledger-conway.components.tests.tests.build-tools = [pkgs.cddl pkgs.cbor-diag];
                packages.cardano-protocol-tpraos.components.tests.tests.build-tools = [pkgs.cddl pkgs.cbor-diag];
              })
            ({pkgs, ...}:
              lib.mkIf pkgs.stdenv.hostPlatform.isWindows {
                packages.set-algebra.components.tests.tests.buildable = lib.mkForce false;
                packages.plutus-preprocessor.buildable = lib.mkForce false;
                packages.cardano-ledger-test.buildable = lib.mkForce false;
                packages.cardano-ledger-shelley.buildable = lib.mkForce false;
                packages.cardano-ledger-allegra.buildable = lib.mkForce false;
                packages.cardano-ledger-mary.buildable = lib.mkForce false;
                packages.cardano-ledger-alonzo.buildable = lib.mkForce false;
                packages.cardano-ledger-babbage.buildable = lib.mkForce false;
                packages.cardano-ledger-conway.buildable = lib.mkForce false;
                packages.cardano-protocol-tpraos.buildable = lib.mkForce false;
              })
          ];
        });
        # ... and construct a flake from the cabal project
        flake =
          cabalProject.flake (
            lib.optionalAttrs (system == "x86_64-linux") {
              # on linux, build/test other supported compilers
              variants = lib.genAttrs ["ghc8107" "ghc962"] (compiler-nix-name: {
                inherit compiler-nix-name;
              });
            }
          );
      in
        lib.recursiveUpdate flake rec {
          project = cabalProject;
          # add a required job, that's basically all hydraJobs.
          hydraJobs =
            nixpkgs.callPackages inputs.iohkNix.utils.ciJobsAggregates
            {
              ciJobs =
                flake.hydraJobs
                // {
                  inherit (legacyPackages) doc specs;
                  inherit (checks) pre-commit-check;
                  # This ensure hydra send a status for the required job (even if no change other than commit hash)
                  revision = nixpkgs.writeText "revision" (inputs.self.rev or "dirty");
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
            #   https://github.com/intersectmbo/cardano-ledger/releases/latest/download/NAME.pdf
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

          checks.pre-commit-check = pre-commit-check;

          devShells = let
            profillingShell = p: {
              # `nix develop .#profiling` (or `.#ghc928.profiling): a shell with profiling enabled
              profiling = (p.appendModule {modules = [{enableLibraryProfiling = true;}];}).shell;
            };
          in
            profillingShell cabalProject
            # Additional shells for every GHC version supported by haskell.nix, eg. `nix develop .#ghc928`
            // lib.mapAttrs (compiler-nix-name: _: let
              p = cabalProject.appendModule {inherit compiler-nix-name;};
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
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];
    allow-import-from-derivation = true;
  };
}
