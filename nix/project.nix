{ repoRoot, inputs, pkgs, system, lib }:

let
  supportedCompilers = [
    # "ghc967" -- broken
    "ghc9121"
  ];

  variants = lib.genAttrs supportedCompilers (compiler-nix-name: {
    inherit compiler-nix-name;
  });

  cabalProject' = pkgs.haskell-nix.cabalProject' ({ pkgs, config, ... }:
    let
      # When `isCross` is `true`, it means that we are cross-compiling the project.
      # WARNING You must use the `pkgs` coming from cabalProject' for `isCross` to work.
      isCross = pkgs.stdenv.hostPlatform != pkgs.stdenv.buildPlatform;
    in {
      src = ../.;

      shell.withHoogle = false;

      inputMap = {
        "https://chap.intersectmbo.org/" = inputs.CHaP;
        "https://github.com/IntersectMBO/formal-ledger-specifications.git" = inputs.formal-ledger-specifications;
      };

      name = "cardano-ledger";

      compiler-nix-name = lib.mkDefault "ghc966";

      flake.variants = variants // {
        profiled = {
          modules = [{
            enableProfiling = true;
            # enableLibraryProfiling = true;
          }];
        };
      };

      cabalProjectLocal = ''
        repository cardano-haskell-packages-local
          url: file:${inputs.CHaP}
          secure: True
        active-repositories: hackage.haskell.org, cardano-haskell-packages-local
      '';

      modules = [
        ({pkgs, ...}: {
          # packages.plutus-core.components.library.ghcOptions = [ "-fexternal-interpreter" ];
          # uncomment if necessary when profiling
          packages.byron-spec-chain.ghcOptions = ["-Werror"];
          packages.byron-spec-ledger.ghcOptions = ["-Werror"];
          packages.non-integral.ghcOptions = ["-Werror"];
          packages.cardano-ledger-shelley.ghcOptions = ["-Werror"];
          packages.cardano-ledger-shelley-ma-test.ghcOptions = ["-Werror"];
          packages.small-steps.ghcOptions = ["-Werror"];
          packages.cardano-ledger-byron = {
            ghcOptions = ["-Werror"];
            components = {
              tests.tests = {
                preCheck = ''
                  export CARDANO_MAINNET_MIRROR="${inputs.cardano-mainnet-mirror}/epochs"
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

  cabalProject = cabalProject'.appendOverlays [ ];

  # Docs for mkHaskellProject: https://github.com/input-output-hk/iogx/blob/main/doc/api.md#mkhaskellproject
  project = lib.iogx.mkHaskellProject {
    inherit cabalProject;

    shellArgs = repoRoot.nix.shell;

    readTheDocs = {
      enable = false; # TODO: broken
      siteFolder = "doc";
      # sphinxToolchain = pkgs.python3.withPackages (py: [
      #   py.sphinx
      #   py.sphinx_rtd_theme
      #   py.sphinx-markdown-tables
      #   py.sphinxemoji
      #   py.recommonmark
      #   for regenerating requirements.txt
      #   py.pip-tools
      # ]);
    };
  };

in project
