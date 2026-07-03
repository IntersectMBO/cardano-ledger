{ inputs, system }:

let
  nixpkgs = import ./pkgs.nix { inherit inputs system; };
  inherit (nixpkgs) lib;

  utils = import ./utils.nix {
    pkgs = nixpkgs;
    inherit lib;
  };
  inherit (utils) fourmoluVersion cabalGildVersion nixfmtVersion;

  cabalProject = import ./project.nix {
    inherit inputs system lib;
    pkgs = nixpkgs;
  };

  # ... and construct a flake from the cabal project
  flake = cabalProject.flake (lib.optionalAttrs (system == "x86_64-linux") {
    # on linux, build/test other supported compilers
    variants = lib.genAttrs [ "ghc967" "ghc9141" ]
      (compiler-nix-name: { inherit compiler-nix-name; });
  });
in lib.recursiveUpdate flake rec {
  project = cabalProject;
  # add a required job, that's basically all hydraJobs.
  hydraJobs = {
    required = utils.makeHydraRequiredJob (flake.hydraJobs // {
      inherit (legacyPackages) doc specs;

      # This ensure hydra send a status for the required job (even if no change other than commit hash)
      revision = nixpkgs.writeText "revision" (inputs.self.rev or "dirty");
    });
  };
  legacyPackages = rec {
    inherit cabalProject nixpkgs;
    # also provide hydraJobs through legacyPackages to allow building without system prefix:
    inherit hydraJobs;
    doc.site = ((import ../doc/flake.nix).outputs {
      main = inputs.self;
    }).packages.${system}.default;
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
      paths = map (d:
        ((import "${../.}/${d}/flake.nix").outputs {
          main = inputs.self;
        }).packages.${system}.default) [
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
          "eras/byron/ledger/impl/cddl-spec"
        ];
    };
  };

  devShells = let
    mkDevShells = p: {
      # `nix develop .#profiling` (or `.#ghc967.profiling): a shell with profiling enabled
      profiling = (p.appendModule {
        modules = [{ enableLibraryProfiling = true; }];
      }).shell;
      # `nix develop .#pre-commit` (or `.#ghc967.pre-commit): a shell with pre-commit enabled
      pre-commit = let
        pre-commit-check = inputs.pre-commit-hooks.lib.${system}.run {
          src = ../.;
          hooks = {
            fourmolu.enable = true;
            cabal-gild.enable = true;
            shellcheck.enable = true;
            nixfmt-classic.enable = true;
          };
          tools = {
            fourmolu = p.tool "fourmolu" fourmoluVersion;
            cabal-gild = p.tool "cabal-gild" cabalGildVersion;
            shellcheck = nixpkgs.shellcheck;
            nixfmt = p.tool "nixfmt" nixfmtVersion;
          };
        };
      in p.shell.overrideAttrs
      (old: { shellHook = old.shellHook + pre-commit-check.shellHook; });
    };
  in mkDevShells cabalProject
  # Additional shells for every GHC version supported by haskell.nix, eg. `nix develop .#ghc9124`
  // lib.mapAttrs (compiler-nix-name: _:
    let p = cabalProject.appendModule { inherit compiler-nix-name; };
    in p.shell // (mkDevShells p)) nixpkgs.haskell-nix.compiler;
  # formatter used by nix fmt
  formatter = nixpkgs.alejandra;
}
