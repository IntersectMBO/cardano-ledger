# This file is used by nix-shell.
{ config ? {}
, sourcesOverride ? {}
, withHoogle ? false
, pkgs ? import ./nix {
    inherit config sourcesOverride;
  }
}:
with pkgs;
let
  ormolu = import pkgs.commonLib.sources.ormolu {};

  haskell-extra =
    # The Hackage index-state from cabal.project
    let checkMaterialization = false;
        index-state =
      let
        # Borrowed from haskell.nix
        parseIndexState = rawCabalProject:
            let
              indexState = pkgs.lib.lists.concatLists (
                pkgs.lib.lists.filter (l: l != null)
                  (builtins.map (l: builtins.match "^index-state: *(.*)" l)
                    (pkgs.lib.splitString "\n" rawCabalProject)));
            in pkgs.lib.lists.head (indexState ++ [ null ]);
      in parseIndexState (builtins.readFile ./cabal.project);
          # Extra Haskell packages which we use but aren't part of the main project definition.
    in pkgs.callPackage ./nix/haskell-extra.nix { inherit index-state checkMaterialization; };

  # This provides a development environment that can be used with nix-shell or
  # lorri. See https://input-output-hk.github.io/haskell.nix/user-guide/development/
  shell = cardanoLedgerSpecsHaskellPackages.shellFor {
    name = "cabal-dev-shell";

    # If shellFor local packages selection is wrong,
    # then list all local packages then include source-repository-package that cabal complains about:
    packages = ps: lib.attrValues (haskell-nix.haskellLib.selectProjectPackages ps);

    # These programs will be available inside the nix-shell.
    buildInputs = with haskellPackages; [
      haskell-extra.haskell-language-server.components.exes.haskell-language-server
      niv
      pkg-config
      hlint
      ormolu.ormolu
    ];

    tools = {
      cabal = "3.2.0.0";
    };

    # Prevents cabal from choosing alternate plans, so that
    # *all* dependencies are provided by Nix.
    exactDeps = false;

    inherit withHoogle;
  };

  devops = pkgs.stdenv.mkDerivation {
    name = "devops-shell";
    buildInputs = [
      niv
    ];
    shellHook = ''
      echo "DevOps Tools" \
      | ${figlet}/bin/figlet -f banner -c \
      | ${lolcat}/bin/lolcat

      echo "NOTE: you may need to export GITHUB_TOKEN if you hit rate limits with niv"
      echo "Commands:
        * niv update <package> - update package

      "
    '';
  };

in

 shell // { inherit devops; }
