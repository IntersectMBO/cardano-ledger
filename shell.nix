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

  # For building the sphinx doc
  pyEnv =
    # TODO: deduplicate with default.nix
    let
      sphinx-markdown-tables = pkgs.python3Packages.callPackage ./nix/python/sphinx-markdown-tables.nix {};
      sphinxemoji = pkgs.python3Packages.callPackage ./nix/python/sphinxemoji.nix {};
    in pkgs.python3.withPackages (ps: [ ps.sphinx ps.sphinx_rtd_theme ps.recommonmark sphinx-markdown-tables sphinxemoji ]);

  # This provides a development environment that can be used with nix-shell or
  # lorri. See https://input-output-hk.github.io/haskell.nix/user-guide/development/
  shell = cardanoLedgerSpecsHaskellPackages.shellFor {
    name = "cabal-dev-shell";

    # If shellFor local packages selection is wrong,
    # then list all local packages then include source-repository-package that cabal complains about:
    packages = ps: lib.attrValues (haskell-nix.haskellLib.selectProjectPackages ps);

    # These programs will be available inside the nix-shell.
    buildInputs = with haskellPackages; [
      niv
      pkg-config
      hlint
      ormolu.ormolu
      pyEnv
    ];

    tools = {
      cabal = "3.4.0.0";
      ghcid = "0.8.7";
      haskell-language-server="latest";
    };

    # Prevents cabal from choosing alternate plans, so that
    # *all* dependencies are provided by Nix.
    exactDeps = false;

    inherit withHoogle;
  };

  devops = pkgs.stdenv.mkDerivation {
    name = "devops-shell";
    buildInputs = [
      niv zlib
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
