{
  outputs = inputs: {
    packages =
      builtins.mapAttrs (_: {nixpkgs, ...}: {
        default = with nixpkgs;
          stdenv.mkDerivation {
            name = "ledger-docs";
            src = lib.sourceFilesBySuffices ./. [".py" ".rst" ".hs" ".png"];
            buildInputs = with python3Packages; [
              sphinx
              sphinx_rtd_theme
              sphinx-markdown-tables
              sphinxemoji
              recommonmark
              # for regenerating requirements.txt
              pip-tools
            ];
            buildPhase = ''
              # -n gives warnings on missing link targets, -W makes warnings into errors
              sphinx-build -n -W . $out
            '';
            dontInstall = true;
          };
      })
      (inputs.main or (import ../nix/flake-compat.nix).defaultNix).legacyPackages;
  };
}
