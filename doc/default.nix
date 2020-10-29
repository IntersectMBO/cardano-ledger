{ stdenv, lib, pythonPackages, sphinx-markdown-tables, sphinxemoji, ... }:
stdenv.mkDerivation {
  name = "ledger-docs";
  src = lib.sourceFilesBySuffices ./. [ ".py" ".rst" ".hs" ".png" ];
  buildInputs = with pythonPackages; [
    sphinx
    sphinx_rtd_theme
    sphinx-markdown-tables
    sphinxemoji
    recommonmark
  ];
  buildPhase = ''
    # -n gives warnings on missing link targets, -W makes warnings into errors
    sphinx-build -n -W . $out
  '';
  dontInstall = true;
}
