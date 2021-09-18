{ pkgs ? import ../../../nix/default.nix {} }:

with pkgs;

latex.buildLatex {
  name = "shelley-ma-spec";
  texFiles = [ "shelley-ma" ];
  meta = with lib; {
    description = "Shelley multi-asset specification";
    license = licenses.asl20;
    platforms = platforms.linux;
  };
  src = latex.filterLatex ./.;

  texInputs = {
    inherit (texlive)
      scheme-small
      collection-latexextra
      collection-latexrecommended
      collection-mathscience
      bclogo
      ;
  };
}
