{ lib, latex, texlive }:

latex.buildLatex {
  name = "shelley-mc-spec";
  texFiles = [ "multi-asset" ];
  meta = with lib; {
    description = "Shelley multi-currency specification";
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
