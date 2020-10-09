{ lib, latex, texlive }:

latex.buildLatex {
  name = "goguen-spec";
  texFiles = [ "goguen-changes" ];
  meta = with lib; {
    description = "Goguen ledger specification";
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
