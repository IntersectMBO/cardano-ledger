{ pkgs ? import ../../../nix/default.nix {} }:

with pkgs;

latex.buildLatex {
  name = "babbage-spec";
  texFiles = [ "babbage-ledger" ];
  meta = with lib; {
    description = "Babbage ledger specification";
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
