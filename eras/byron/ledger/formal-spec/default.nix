{ pkgs ? import ../../../../nix/default.nix {} }:

with pkgs;

latex.buildLatex {
  name = "byron-ledger-spec";
  texFiles = [ "byron-ledger" ];
  meta = with lib; {
    description = "Byron Ledger Specification";
    license = licenses.asl20;
    platforms = platforms.linux;
  };
  src = latex.filterLatex ./.;

  texInputs = {
                    inherit (texlive)
                      scheme-small

                      # libraries
                      stmaryrd lm-math amsmath extarrows cleveref semantic tikz-cd xcolor

                      # bclogo and dependencies
                      bclogo mdframed xkeyval etoolbox needspace pgf

                      # font libraries `mathpazo` seems to depend on palatino
                      # , but it isn't pulled.
                      mathpazo palatino microtype

                      # libraries for marginal notes
                      xargs todonotes

                      # build tools
                      latexmk

                      # Referencing
                      zref
                      ;
  };
  buildInputs = [ gitMinimal ];
}
