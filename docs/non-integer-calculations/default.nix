{ pkgs ? import ../../nix/default.nix {} }:

with pkgs;

latex.buildLatex {
  name = "non-integer-calculations-spec";
  texFiles = [ "non-integer-calculations" ];
  meta = with lib; {
    description = "Non-integer Calculations Specification";
    license = licenses.asl20;
    platforms = platforms.linux;
  };
  src = latex.filterLatex ./.;

  texInputs = {
                    inherit (texlive)
                      scheme-small

                      # libraries
                      stmaryrd lm-math amsmath extarrows cleveref semantic xcolor appendix paralist cm-super

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
