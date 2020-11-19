{ pkgs ? import ../../nix/default.nix {} }:

with pkgs;

latex.buildLatex {
  name = "delegation-design-spec";
  texFiles = [ "delegation_design_spec" ];
  meta = with lib; {
    description = "Delegation Design Specification";
    license = licenses.asl20;
    platforms = platforms.linux;
  };
  src = latex.filterLatex ./.;

  texInputs = {
    inherit (texlive)
                      scheme-small

                      # fonts
                      cm-super

                      # libraries
                      stmaryrd lm-math amsmath
                      extarrows cleveref
                      titlesec

                      # font libraries `mathpazo` seems to depend on palatino, but it isn't pulled.
                      mathpazo palatino microtype

                      # libraries for marginal notes
                      xargs todonotes

                      # drawing
                      pgf

                      # build tools
                      latexmk

                      # Referencing
                      zref
                      ;

  };
  buildInputs = [ gitMinimal ];
}
