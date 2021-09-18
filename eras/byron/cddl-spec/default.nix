{ pkgs ? import ../../../nix/default.nix {} }:

with pkgs;

latex.buildLatex {
  name = "blocks-cddl-spec";
  texFiles = [ "binary" ];
  meta = with lib; {
    description = "Byron blocks CDDL specification";
    license = licenses.asl20;
    platforms = platforms.linux;
  };
  src = ./.;

  texInputs = {
    inherit (texlive)
      scheme-small

      # Fonts
      cm-super

      # libraries
      unicode-math lm-math amsmath
      enumitem bclogo xcolor newunicodechar
      appendix syntax

      # build tools
      latexmk
      ;
  };
  buildInputs = [
    gitMinimal
    # CBOR scheme specification related tools
    cddl
    cbor-diag
  ];
}
