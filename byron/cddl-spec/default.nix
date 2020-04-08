{ pkgs ? (import  ../../nix/default.nix  {}).pkgs
}:

with pkgs;

stdenv.mkDerivation {
  name = "docsEnv";
  buildInputs = [ (texlive.combine {
                    inherit (texlive)
                      scheme-small

                      # libraries
                      unicode-math lm-math amsmath
                      enumitem bclogo xcolor newunicodechar
                      appendix syntax

                      # build tools
                      latexmk
                      ;
                  })
                  # CBOR scheme specification related tools
                  cddl
                  cbor-diag
                ];
  src = ./.;
  buildPhase = "make";

  meta = with lib; {
    description = "Byron blocks CDDL specification";
    license = licenses.bsd3;
    platforms = platforms.linux;
  };
}
