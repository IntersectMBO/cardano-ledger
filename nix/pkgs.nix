# our packages overlay
prev: final:
import ./latex.nix {inherit (final) stdenv lib texlive;}
// {
  cddl = final.callPackage ./pkgs/cddl {};
}
