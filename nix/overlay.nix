# our packages overlay
final: prev:
import ./latex.nix { inherit (final) stdenv lib texlive; }
