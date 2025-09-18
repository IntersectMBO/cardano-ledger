# our packages overlay
final: prev:
import ./latex.nix {inherit (final) stdenv lib texlive;}
// {
  cddl = final.callPackage ./pkgs/cddl {};
  cardano-ledger-release-tool = final.callPackage ./pkgs/cardano-ledger-release-tool.nix {};
}
