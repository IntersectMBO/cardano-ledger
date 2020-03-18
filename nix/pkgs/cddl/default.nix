{ lib, bundlerApp, bundlerUpdateScript }:

bundlerApp {
  pname = "cddl";
  gemdir = ./.;
  exes = [ "cddl" ];
}
