let
  localPkgs = import ./. { };
  mainShell = localPkgs.shell;
in mainShell // {
  inherit (localPkgs) runCoveralls;
}
