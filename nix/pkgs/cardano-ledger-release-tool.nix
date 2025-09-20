{
  stdenv,
  lib,
  fetchzip,
}:
# Although `cardano-ledger-release-tool` is a flake, we choose to download
# the released binary artifact for compatibility with the non-nix CI
let
  pname = "cardano-ledger-release-tool";
  version = "0.1.0.0";
  hash = "sha256-Xge1/T5mtsOF5XiIDPoWU2f2HMJ0pCEbS+brfQyCZpw=";

  repo = "input-output-hk/${pname}";
  release = "${pname}-${version}";
  url = "https://github.com/${repo}/releases/download/${release}/${pname}-Linux-x86_64.zip";
in
  stdenv.mkDerivation {
    inherit pname version;
    src = fetchzip {inherit url hash;};
    installPhase = ''
      mkdir -p $out/bin
      cp -a ${pname} $out/bin/
    '';
  }
