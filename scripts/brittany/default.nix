{ system ? builtins.currentSystem
, config ? {}
, pkgs ? import (import ../nix/fetch-nixpkgs.nix) { inherit system config; }
}:

with pkgs.lib;
with pkgs;

writeScript "check-brittany" ''
  #!${stdenv.shell}
  export PATH=${lib.makeBinPath [ findutils git haskellPackages.brittany ]}:$PATH

  set -xe

  ${builtins.readFile ./brittany-all-hs}

  fail_brittany_check () {
    git diff --text > /tmp/brittany.patch
    buildkite-agent artifact upload /tmp/brittany.patch --job $BUILDKITE_JOB_ID
    rm /tmp/brittany.patch
    echo "ERROR: You need to run brittany over your changes and commit them" >&2
    exit 1
  }

  git diff --text --exit-code || fail_brittany_check
''
