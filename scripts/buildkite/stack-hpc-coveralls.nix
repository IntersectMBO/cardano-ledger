{ mkDerivation, aeson, base, bytestring, containers, deepseq
, directory, docopt, fetchgit, filepath, hlint, hpc, hspec
, hspec-contrib, http-client, HUnit, lens, lens-aeson, process
, pureMD5, stdenv, text, time, unordered-containers, utf8-string
, wreq, yaml
}:
mkDerivation {
  pname = "stack-hpc-coveralls";
  version = "0.0.4.0";
  src = fetchgit {
    url = "https://github.com/input-output-hk/stack-hpc-coveralls";
    rev = "347159869bc9af2de6c6fa16e247ae060e82b954";
    sha256 = "0qsfxddbzvw3lkyvr0876vnlsm1901gy2mksbghg709wpip4xksp";
    fetchSubmodules = true;
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring containers directory filepath hpc http-client
    lens lens-aeson process pureMD5 text unordered-containers
    utf8-string wreq yaml
  ];
  executableHaskellDepends = [ aeson base bytestring docopt ];
  testHaskellDepends = [
    aeson base containers deepseq hlint hpc hspec hspec-contrib HUnit
    time
  ];
  homepage = "http://github.com/rubik/stack-hpc-coveralls";
  description = "Initial project template from stack";
  license = stdenv.lib.licenses.isc;
}
