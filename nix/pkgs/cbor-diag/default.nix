{ lib, bundlerApp, bundlerUpdateScript }:

bundlerApp {
  pname = "cbor-diag";
  gemdir = ./.;
  exes = [
    "cbor2diag.rb"
    "cbor2json.rb"
    "cbor2pretty.rb"
    "cbor2yaml.rb"
    "diag2cbor.rb"
    "diag2pretty.rb"
    "json2cbor.rb"
    "json2pretty.rb"
    "pretty2cbor.rb"
    "pretty2diag.rb"
    "yaml2cbor.rb"
  ];
}
