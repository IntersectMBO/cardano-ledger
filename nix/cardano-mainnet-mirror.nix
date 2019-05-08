{ pkgs, ... }:

pkgs.fetchgit {
  url = "https://github.com/input-output-hk/cardano-mainnet-mirror";
  rev = "a31ac7534ec855b715b9a6bb6a06861ee94935d9";
  sha256 = "1z51ak4f7klz5pv2kjgaj5jv6agn2aph2n172hjssmn8x1q2bdys";
}
