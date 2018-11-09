let
  nixpkgs = builtins.fetchTarball "https://github.com/nixos/nixpkgs/archive/38db6fdfb9672a0206a2042447132094bc05a5ea.tar.gz";
in
import nixpkgs {
  config = {
      packageOverrides = pkgs: {};
    };
}
