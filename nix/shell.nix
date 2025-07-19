# Docs for this file: https://github.com/input-output-hk/iogx/blob/main/doc/api.md#mkhaskellprojectinshellargs

{ repoRoot, inputs, pkgs, lib, system }:

# Each flake variant defined in your project.nix project will yield a separate
# shell. If no flake variants are defined, then cabalProject is the original 
# project.
cabalProject:

{
  name = "nix-shell";

  # prompt = null;

  # welcomeMessage = null;

  packages = with pkgs;
    [
      haskellPackages.implicit-hie
    ] ++
    (let
      doctest = haskell-nix.hackage-package {
        name = "doctest";
        version = "0.24.0";
        configureArgs = "-f cabal-doctest";
        inherit (cabalProject.args) compiler-nix-name;
      };
    in
      [
        (doctest.getComponent "exe:cabal-doctest")
        (doctest.getComponent "exe:doctest")
      ]);

  env = {
    LANG = "en_US.UTF-8";
    LC_ALL = "en_US.UTF-8";
    CARDANO_MAINNET_MIRROR = "${inputs.cardano-mainnet-mirror}/epochs";
    LOCALE_ARCHIVE = lib.optionalString (pkgs.glibcLocales != null && pkgs.stdenv.hostPlatform.libc == "glibc")
      "{pkgs.glibcLocales}/lib/locale/locale-archive";
  };

  shellHook = ''
    DEFAULT_PS1="\n\[\033[1;32m\][nix-shell:\w]\$\[\033[0m\] "
    prompt() {
      local EXIT="$?"
      if [ $EXIT != 0 ]; then
        PS1="$DEFAULT_PS1\[\033[1;31m\]($EXIT)\[\033[00m\] "
      else
        PS1="$DEFAULT_PS1"
      fi
    }
    PROMPT_COMMAND=prompt
  '';

  tools = {
    haskellCompilerVersion = cabalProject.args.compiler-nix-name;
    # haskell-language-server = cabalProject.tool "haskell-language-server" "2.9.0.0";
    # fourmolu = cabalProject.tool "fourmolu" "0.18.0.0";
    # hlint = cabalProject.tool "hlint" "3.8";
  };

  # preCommit = {};
}
