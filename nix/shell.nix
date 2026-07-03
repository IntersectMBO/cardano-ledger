{ inputs, system, nixpkgs, lib, config, }:

let
  inherit (import ./utils.nix {
    pkgs = nixpkgs;
    inherit lib;
  })
    defaultCompiler fourmoluVersion nixfmtVersion;
in {
  # Due to plutus-tx-plugin being a bit special, we need to augment the default package selection.
  packages = ps:
    builtins.attrValues (nixpkgs.haskell-nix.haskellLib.selectLocalPackages ps)
    ++ lib.optional (ps ? plutus-tx-plugin) ps.plutus-tx-plugin;

  # force LANG to be UTF-8, otherwise GHC might choke on UTF encoded data.
  shellHook = ''
    export LANG=en_US.UTF-8
    export CARDANO_MAINNET_MIRROR="${inputs.cardano-mainnet-mirror}/epochs"
  '' + lib.optionalString (nixpkgs.glibcLocales != null
    && nixpkgs.stdenv.hostPlatform.libc == "glibc") ''
      export LOCALE_ARCHIVE="${nixpkgs.glibcLocales}/lib/locale/locale-archive"
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

  # tools we want in our shell, from hackage
  tools = {
    cabal = "3.16.1.0";
  } // lib.optionalAttrs (config.compiler-nix-name == defaultCompiler) {
    # tools that work only with default compiler
    cabal-gild = "1.5.0.1";
    cuddle = "latest";
    fourmolu = fourmoluVersion;
    haskell-language-server = "2.12.0.0";
    hlint = "3.8";
    nixfmt = nixfmtVersion;
  };

  # and from nixpkgs or other inputs
  nativeBuildInputs = with nixpkgs;
    with (import ./doctest.nix { inherit config nixpkgs; }); [
      (python3.withPackages (ps:
        with ps; [
          sphinx
          sphinx-rtd-theme
          recommonmark
          sphinx-markdown-tables
          sphinxemoji
        ]))
      haskellPackages.implicit-hie
      shellcheck
      act
      doctest
      inputs.cardano-ledger-release-tool.packages.${system}.default
    ];
  # disable Hoogle until someone request it
  withHoogle = false;
  # Skip cross compilers for the shell
  crossPlatforms = _: [ ];
}
