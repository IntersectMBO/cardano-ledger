{ repoRoot, inputs, pkgs, lib, system }:

let

  project = repoRoot.nix.project;

in [
  (
    # Docs for project.flake: https://github.com/input-output-hk/iogx/blob/main/doc/api.md#mkhaskellprojectoutflake
    project.flake)
  {
    devShells = lib.genAttrs (lib.attrNames project.variants) (variantName:
      project.variants."${variantName}".devShell
    );
  }
]
