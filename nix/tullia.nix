{
  config,
  inputs,
  ...
}: {
  imports = [inputs.tullia.flakePartsModules.default];

  perSystem = {lib, ...}: {
    tullia = let
      ciInputName = "GitHub event";
    in rec {
      tasks.ci = {
        config,
        lib,
        pkgs,
        ...
      }: {
        preset = {
          nix.enable = true;

          github.ci = {
            # Tullia tasks can run locally or on Cicero.
            # When no facts are present we know that we are running locally
            # and vice versa.
            # When running locally, the current directory is already
            # bind-mounted into the container, so we don't need to fetch the
            # source from GitHub and we don't want to report a GitHub status.
            enable = config.actionRun.facts != {};
            repository = "input-output-hk/cardano-ledger";
            revision = config.preset.github.lib.readRevision ciInputName null;
          };
        };

        command.text = config.preset.github.status.lib.reportBulk {
          bulk.text = ''
            nix eval .#hydraJobs --apply __attrNames --json | nix-systems -i
          '';
          each.text = ''nix build -L .#hydraJobs."$1".required'';
          skippedDescription =
            lib.escapeShellArg
            "No nix builder available for this system";
        };

        memory = 1024 * 8;
        nomad.resources.cpu = 10000;
      };

      actions."cardano-ledger/ci" = {
        task = "ci";
        io = ''
          // This is a CUE expression that defines what events
          // trigger a new run of this action.
          // There is no documentation for this yet.
          // Ask SRE if you have trouble changing this.

          let github = {
            #input: "${ciInputName}"
            #repo: "input-output-hk/cardano-ledger"
          }

          #lib.merge
          #ios: [
            #lib.io.github_push & github,
            { #lib.io.github_pr, github, #target_default: false },
          ]
        '';
      };
    };
  };
}
