To add a new Haskell project
----------------------------

To add a new Haskell project, you should do the following:

1. Create the project in the usual way. It should have an appropriate
   ``.cabal`` file.
2. Add the project to the `top-level stack.yaml <./stack.yaml>`__,
   configuring dependencies etc as needed. If your project's
   configuration deviates too far from the `snapshot in
   \``cardano-prelude\` <https://github.com/input-output-hk/cardano-prelude/blob/master/snapshot.yaml>`__,   
   then you may have to submit a PR there to update that snapshot.
3. At this point, test that your new project builds using
   ``stack build <project_name>``.
4. Run `nix-shell ./nix -A iohkNix.stack-cabal-sync-shell --run
   scripts/stack-cabal_config_check.sh <./scripts/stack-cabal_config_check.sh>`__
   script to check and report your change from stack.yaml to
   cabal.project.
5. Run the `regenerate <./nix/regenerate.sh>`__ script to update sha256
   checksums in cabal.project.
6. Test that you can build your new project by running the following:
   ``nix build -f default.nix libs.<project_name>``. If you have
   executables, then you may also try building these using the
   ``exes.<executable_name>`` attribute path. A good way to see what's
   available is to execute ``:l default.nix`` in ``nix repl``. This will
   allow you to explore the potential attribute names.
7. If you want your product to be tested by CI, add it to
   `release.nix <./release.nix>`__ using the format specified in that
   file.