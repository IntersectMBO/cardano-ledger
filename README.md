# Formal Models for Ledger Rules

Formal and executable specifications for the new features to be introduced by Shelley.

## Building LaTeX documents

Change to the latex directory `fm-ledger-rules/latex/`.

To build the latex document run:

```shell
nix-shell --pure --run make
```

For a continuous compilation of the `LaTeX` file run:

```shell
nix-shell --pure --run "make watch"
```

## Testing the Haskell model

Change to the haskell directory `fm-ledger-rules/hs/`.

The tests can be run with stack:

```shell
stack test --pedantic
```

While developing the models, it can be helpful to run ghcid in a separate shell:

```shell
make ghcid
```

or with tests included:

```shell
make ghcid-test
```

---

[![Build Status](https://travis-ci.org/input-output-hk/fm-ledger-rules.svg?branch=master)](https://travis-ci.org/input-output-hk/fm-ledger-rules)


# nix-build Infrastructure

The artifacts in this repository can be built and tested using nix. This is
additionally used by the Hydra CI to test building, including cross-compilation
for other systems.

## To add a new Haskell project

To add a new Haskell project, you should do the following:

1. Create the project in the usual way. It should have an appropriate `.cabal` file.
2. Add the project to the [top-level stack.yaml](./stack.yaml), configuring
   dependencies etc as needed. If your project's configuration deviates too far
   from the [snapshot in
   ``cardano-prelude`](https://github.com/input-output-hk/cardano-prelude/blob/master/snapshot.yaml),
   then you may have to submit a PR there to update that snapshot.
3. At this point, test that your new project builds using `stack build <project_name>`.
4. Run the [nix-tools-generate](./scripts/nix-tools-generate.sh) script to
   rebuild the nix configuration from your stack.yaml file.
5. Test that you can build your new project by running the following: `nix build
   -f default.nix nix-tools.libs.<project_name>`. If you have executables, then
   you may also try building these using the `nix-tools.exes.<executable_name>`
   attribute path. A good way to see what's available is to execute `:l
   default.nix` in `nix repl`. This will allow you to explore the potential
   attribute names.
6. If your project uses template haskell, it may not build in the previous step.
   In this case, add it to the `th-packages` list in [pkgs.nix](./nix/pkgs.nix).
7. If you want your product to be tested by CI, add it to
   [release.nix](./release.nix) using the format specified in that file.

## To add a new LaTeX specification

To add a new LaTeX specification, the easiest way is to copy from one of the
existing specifications. You will want the `Makefile` and `default.nix` (say
from [the Shelley ledger spec](./latex)).

1. Copy these files into the root of your new LaTeX specification.
2. Modify the `DOCNAME` in the `Makefile`.
3. Update `default.nix` to:
   1. Make sure that the relative path in the first line is pointing to
      (fetch-nixpkgs.nix)[./nix/fetch-nixpkgs.nix]. This is used to pin the
      `nixpkgs` version used to build the LaTeX specifications.
   2. Update the `buildInputs` to add in any LaTeX packages you need in your
      document, and remove any unneeded ones.
   3. Alter the `meta` description field to reflect the nature of this document.
4. Add a link to the package at the bottom of [default.nix](./default.nix),
   following the existing examples.
5. To require that your specification be built in CI, add it to the
   `required-targets` list in [release.nix](./release.nix) following the
   existing examples.

## Additional documentation

You can find additional documentation on the nix infrastructure used in this
repo in the following places:

[The haskell.nix user guide](https://github.com/input-output-hk/haskell.nix/blob/documentation/docs/user-guide.md)
[The nix-tools repository](https://github.com/input-output-hk/nix-tools)
[The iohk-nix repository](https://github.com/input-output-hk/iohk-nix)

Note that the user guide linked above is incomplete and does not correctly refer
to projects built using `iohk-nix`, as this one is. A certain amount of trial
and error may be required to make substantive changes!
