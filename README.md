<h1 align="center">Formal Models for Ledger Rules</h1>

<p align="center">
  <a href="https://buildkite.com/input-output-hk/cardano-ledger-specs">
    <img alt="Build Status" src="https://img.shields.io/buildkite/a94c23758aeb2858869d5e256e466fc78e03a5baf1954cb8cc.svg?style=for-the-badge"/>
  </a>
  <a href="https://coveralls.io/github/input-output-hk/cardano-ledger-specs?branch=master">
    <img alt="Coverage Status" src="https://img.shields.io/coveralls/github/input-output-hk/cardano-ledger-specs.svg?style=for-the-badge"/>
  </a>
</p>

Formal and executable specifications for the new features to be introduced by Shelley.

The documents are built in our CI and can be readily accessed using the
following links:

- [Delegation Design Specification](https://hydra.iohk.io/job/Cardano/cardano-ledger-specs/delegationDesignSpec/latest/download-by-type/doc-pdf/delegation_design_spec)
- [Shelley specification](https://hydra.iohk.io/job/Cardano/cardano-ledger-specs/shelleyLedgerSpec/latest/download-by-type/doc-pdf/ledger-spec)
- [Non-integer Calculations Specification](https://hydra.iohk.io/job/Cardano/cardano-ledger-specs/nonIntegerCalculations/latest/download-by-type/doc-pdf/non-integer-calculations)
- [Byron Chain Specification](https://hydra.iohk.io/job/Cardano/cardano-ledger-specs/byronChainSpec/latest/download-by-type/doc-pdf/blockchain-spec)
- [Byron Ledger Specification](https://hydra.iohk.io/job/Cardano/cardano-ledger-specs/byronLedgerSpec/latest/download-by-type/doc-pdf/ledger-spec)
- [Explanation of the Small-step-semantics Framework](https://hydra.iohk.io/job/Cardano/cardano-ledger-specs/semanticsSpec/latest/download-by-type/doc-pdf/semantics-spec)
- [Simple Script-Based Multi-Signature Scheme](https://hydra.iohk.io/job/Cardano/cardano-ledger-specs/shelleyLedgerSpec/latest/download-by-type/doc-pdf/multi-sig)

In addition, there is a formalization of the Ledger Specification in Isabelle/HOL which can be found [here](https://github.com/input-output-hk/fm-ledger-formalization).

# Repository structure

This repo contains formal (LaTeX) and executable (Haskell model) specs for both
the Byron and Shelley eras of Cardano. The outline of the specs is as follows:

- [byron](./byron)
  - [ledger](./byron/ledger)
    - [formal-spec](./byron/ledger/formal-spec)
    - [executable-spec](./byron/ledger/executable-spec)
  - [chain](./byron/chain)
    - [formal-spec](./byron/chain/formal-spec)
    - [executable-spec](./byron/chain/executable-spec)
- [shelley](./shelley)
  - [design-spec](./shelley/design-spec)
  - [chain-and-ledger](./shelley/chain-and-ledger) (specs are combined in Shelley era)
    - [formal-spec](./shelley/chain-and-ledger/formal-spec)
    - [executable-spec](./shelley/chain-and-ledger/executable-spec)
    - [dependencies](./shelley/chain-and-ledger/dependencies)

## Build tools

For building LaTeX documents we use
[`nix`](https://nixos.org/nix/download.html). Haskell files can be built either
with `nix` or [`stack`](https://docs.haskellstack.org/en/stable/README/).

When using `nix` it is recommended that you setup the cache, so that it can
reuse built artifacts, reducing the compilation times dramatically:

If you are using [NixOS](https://nixos.org/) add the snippet below to your
`/etc/nixos/configuration.nix`:

```
nix.binaryCaches = [
  "https://cache.nixos.org"
  "https://hydra.iohk.io"
];

nix.binaryCachePublicKeys = [
  "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
];
```

If you are using the `nix` package manager next to another operating system put
the following in `/etc/nix/nix.conf` if you have a system-wide `nix`
installation , or in `~/.config/nix/nix.conf` if you have a local installation:

```
substituters        = https://hydra.iohk.io https://cache.nixos.org/
trusted-public-keys = hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=
```

## Building the LaTeX documents and executable specifications

When using `nix` the documents and executable specifications can be readily
built by running:

```shell
nix build
```

The LaTeX documents will be places inside directories named `result*`, e.g.:

```shell
result-2/ledger-spec.pdf
result-3/delegation_design_spec.pdf
result-4/non-integer-calculations.pdf
result-5/small-step-semantics.pdf
result-6/ledger-spec.pdf
result/blockchain-spec.pdf
```


## Building individual LaTeX documents


Change to the latex directory where the latex document is (e.g. `shelley/chain-and-ledger/formal-spec`
for the ledger specification corresponding to the Shelley release, or
`byron/ledger/formal-spec` for the ledger specification corresponding to
the Byron release). Then, build the latex document by running:

```shell
nix-shell --pure --run make
```

For a continuous compilation of the `LaTeX` file run:

```shell
nix-shell --pure --run "make watch"
```

## Testing the Haskell executable specifications

Change to the directory where the executable specifications are (e.g.
`shelley/chain-and-ledger/executable-spec` for the executable ledger specifications corresponding to
the Shelley release, or `byron/ledger/executable-spec` for the executable ledger specifications
corresponding to the Byron release). Then the tests can be run by executing:

```shell
stack test
```

For the executable models test suites that use `tasty` (e.g. Byron), it is possible to select which
tests to run by passing the `-p` flag to the test program, followed by an `awk` pattern. For
instance for running only the `UTxO` tests, we can pass the `-p UTxO` option. `tasty` allows for
more [complex patterns](https://github.com/feuerbach/tasty#patterns), for instance, to run only the
update mechanism tests for the ledger that classify traces, we can pass the `-p $1 ~ /Ledger/ && $2
~ /Update/ && $3 ~ /classified/` option. Here each `$i` refers to a level in the tests names
hierarchy. Passing `-l` to `tasty` will list the available test names.

When testing using `stack`, pay special attention to escaping the right symbols, e.g.:

```sh
stack test cs-ledger:test:ledger-rules-test --ta "-p \"\$1 ~ /Ledger/ \&\& \$2 ~ /Update/ \&\& \$3 ~ /classified/\""
```

Alternatively, it is also possible to use `ghcid` if it is installed in your system. In this case,
it can be helpful to run ghcid in a separate shell:

```shell
make ghcid
```

or with tests included:

```shell
make ghcid-test
```

---

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
4. Run the [regenerate](./nix/regenerate.sh) script to
   rebuild the nix configuration from your stack.yaml file.
5. Test that you can build your new project by running the following: `nix build
   -f default.nix nix-tools.libs.<project_name>`. If you have executables, then
   you may also try building these using the `nix-tools.exes.<executable_name>`
   attribute path. A good way to see what's available is to execute `:l
   default.nix` in `nix repl`. This will allow you to explore the potential
   attribute names.
5. If you want your product to be tested by CI, add it to
   [release.nix](./release.nix) using the format specified in that file.

## To add a new LaTeX specification

To add a new LaTeX specification, the easiest way is to copy from one of the
existing specifications. You will want the `Makefile` and `default.nix` (say
from [the Shelley ledger spec](./shelley/chain-and-ledger/formal-spec)).

1. Copy these files into the root of your new LaTeX specification.
2. Modify the `DOCNAME` in the `Makefile`.
3. Update `default.nix` to:
   1. Make sure that the relative path in the first line is pointing to
      (default.nix)[./default.nix]. This is used to pin the
      `nixpkgs` version used to build the LaTeX specifications.
   2. Update the `buildInputs` to add in any LaTeX packages you need in your
      document, and remove any unneeded ones.
   3. Alter the `meta` description field to reflect the nature of this document.
4. Add a link to the package at the bottom of [default.nix](./default.nix),
   following the existing examples.
5. To require that your specification be built in CI, add it at the end of the
   list in [default.nix](./default.nix) following the existing examples.

## Additional documentation

You can find additional documentation on the nix infrastructure used in this
repo in the following places:

- [The haskell.nix user guide](https://github.com/input-output-hk/haskell.nix/blob/documentation/docs/user-guide.md)
- [The nix-tools repository](https://github.com/input-output-hk/nix-tools)
- [The iohk-nix repository](https://github.com/input-output-hk/iohk-nix)

Note that the user guide linked above is incomplete and does not correctly refer
to projects built using `iohk-nix`, as this one is. A certain amount of trial
and error may be required to make substantive changes!

<p align="center">
  <a href="https://github.com/input-output-hk/cardano-ledger-specs/blob/master/LICENSE">
    <img src="https://img.shields.io/github/license/input-output-hk/cardano-ledger-specs.svg?style=for-the-badge"/>
  </a>
</p>

# Contributing

## Code formatting

We use [`editorconfig`](https://editorconfig.org/) to ensure consistency in the format of our
Haskell code. There are editorconfig plugins for several text editors, so make sure that your editor
honors the configuration in [`.editorconfig`](.editorconfig).

Additionally, we use [`stylish-haskell`](https://github.com/jaspervdj/stylish-haskell/) to format
grouped imports and language pragmas. There is a [`.stylish-haskell.yaml`](.stylish-haskell.yaml)
configuration file that determines how `stylish-haskell` formats the code. Make sure that you have a
recent version of `stylish-haskell` installed and that your editor enforces the rules defined by the
`.stylish-haskell.yaml` configuration file.

The `stylish-haskell` configuration prioritizes "diff-safety": it should introduce only minimal
changes, to avoid polluting our diffs with irrelevant information.


For Emacs, we provide [directory
variables](https://www.gnu.org/software/emacs/manual/html_node/emacs/Directory-Variables.html) to
set the `stylish-haskell` options for this project, so that `stylish-haskell` does not need to be
enabled globally (see [`.dir-locals.el`](.dir-locals.el)).
