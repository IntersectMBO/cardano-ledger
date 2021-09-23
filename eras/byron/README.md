# Formal and executable specifications for the cardano chain

This directory is organized as follows:

- [`ledger/formal-spec`](ledger/formal-spec) contains the LaTeX specification of Cardano
  ledger semantics.
- [`ledger/executable-spec`](ledger/executable-spec) contains an executable specification of Cardano
  ledger semantics.
- [`chain/formal-spec`](chain/formal-spec) contains the LaTeX specification of Cardano
  chain semantics.
- [`chain/executable-spec`](chain/executable-spec) contains an executable specification of Cardano chain
  semantics.

## Building the documents

To build the `LaTeX` document run:

```shell
nix-shell --pure --run make
```

For a continuous compilation of the `LaTeX` file run:

```shell
nix-shell --pure --run "make watch"
```

## Building the executable specification

The executable specifications can be built and tested using
[Nix](https://nixos.org/nix/).

To build to go to the directory in which the executable specifications are
(e.g. [`ledger/executable-spec`](ledger/executable-spec)) and then run:

```sh
nix-build
```

To start a REPL first make sure to run the configure script:

```sh
nix-shell --pure --run "runhaskell Setup.hs configure"
```

then run:

```sh
nix-shell --pure --run "runhaskell Setup.hs repl"
```

To test run:

```sh
nix-shell --pure --run "runhaskell Setup.hs test"
```

### Development

For running the tests you can use:

```sh
nix-shell --pure --command "cabal new-test <target>"
```

Example, while in the `byron/ledger/executable-spec` directory one can run:

```sh
nix-shell --pure --run "cabal new-test ledger-delegation-test"
```

To have the warnings not being treated as errors the `development` flag can be
used, e.g.:

```sh
nix-shell --pure --run "cabal new-test ledger-delegation-test -fdevelopment"
```
