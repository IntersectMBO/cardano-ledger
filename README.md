<h1 align="center">Cardano Ledger</h1>

<p align="center">
  <a href="https://buildkite.com/input-output-hk/cardano-ledger">
    <img alt="Build Status" src="https://img.shields.io/buildkite/92690086997996d4f9703ef752c0e918a02bb389b44d0659a0/master.svg?style=for-the-badge"/>
  </a>
  <a href="https://coveralls.io/github/input-output-hk/cardano-ledger?branch=master">
    <img alt="Coverage Status" src="https://img.shields.io/coveralls/github/input-output-hk/cardano-ledger/master.svg?style=for-the-badge"/>
  </a>
</p>

A re-implementation of the Cardano ledger layer, designed to ease transition between the Byron and Shelley eras.

The formal specifications and associated executable specifications can be found
in the
[`cardano-ledger-specs`](https://github.com/input-output-hk/cardano-ledger-specs)
repository.


## Demo Mainnet Validation

The `validate-mainnet` package contains an executable that integrates the
`cardano-ledger` validation logic with the `cardano-shell` application wrapper.
It validates mainnet blocks held in the `cardano-mainnet-mirror` package and
demonstrates:
- We have implemented ledger validation compatible with the existing chain
- We are ready to integrate with other parts of the system, including consensus,
  logging, and benchmarking

You can run the demo using stack by running
```
stack build && stack exec validate-mainnet
```


## Building

`cardano-ledger` can be built using `stack`, `cabal`, `nix`, or a combination.

To use `stack` or `cabal` without `nix`, you first need to make sure you have
`git`, `openssl`, and `zlib` installed on your machine. You should do this using
a package manager appropriate to your OS. After that you can simply use `stack
build` or `cabal new-build`.

Alternatively you can use `nix` to install the external dependencies. For
`stack` simply add the `--nix` flag to your invocation of `stack build`. If
you're on `NixOS` this will happen automatically. For `cabal`, you can run
`nix-shell nix/stack-shell.nix` to enter a shell with the dependencies,
and use `cabal new-build` as normal from there.

You can build directly with `nix`, by running `nix-build -A
nix-tools.libs.cardano-ledger`. To run the test executable you must first build
it with `nix-build -A nix-tools.tests.cardano-ledger.cardano-ledger-test -o
cardano-ledger-test` and then run it with `./cardano-ledger-test`.


## Running Specific Tests

The `cardano-ledger-test` test suite uses `tasty`, so you can choose to run only
some of the tests using the `-p` flag followed by a pattern. Simple patterns
such as `foo` just check for `foo` anywhere in the tests name or the name of the
groups that hold it. So to run only test for the `UTxO` you could add `-p UTxO`.
The patterns are actually `awk` expressions, so you can refer to the [`tasty`
documentation](http://hackage.haskell.org/package/tasty) to help with more
complex pattern matching.


## Local Dependencies

The `cardano-ledger` library depends on other libraries of the `input-output-hk`
organization, whose versions are pinned in the `stack.yaml` file, e.g.:

```yaml
extra-deps:
  - git: https://github.com/input-output-hk/cardano-prelude
    commit: ff5fd5f33849be8a826506c34e5b0278f267f804
    subdirs:
      - .
      - test

  - git: https://github.com/input-output-hk/cardano-ledger
    commit: 4d9eec080374179bf15bf9c4fca09cc7d73e5f53
    subdirs:
      - crypto
      - crypto/test
```

Some of these extra dependencies, like `cardano-crypto-wrapper` above, have
their source files in a sub-directory of the `cardano-ledger` repository. When
developing, sometimes it is necessary to modify not only `cardano-ledger` but
also one of these local dependencies. To avoid having to pin local libraries to
a specific commit when developing, which allows us to test the changes in a more
convenient manner, we recommend using a custom `stack` configuration file,
`stack-local.yaml`, which specifies that the local dependencies are located in
their corresponding sub-folders. For instance, the `extra-deps` field above,
could be modified as follows (in the `stack-local.yaml` file):

```yaml
extra-deps
  - ../cardano-prelude # Assuming `cardano-prelude` was checked out one directory above.
  - ../cardano-prelude/test
  - crypto
  - crypto/test
```

We do not include such `stack-local.yaml` file here, since it depends on the
particular needs of the work being done. Such file would be a copy of the
`stack.yaml` file, with the necessary adaptation. However, we do include a
`local-stack.sh` file, which can be used with the same commands and flags that
`stack` supports, e.g.:

```sh
./local-stack.sh build
```


## Updating GHC and Package Dependencies

`nix` building is handled by `nix-tools`, which generates `nix` infrastructure
from `stack` and `cabal` files. To generate the infrastructure, run
`nix/regenerate.sh`.

You should rerun this script whenever you update the dependencies in a cabal
file or you update dependency/GHC versions in the `cardano-prelude` snapshot.

So an update of GHC should be as simple as:
1. Updating `snapshot.yaml` in `cardano-prelude`
2. Updating `stack.yaml` `resolver` in `cardano-ledger`
3. Running `nix/regenerate.sh`

This may require updating the version of `iohk-nix` if the compiler version
you're switching to isn't supported in the current version of `iohk-nix`. This
will result in an error like `missing attribute 'ghc864'`. To update `iohk-nix`,
simply change the git revision in `iohk-nix.json`, this can be done automatically
by running the `update-iohk-nix.sh` script in the [`nix/`](nix/) folder.


## Formatting

This repo uses `brittany` to encourage a consistent formatting style.

If you have `brittany` installed, the `scripts/brittany/brittany-all-hs` script
will run it over all `.hs` files with our `brittany` config file.

Otherwise, if your build fails the `brittany` CI tests, the buildkite page will
include a `git` patch that you can apply and amend you your commit.


## Scaling tests according to TestScenario

This repo uses custom Template Haskell helper functions allow the number of
tests to scale for the scenarios of `Development`, `ContinuousIntegration`, and
`QualityAssurance` (as defined
[here](https://github.com/input-output-hk/cardano-ledger/blob/062983f0583852c99545efcf1a7d697dff470107/test/Test/Options.hs#L52-L55)).
This code block illustrates how to use said functionality:
```
import Test.Cardano.Prelude
import Test.Options (TestScenario, TSProperty, eachOfTS, withTestsTS)
import Hedgehog (property, (===))

ts_prop_trivial :: TSProperty
ts_prop_trivial = withTestsTS 1000 . property $ do
  True === True

ts_roundTripTrivial :: TSProperty
ts_roundTripTrivial = eachOfTS 1000 genTrivial roundTripsCBORBuildable

tests :: TestScenario -> IO Bool
tests ts = and <$> sequence
  [ H.checkParallel (($$discoverPropArg :: TestScenario -> Group) ts)
  , H.checkParallel (($$discoverRoundTripArg :: TestScenario -> Group) ts)
  ]
```
It is assumed that `genTrivial` is defined and in-scope, and that the type it
generates has appropriate instances that allow it to roundtrip.

Note that we specify a concrete number of tests to run: `1000`. This is the
number which will execute in the `ContinuousIntegration` scenario, and the
ratios by which that number will be multiplied for the other scenarios are given
[here](https://github.com/input-output-hk/cardano-ledger/blob/062983f0583852c99545efcf1a7d697dff470107/test/Test/Options.hs#L81-L91).


## Nix Tools

The `nix` directory contains files related to `iohk-nix`, the `nix`-based
infrastructure were using to manage our dependencies and build on hydra. The
important files are:

- `nix/iohk-nix-src.json`, which contains JSON pointing to the `iohk-nix` source

- `nix/lib.nix`, which imports `iohk-nix` and creates and attribute set
  containing its `lib`, `pkgs`, and `nix-tools` attributes

- `nix/regenerate.sh`, which generates `nix` expressions for all the Haskell
  dependencies in the Stack project
  
- `nix/update-iohk-nix.sh`, which updates the `iohk-nix-src.json` according to
the latest version of [`iohk-nix`](https://github.com/input-output-hk/iohk-nix/)

- `nix/pkgs.nix`, which creates a package set from the generated `nix`
  expressions

- `default.nix`, the top-level `nix` expression for the project, based on the
  generated package set

- `release.nix`, the specification for which packages to build on hydra

There are a couple of common issues that developers run into while working with
`iohk-nix`:

- `error: attribute 'ghc864' missing, at (string):1:43` is an error that you
  will usually get if you're trying to evaluate some `nix` expression using your
  system-wide `nixpkgs` and the version of `ghc` there doesn't match the one
  that you're trying to build with. This is usually solved by getting the
  offending `.nix` file to `import ./nix/lib.nix`. Sometime this occurs while
  running `stack` and this means `stack` is running with the `--nix` flag. In
  this case, you need to make sure you have a `shell.nix` file for `stack` that
  points to `nix/lib.nix`. Then add:
  ```
  nix:
    shell-file: nix/stack-shell.nix
  ```
  to your `stack.yaml`.

- While evaluating `release.nix`, e.g. using the
  `scripts/buildkite/check-hydra.sh` script, you might see
  ```
  "error": "attribute '1.0.0.0' missing, at /path/to/nix/.stack.nix/default.nix:6:30"
  ```
  which indicates that a package version is missing from the `iohk-nix` package
  set. This is usually because the version is new to Hackage, and so it has only
  recently made it into
  [`hackage.nix`](https://github.com/input-output-hk/hackage.nix/), a set of
  `nix` expressions for all the packages on Hackage. This is pinned in
  `iohk-nix`, so to solve this, submit a PR to `iohk-nix` updating the revision
  of `pins/haskell-nix.json`, or get IOHK DevOps to submit this PR for you if
  you're unsure how to do that.


<hr/>

<p align="center">
  <a href="https://github.com/input-output-hk/cardano-wallet/blob/master/LICENSE">
    <img src="https://img.shields.io/github/license/input-output-hk/cardano-wallet.svg?style=for-the-badge"/>
  </a>
</p>
