# cardano-ledger

A re-implementation of the Cardano blockchain layer, started in Q4 2018.

The formal specifications and associated executable specifications can be found
in the
[`cardano-ledger-specs`](https://github.com/input-output-hk/cardano-ledger-specs)
repository.

[![Build status](https://badge.buildkite.com/92690086997996d4f9703ef752c0e918a02bb389b44d0659a0.svg?branch=master)](https://buildkite.com/input-output-hk/cardano-ledger)
[![Coverage Status](https://coveralls.io/repos/github/input-output-hk/cardano-ledger/badge.svg?branch=master)](https://coveralls.io/github/input-output-hk/cardano-ledger?branch=master)

## Developing

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


## Formatting

This repo uses `brittany` to encourage a consistent formatting style.

If you have `brittany` installed, the `scripts/brittany/brittany-all-hs` script
will run it over all `.hs` files with our `brittany` config file.

Otherwise, if your build fails the `brittany` CI tests, the buildkite page will
include a `git` patch that you can apply and amend you your commit.
