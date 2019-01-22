# cardano-chain

A re-implementation of the Cardano blockchain layer, started in Q4 2018.

The formal specifications and associated executable specifications can be found
in the [`specs`](specs/) directory.

[![Build status](https://badge.buildkite.com/92690086997996d4f9703ef752c0e918a02bb389b44d0659a0.svg?branch=master)](https://buildkite.com/input-output-hk/cardano-chain)


## Formatting

This repo uses `brittany` to enforce a consistent formatting style.

If you have `brittany` installed, the `scripts/brittany/brittany-all-hs` script
will run it over all `.hs` files with our `brittany` config file.

Otherwise, if your build fails the `brittany` CI tests, the buildkite page will
include a `git` patch that you can apply and amend you your commit.
