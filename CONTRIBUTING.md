# Contributing to the Cardano Ledger

## Branching Model

We use [trunk based developement](https://trunkbaseddevelopment.com/).
In particular, releases will be handled by release branches,
starting with `release/1.0.x`.
Normal development will branch off of master and be merged back to master.
Only bug-fixes can be cherry-picked onto the release branches.

We use tags on the release branches to indicate patches, of the form `ledger/a.b.c`.
We also use tags to indicate what version of the ledger was used in
cardano-node releases, of the form `node/a.b.c` (possible with a `rc` or `rc1`, etc).

## Building

See the [Readme](https://github.com/input-output-hk/cardano-ledger#building) for instructions on building.

## Warnings

While building most compilation warnings will be turned into an error due to
`-Werror` flag. However during development it might be a bit inconvenient thus
can be disabled on per project basis:

```shell
cabal configure <package-name> --ghc-options="-Wwarn"
cabal build <package-name>
```

## Testing the Haskell programs

The tests can be run with cabal.
For example the Shelley tests can be run with:

```shell
cabal test cardano-ledger-shelley-test
```

**Note** that the tests in `cardano-ledger-shelley-test` require two Ruby gems,
[cbor-diag](https://rubygems.org/gems/cbor-diag) and
[cddl](https://rubygems.org/gems/cddl).

It can be helpful to use the `--test-show-details=streaming` option for seeing
the output of the tests while they run:

```shell
cabal test cardano-ledger-shelley-test --test-show-details=streaming
```

### Running Specific Tests

The test suites use [Tasty](https://github.com/feuerbach/tasty),
which allows for running specific tests.
This is done by passing the `-p` flag to the test program, followed by an `awk` pattern.
You can alternatively use the `TASTY_PATTERN` environment variable with a pattern.
For example, the Shelley golden tests can be run with:

```shell
cabal test cardano-ledger-shelley-test --test-options="-p golden"
```

or

```shell
TASTY_PATTERN=golden cabal test cardano-ledger-shelley-test
```

`Tasty` allows for more
[complex patterns](https://github.com/feuerbach/tasty#patterns).
For instance, to run only the Byron update mechanism tests for the ledger
that classify traces, we can pass the
`-p $1 ~ /Ledger/ && $2 ~ /Update/ && $3 ~ /classified/` option.
Here each `$i` refers to a level in the tests names hierarchy.
Passing `-l` to `tasty` will list the available test names.

When testing using `cabal`, pay special attention to escaping the right symbols, e.g.:

```shell
cabal test byron-spec-ledger:test:byron-spec-ledger-test --test-options "-p \"\$1 ~ /Ledger/ && \$2 ~ /Update/ && \$3 ~ /classified/\""
```

### Replaying QuickCheck Failures

When a QuickCheck test fails, the seed which produced the failure is reported.
The failure can be replayed with:

```shell
cabal test cardano-ledger-shelley-test --test-options "--quickcheck-replay=42"
```
(where 42 is an example seed).

### Test Scenarios

Most of the test suites are grouped into test scenarios.
For example, the Shelley test suite contains
`ContinuousIntegration`, `Development`, `Nightly`, and `Fast`,
which can be run with the `--scenario` flag. For example:

```shell
cabal test cardano-ledger-shelley-test --test-options --scenario=Fast
```

### ghcid

We have support for running
[ghcid](https://github.com/ndmitchell/ghcid)
from inside of nix-shell.
Enter nix-shell from the base directory of the repository,
change directories to the cabal package that you wish to check,
then run `ghcid`.

For example:

```shell
nix-shell
cd eras/shelley/impl/
ghcid
```

## nix-build Infrastructure

The artifacts in this repository can be built and tested using nix. This is
additionally used by the Hydra CI to test building, including cross-compilation
for other systems.

### To add a new Haskell package

To add a new Haskell package, you should do the following:

1. Create the project in the usual way. It should have an appropriate `.cabal` file.
2. Test that you can build your new project by running the following: `nix build
   -f default.nix libs.<project_name>`. If you have executables, then
   you may also try building these using the `exes.<executable_name>`
   attribute path. A good way to see what's available is to execute `:l
   default.nix` in `nix repl`. This will allow you to explore the potential
   attribute names by using tab completion on "libs.".

### To add a new LaTeX specification

To add a new LaTeX specification, the easiest way is to copy from one of the
existing specifications. You will want the `Makefile` and `default.nix` (say
from [the Shelley ledger spec](./eras/shelley/formal-spec)).

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

### Additional documentation

You can find additional documentation on the nix infrastructure used in this
repo in the following places:

- [The haskell.nix user guide](https://github.com/input-output-hk/haskell.nix/blob/documentation/docs/user-guide.md)
- [The nix-tools repository](https://github.com/input-output-hk/nix-tools)
- [The iohk-nix repository](https://github.com/input-output-hk/iohk-nix)

Note that the user guide linked above is incomplete and does not correctly refer
to projects built using `iohk-nix`, as this one is. A certain amount of trial
and error may be required to make substantive changes!

## Working Conventions

### Code formatting

We use [`ormolu`](https://github.com/tweag/ormolu/) for formatting.
There is a script [here](https://github.com/input-output-hk/cardano-ledger/blob/master/scripts/ormolise.sh)
which uses nix to format the appropriate directories.

### Compiler warnings

The CI builds Haskell code with -Werror, so will fail if there are any compiler warnings.

If the warnings are stupid, we can turn them off, e.g. sometimes it makes sense to add -Wno-orphans.

### Commit messages

Summarize changes in around 50 characters or less.

Provide more detailed explanatory text, if necessary.
Wrap it to about 72 characters or so.
In some contexts, the first line is treated as the
subject of the commit and the rest of the text as the body.
The blank line separating the summary from the body is critical
(unless you omit the body entirely);
various tools like `log`, `shortlog` and `rebase` can get
confused if you run the two together.

Explain the problem that this commit is solving,
and use one commit per conceptual change.
Focus on why you are making this change as opposed to how (the code explains that).
Are there side effects or other unintuitive consequences of this
change? Here's the place to explain them.

Further paragraphs come after blank lines.

 - Bullet points are okay, too

 - Typically a hyphen or asterisk is used for the bullet, preceded
   by a single space, with blank lines in between, but conventions
   vary here

If you use an issue tracker, put references to them at the bottom,
like this:

Resolves: #123
See also: #456, #789

### Commit signing

Commits are required to be [signed](https://docs.github.com/en/authentication/managing-commit-signature-verification/signing-commits).

### Pull Requests

Keep commits to a single logical change where possible.
The reviewer will be happier, and you’ll be happier if you ever have to revert it.
If you can’t do this (say because you have a huge mess), best to just have one commit with everything in it.

Keep your PRs to a single topic.
Including unrelated changes makes things harder for your reviewers, slowing them down, and makes it harder to integrate new changes.

If you’re working on something that’s likely to conflict with someone else, talk to them. It’s not a race.

## Performance

### Memory

The [ledger-state](https://github.com/input-output-hk/cardano-ledger/tree/master/libs/ledger-state)
tool is helpful for obverserving the memory overhead of the ledger state.

### Profiling

A good way to profile the ledger code is to use the
[db-analyser](https://github.com/input-output-hk/ouroboros-network/blob/master/ouroboros-consensus-cardano/tools/db-analyser/Documentation.md)
to replay block validation from mainnet.

First, inside the ouroboros repository base directory,
open a nix shell with profiling enabled:
```
~/ouroboros-network$ nix-shell --arg config "{ haskellNix.profiling = true; }"
```

Configure cabal to build everything with profiling enabled:
```
cabal configure --enable-profiling --profiling-detail=all-functions
```

Now we need to run a node to build up the dataabase.
This can be done in the cardano-node repository by
opening a nix-shell and running:
```
nix build -f default.nix scripts.mainnet.node
./result/bin/cardano-node-mainnet
```
This will take a very long time.
You can stop the node once it is past any slots that you care about.

Change directories back to the ouroboros-network repository.
Download the [mainnet config files](https://hydra.iohk.io/job/Cardano/iohk-nix/cardano-deployment/latest-finished/download/1/index.html).

Create a snapshot at the slot that you wish the profiling to start. We use 45288084 in this example:

```
cabal run db-analyser -- --db ~/io/cardano-node/state-node-mainnet/db-mainnet/ --minimum-block-validation cardano --configByron mainnet-byron-genesis.json --configShelley mainnet-shelley-genesis.json --nonce 1a3be38bcbb7911969283716ad7aa550250226b76a61fc51cc9a9a35d9276d81 --configAlonzo mainnet-alonzo-genesis.json --only-immutable-db --store-ledger 45288084
```

The value of the nonce used above can be discovered in the
[config](https://github.com/input-output-hk/cardano-node/blob/61da26bddd4d34a5ec750492aa625c62941d808a/configuration/cardano/mainnet-config.json#L15a).

Finally, 

Run the block validation, say for 1000 slots, with:
```
cabal run db-analyser -- --db <PATH_TO_NODE>/cardano-node/state-node-mainnet/db-mainnet/ --minimum-block-validation cardano --configByron mainnet-byron-genesis.json --configShelley mainnet-shelley-genesis.json --configAlonzo mainnet-alonzo-genesis.json --only-immutable-db --analyse-from 45288084 --num-blocks-to-process 1000 --trace-ledger +RTS -pj -l-agu -RTS
```

This produces the profiling file `db-analyser.prof`.

### Odds and Ends

See the [wiki](https://github.com/input-output-hk/cardano-ledger/wiki) for some other odds and ends.
