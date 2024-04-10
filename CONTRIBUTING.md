# Contributing to the Cardano Ledger

## Roles and responsibilities

The
[@cardano-ledger](https://github.com/orgs/input-output-hk/teams/cardano-ledger)
group is responsible for
helping with reviewing and merging pull requests, adjudicating technical (or other) disputes,
releasing the ledger packages on [CHaP](https://github.com/input-output-hk/cardano-haskell-packages).

@hamishmack can help with issues regarding this repository's continuous integration and nix infrastructure.

**For security related issues** please consult the security file in the
[Cardano engineering handbook](https://github.com/input-output-hk/cardano-engineering-handbook/blob/main/SECURITY.md).

## Development

We use trunk based developement. Normal development will branch off of master and be
merged back to master.

### Recommended `git` configuration

Once you cloned the repository, it is recommended to run the following
from the repository's root:
```bash
git config blame.ignoreRevsFile .git-blame-ignore-revs
```
This way `git blame` will ignore the commits specified in the `.git-blame-ignore-revs`
file. This can come in handy if you want to exclude large commits
with only formatting changes.
You can ignore the above however, if you tend to look at `git blame`
through GitHub. In that case, you don't have to do anything,
as GitHub will pick up `.git-blame-ignore-revs` automatically and ignore
the specified commits.

If you want to add further revisions to the `ignore-revs` file,
just prepend the full commit hash that you want `git blame` to ignore
and add the commit's title and date as a comment for clarity.

### Releasing and versioning

See documentation on the adopted [release and versioning processes](./RELEASING.md) in ledger.

### Releasing the ledger packages to CHaP

Ledger packages are released to [CHaP](https://github.com/input-output-hk/cardano-haskell-packages).

Also see the CHaP README for [instructions](https://github.com/input-output-hk/cardano-haskell-packages#-from-github).

## Building

See the [Readme](https://github.com/intersectmbo/cardano-ledger#building) for instructions on building.

### GHC 9.2 transition

We are transitioning to use GHC 9.2 rather than GHC 8.10.
We need to retain 8.10 compatibility until we are sure that the Cardano node can switch over to 9.2 without any problems.
At that point we can drop it.

The main `nix develop` shell will now give you a GHC 9.2 compiler, but you can get a GHC 8.10 shell by calling
```
nix develop .#ghc8107
```
(this pattern can also be used to test any supported GHC version)

## Updating dependencies

Our Haskell packages come from two package repositories:
- Hackage
- [CHaP](https://github.com/input-output-hk/cardano-haskell-packages) (which is essentially another Hackage)

The "index state" of each repository is pinned to a particular time in `cabal.project`.
This tells Cabal to treat the repository "as if" it was the specified time, ensuring reproducibility.
If you want to use a package version from repository X which was added after the pinned index state time, you need to bump the index state for X.
This is not a big deal, since all it does is change what packages `cabal` considers to be available when doing solving, but it will change what package versions cabal picks for the plan, and so will likely result in significant recompilation, and potentially some breakage.
That typically just means that we need to fix the breakage (and add a lower-bound on the problematic package), or add an upper-bound on the problematic package.

Note that `cabal` itself keeps track of what index states it knows about, so when you bump the pinned index state you may need call `cabal update` in order for `cabal` to be happy.

The Nix code which builds our packages also cares about the index state.
This is represented by inputs managed by `nix flake`:
You can update these by running:
- `nix flake lock --update-input haskellNix/hackage` for Hackage
- `nix flake lock --update-input CHaP` for CHaP (Cardano Haskell Packages)

If you fail to do this you may get an error like this from Nix:
```
error: Unknown index-state 2021-08-08T00:00:00Z, the latest index-state I know about is 2021-08-06T00:00:00Z. You may need to update to a newer hackage.nix.
```

### Use of `source-repository-package`s

We *can* use Cabal's `source-repository-package` mechanism to pull in un-released package versions.
However, we should try and avoid this.
In particular, we should not release our packages to CHaP while we depend on a `source-repository-package`.

If we are stuck in a situation where we need a long-running fork of a package, we should release it to CHaP instead (see the [CHaP README](https://github.com/input-output-hk/cardano-haskell-packages) for more).

If you do add a `source-repository-package`, you need to provide a `--sha256` comment in `cabal.project` so that Nix knows the hash of the content.

## Warnings

While building most compilation warnings will be turned into an error due to
`-Werror` flag. However during development it might be a bit inconvenient thus
can be disabled on per project basis:

```shell
cabal configure <package-name> --ghc-options="-Wwarn"
cabal build <package-name>
```

## Publishing specifications
PDF specs are stored as attachments to [github releases](https://github.com/intersectmbo/cardano-ledger/releases)
We can create a release that builds and attaches the latest specs, by triggering the [push-docs github action](https://github.com/intersectmbo/cardano-ledger/blob/master/.github/workflows/push-specs.yml).
This github action can be triggered by pushing a tag of the pattern: `cardano-ledger-spec-YYYY-MM-DD`, for example: `cardano-ledger-spec-2023-01-17`

For example, if we decide it's time to publish new versions of docs,
we can do the following to publish the PDFs under release `cardano-ledger-spec-2023-03-21`:
```
git tag cardano-ledger-spec-2023-03-21
git push origin cardano-ledger-spec-2023-03-21
```

This will create a new release that will be available as [latest](https://github.com/intersectmbo/cardano-ledger/releases/latest).
Make sure that the `YYYY-MM-DD` part in the tag name is alphabetically greater than the rest, otherwise the release won't be tagged as `latest`.
Using the current date should ensure that this is the case.

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

### Default and Nightly builds
Most test suites have two different sets of tests: default and "nightly" (which take longer to run).
The latter are being run when the environment variable NIGHTLY is set:

```shell
NIGHTLY=true cabal test cardano-ledger-shelley-test
```
### ghcid

We have support for running
[ghcid](https://github.com/ndmitchell/ghcid)
from inside of `nix develop`.
Enter `nix develop` from the base directory of the repository,
change directories to the cabal package that you wish to check,
then run `ghcid`.

For example:

```shell
nix develop
cd eras/shelley/impl/
ghcid
```

`ghcid` may complain of not being able to load multiple components at once for targets with multiple components.
In this case, just appending the `ghcid` command with the name of the component usually solves the problem.

For example, under `cardano-ledger-binary`, running `ghcid` errors out with the following output:

```shell
Error: cabal: Cannot open a repl for multiple components at once. The target '' refers to the package cardano-ledger-binary-0.1.0.0 which includes the libraries testlib and cardano-ledger-binary.
```

Specifying the component solves this problem:

```shell
nix develop
cd libs/cardano-ledger-binary/
ghcid testlib # or `ghcid cardano-ledger-binary`
```

## nix build Infrastructure

The artifacts in this repository can be built and tested using nix. This is
additionally used by the Hydra CI to test building, including cross-compilation
for other systems.

### To add a new Haskell package

To add a new Haskell package, you should do the following:

1. Create the project in the usual way. It should have an appropriate `.cabal` file.
2. Test that you can build your new project by running the following: `nix build
   .#<project_name>:lib:<lib_name>`. If you have executables, then
   you may also try building these using the `.#<project_name>:exe:<exe_name>`
   attribute path. A good way to see what's available is to execute `:lf .`
   in `nix repl`. This will allow you to explore the potential
   attribute names by using tab completion on "packages.<your_system>".

### To add a new LaTeX specification

To add a new LaTeX specification, the easiest way is to copy from one of the
existing specifications. You will want the `Makefile` and `default.nix` (say
from [the Shelley ledger spec](./eras/shelley/formal-spec)).

1. Copy these files into the root of your new LaTeX specification.
2. Modify the `DOCNAME` in the `Makefile`.
3. Update `default.nix` to:
   1. Update the `buildInputs` to add in any LaTeX packages you need in your
      document, and remove any unneeded ones.
   2. Alter the `meta` description field to reflect the nature of this document.
4. Add a link to the package near the bottom of [flake.nix](./flake.nix),
   following the existing examples.

### To update the conformance test

To update the conformance test, do the following:

1. Clone the [Agda specification repo](https://github.com/IntersectMBO/formal-ledger-specifications)
2. Run `nix-build -A ledger.hsSrc` in the cloned repo, take note of the output path
   in the nix store
3. Clone the [executable spec repo](https://github.com/input-output-hk/cardano-ledger-executable-spec)
4. Replace the content of the repo cloned above with the files at `/nix/store/<output of the nix-build>/haskell/Ledger/*`
   ```bash
   rm -rf cardano-ledger-executable-spec/*
   cp -r  /nix/store/<output of the nix-build>/haskell/Ledger/*  cardano-ledger-executable-spec
   ```
   Then make a commit and push it.
5. In the `cardano-ledger` repo, edit `cabal.project`. Look for
   `source-repository-package` that points to the executable spec repo, and
   update the `tag` and `sha256` entries in that block.

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

We use [`fourmolu`](https://github.com/fourmolu/fourmolu) for formatting.
You can either use it via a [script](https://github.com/intersectmbo/cardano-ledger/blob/master/scripts/fourmolize.sh) or use `pre-commit`.
`pre-commit` is provided via a separate `devShell` which uses
the default shell as a base and adds `pre-commit` on top.
You can use it by calling `nix develop .#pre-commit`.
If you use `direnv` in some form, you can make this your default shell
by adding a flake parameter to `use flake` in your `.envrc`. I.e:
`use flake .#pre-commit`.

When running `fourmolu` manually via the `fourmolize.sh` script,
you can instruct the script to run it only on changed files (compared to `origin/master`)
by providing the `--changes` flag. If you omit it, then `fourmolu` will format everything.

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

The [ledger-state](https://github.com/intersectmbo/cardano-ledger/tree/master/libs/ledger-state)
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
This can be done in the cardano-node repository by running:
```
nix run .#mainnet/node
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

## Architectural Decision Records

See [ADR-1](docs/adr/2022-12-01_001-record-architectural-decisions.md).

## Odds and Ends

See the [wiki](https://github.com/intersectmbo/cardano-ledger/wiki) for some other odds and ends.
