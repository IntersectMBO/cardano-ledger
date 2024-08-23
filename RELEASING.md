# Versioning and release processes

## Versioning Process

Ledger team has fully adopted the [PVP versioning scheme](https://pvp.haskell.org/) for
all packages in this repo.

Updating the version for every package in this repository is the responsibility of the
developer that is introducing a change to any package that is subject to release
process. Version must be updated in both the `CHANGELOG.md` and in the `.cabal` file for
the package in the same PR, as well as every other package that is affected by the change
in the repository. Every affected package that is subject to release process must receive
a lower and, potentially, an upper bound update on the dependency that has experienced the
change.

For example if I add a function to `cardano-ledger-core` and use it in
`cardano-ledger-conway`, then I must add a minor version bump to `cardano-ledger-core` and
I also must add a lower version restriction in `cardano-ledger-conway` for
`cardano-ledger-core`, as per PVP.

In other words, when it comes to versioning, the fact that all of the packages in this
repository are built together at the same time should be ignored. It should be assumed
that every package and all of its required dependencies can be released to CHaPs at any
point in time. This is necessary to guarantee that if such a package is compatible with
other packages from this repository, which have been previously released, the version
bounds should reflect that.

### Exceptions to the PVP versioning scheme

In certain instances, we don't strictly follow the PVP. One such case is when we are adding
orphan instances to `testlib`s. As per the PVP, this should warrant a major version bump.
However, we treat `testlib` orphans as regular instances since they are only defined for
types that are local to the package that contains the `testlib` where the orphan must live.
Hence this won't cause breakage for dependent packages and thus it is enough to only bump
the minor version.
Another exception that we make is related to [upper bounds](#upper-bounds) mentioned below.

### Upper bounds

Normally, [upper bounds are required by
PVP](https://pvp.haskell.org/#dependencies-in-cabal) for all dependencies of a
package. This is one exception we make to the PVP, and we do not require strict upper
bounds for dependencies that come from Hackage. For example, there is no need to add an
upper bound on `containers` package, unless there is a known incompatibility. Speculative
upper bounds over time lead to overly restrictive build plans that are forced to favor
older packages. Moreover, they lead to a lot of redundant maintenance burden. That being
said, most of the packages from within `cardano-ledger` repository are a lot more volatile
and require tight interoperability, therefore speculative upper bounds are encouraged.

### Lower bounds

Placing lower bounds is a lot less controversial than upper bounds, because they don't
suffer from speculation about the future. Older versions of dependencies are known at
release time. Therefore lower bounds must be added for all dependencies that are known to
be incompatible with the package

### Handbook

See the [versioning section](https://github.com/input-output-hk/cardano-engineering-handbook/blob/main/policy/haskell/packaging/versioning.md)
of the Cardano Engineering Handbook for more detailed information on this topic.

### `CHANGELOG.md`

Every package, that [is subject to the release
process](packages-excluded-from-release-process), will always have the top most entry in
the `CHANGELOG.md` set to an unreleased version. That top most section will have either
the patch, minor or the major versions bumped, but not all three, when compared to the
latest released version. Which one it will be depends on the recent changes that were
added after the latest release was made. More on why this should always be true is in the
[Release Process](#release-process) section. For instance, if the latest version of
`cardano-ledger-core` that was released to CHaPs is `1.2.3.0`, then there will be a
section like this which will have a version that is strictly higher than `1.2.3.0`, eg:

```
# Version history for `cardano-ledger-core`

## 1.2.3.1

* Add ...
...
```

In order to decide if the version needs to be bumped up it is necessary to know what was
the latest released version of a package. Two simple ways are either look at the version
on CHaP or look at the latest git tag for the version.

However the easiest, while also reliable way to figure out whether you need to bump up the
version is to simply look at the current version in the changelog. It will be easier to
explain why this is the case with an example.

Let's say you submit a PR which contains breaking changes to `cardano-ledger-core`. You
then look into the changelog for current development version on `master`:

* If the current version is set to `cardano-ledger-core-1.2.3.5`. We see that the patch
  version is bumped (not zero), that means that the latest version released is
  `cardano-ledger-core-1.2.3.4`, so you update it to `cardano-ledger-core-1.3.0.0`,
  because your changes break stuff
* If the current version is set to `cardano-ledger-core-1.2.4.0`. That means there was at
  most a minor version bump and currently released version is
  `cardano-ledger-core-1.2.3.x`. But your changes are breaking so you increase it even
  further to `cardano-ledger-core-1.3.0.0`
* If the current version is `cardano-ledger-core-1.3.0.0`. That means currently released
  version to CHaPs is `cardano-ledger-core-1.2.x.x`. There is nothing to do, the version
  on `master` already contains some other breaking changes since the last release to CHaPs
* If the current version is `cardano-ledger-core-2.0.0.0`. There was a complete
  overhaul. Definitely nothing needs to be bumped. :grin:


It is quite common to experience conflicts in the changelog, since that will be the most
common section of the codebase being updated at the same time. When it comes to conflicts
resolution, it is pretty easy:

* on the package version, the highest value should usually win.
* on the change log entries, all entries should usually survive.

New entries should always be prepended to the **top** of the section.

#### When to update

`CHANGELOG.md` must be updated for the package that is experiencing a change and any other
package in the repository that re-exports that change in one form or another. In general
an entry is added according to these rules:

* Every breaking change MUST appear in the `CHANGELOG.md`.

* Every non-breaking change SHOULD appear in the `CHANGELOG.md`

* Every change that does not affect the user facing semantics of the code (eg. changes to
  documentation, dependency bounds, test suites, addition of internal functions,
  performance improvements, etc.)  COULD be added to the `CHANGELOG.md`, but they are
  discouraged. Exceptions should be made for changes that could really be valuable to the
  downstream user.

* Every change to a public sub-library of a package must be added to a separate section in
  the `CHANGELOG.md` and is versioned together with the main library. Eg. `testlib`:

  ```
  # Version history for `cardano-ledger-core`

  ## 1.20.2.0

  * Add `someFunction`

  ### `testlib`

  * Add `Arbitrary` instance for ...
  ...
  ```

## Release Process

Usually a package release will happen directly from the `master` branch. Current release
process even allows for a release to happen from a commit that is slightly behind `HEAD`
of the `master` branch. On a rare occasion when a bugfix needs to be backported to an
older version, a release can happen from an ephemeral release branch. Such a release
branch must follow the same procedure of a PR review and CI, that is why it should be
prefixed with `release/some-branch-name`, because that will ensure that CI is triggered.

### Release Steps

Here are the steps for a release engineer to follow.

#### Decide what to release

Normally it should be OK to release at the same time all of the packages from the
repository that have experienced some sort of change, but that is not mandatory. It is
fine to release any package by itself at any point from the `HEAD` of `master` or even a
commit that is behind the `HEAD` on the `master` branch. A release from an ephemeral
branch is also possible, but it is paramount to ensure that all of the same changes will
be present in the next release from `master` branch.

The most common case is to blindly release all of the packages that were changed since
they were last released. This begs a question. How to decide which of the packages have
changed, thus deserve a release?

The rule is very simple. Every package that falls under this release process and has a
version in its cabal file higher than the highest version released to CHaPs is allowed to
be released and CHaP will not allow to release a package version that has already been
released. Therefore, it safe to use this command to release all packages from the repo
from a sepecific `<SHA>` that had a version bump when compared to CHaP:

```shell
$ ./scripts/add-from-github.sh https://github.com/IntersectMBO/cardano-ledger <SHA> \
  eras/allegra/impl \
  eras/alonzo/impl \
  eras/alonzo/test-suite eras/babbage/impl \
  eras/babbage/test-suite eras/conway/impl \
  eras/conway/test-suite \
  eras/mary/impl \
  eras/shelley/impl \
  eras/shelley/test-suite \
  eras/shelley-ma/test-suite \
  libs/cardano-ledger-api \
  libs/cardano-ledger-core \
  libs/cardano-ledger-binary \
  libs/cardano-protocol-tpraos \
  libs/non-integral \
  libs/small-steps \
  libs/cardano-data \
  libs/set-algebra \
  libs/vector-map \
  eras/byron/chain/executable-spec \
  eras/byron/ledger/executable-spec \
  eras/byron/ledger/impl \
  eras/byron/ledger/impl/test \
  eras/byron/crypto \
  eras/byron/crypto/test
```

More on that command in the section below.

#### Release to CHaP

1. Follow the [CHaP release
   instructions](https://github.com/intersectmbo/cardano-haskell-packages#-from-github)

   For example, to release commit with SHA `deadbeef`:

   ```shell
   $ ./scripts/add-from-github.sh https://github.com/intersectmbo/cardano-ledger deadbeef libs/cardano-ledger-core ...
   ```
   It is important to supply a commit SHA instead of a branch name.

2. Create and merge a PR to https://github.com/intersectmbo/cardano-haskell-packages
   with the release(s). In case that a current release causes breakage on some downstream
   package due to that package lacking upper bounds, you will require to [add a revision
   for that package](https://github.com/intersectmbo/cardano-haskell-packages#how-to-add-a-new-package-metadata-revision) that fixes the bounds in the same PR as the release. Also it is
   necessary to notify the maintainers of the package via a bug report or a PR with a fix.

3. Once the PR is merged then create a git tag with the same version for the same git SHA
   that was released, eg:

   ```
   $ git tag cardano-ledger-core-1.20.1.1 deadbeef...
   $ git push tag cardano-ledger-core-1.20.1.1
   ```

4. Create a PR to `master` that updates `CHANGELOG.md` files for all of the packages that
   have just been released. The only addition to the file should be a markdown header
   section with the next patch version bumped, which must bring the `CHANGELOG.md` to the
   state of the top level section containing a version higher than the highest one ever
   released. Due to concurrent nature of editing the repository it is possible that
   `CHANGELOG.md` have already received a version bump with a section that fits the
   higher version criteria, in which case nothing needs to be added. The body of the
   section, if added, must be empty with just one single asterisk `*`.

   For example, if `cardano-ledger-core-1.20.1.1` was just released, then a new empty
   `1.20.1.2` section in the `CHANGELOG.md` must be added:

   ```markdown
   # Version history for `cardano-ledger-core`

   ## 1.20.1.2

   *

   ## 1.20.1.1

   ...
   ```

   It is important to note that the version in the cabal file should *not* be changed at
   this stage, because it will later be used for deciding which package have changed and
   can be released.

##### Adding revisions

As mentioned above, sometimes a release could break downstream packages in which case you will need to add revisions.
The instructions [here](https://github.com/IntersectMBO/cardano-ledger/blob/master/REVISIONING.md) will walk you through a specific case that we had when we released some packages in `cardano-base`
and will also provide general instructions on how to add and test revisions.

#### Backporting changes

It is occasionally necessary to release a minor version for a package that has its history
diverged from a version on `master` significantly enough to make it impossible for a minor
version to be released from `master`. In other words a patch backporting. In such a
scenario a few steps should be followed:

1. Two ephemeral branches with a prefix `release/` need to be created. Both should branch
   of from a tag of a package version that is being updated. For example if a current
   version on `master` is `cardano-ledger-core-1.22.10.0` then the latest
   `cardano-ledger-core-1.21.x` should be used as base:

   ```shell
   $ git checkout -b release/cardano-ledger-core-1.21.2.1 cardano-ledger-core-1.21.2.1
   $ git push -u origin release/cardano-ledger-core-1.21.2.1
   $ git checkout -b release/cardano-ledger-core-1.21.3.0 cardano-ledger-core-1.21.2.1
   ```

   We'll need the first branch in order to use it as base when creating a PR for code
   review.

2. Changes that need to be released should be `cherry-pick`ed from master. If a fix on
   `master` was implemented in some incompatible fashion to the current release, then it
   is fine to reimplement it anew, as long as the change being introduced is also present
   on master in some form. That requirement also concerns the changelog entry, it should
   be present in both the patched version and in the next version released from `master`.

3. Regular release process should follow from here.

4. Once the package has been released and a git tag for that release was created, both of
   the `release/` branches can be removed.

This process does not accommodate backporting fixes to versions that are at least two major
versions behind the one on `master`.

## Packages excluded from release process

We release most of the packages in this repo to [CHaP (Cardano Haskell
Packages)](https://github.com/intersectmbo/cardano-haskell-packages). However, there
are a few packages that are either used for testing, debugging or benchmarking and do not
deserve to be released into the World. They are neither released nor versioned. Bounds on
the local dependencies do not need to be updated, because they will always use the versions
for ledger packages from within the repository, rather than from CHaPs. Here is the full
list of such packages as of today:

* `libs/cardano-ledger-test`
* `libs/plutus-preprocessor`
* `libs/ledger-state`

Above list is likely to change in the future.

## Test packages

Here are test suite packages that are still subject to the versioning and release process,
but the changelog _only_ needs to indicate if breaking changes have been made:

* `cardano-ledger-shelley-test`
* `cardano-ledger-shelley-ma-test`
* `cardano-ledger-alonzo-test`
* `cardano-ledger-babbage-test`
* `cardano-ledger-conway-test`
* `cardano-crypto-test`
* `cardano-ledger-byron-test`

A changelog which indicates that no changes have been made will have the form:

```
# Version history for `cardano-ledger-conway-test`

## 1.2.0.1

*
```

A changelog which indicates that changes have been made will have the form:

```
# Version history for `cardano-ledger-conway-test`

## 1.3.0.0

* breaking changes
```

These packages are mostly used internally and are planned to be deprecated and removed in the near
future in favor of `testlib`s for each corresponding package.
