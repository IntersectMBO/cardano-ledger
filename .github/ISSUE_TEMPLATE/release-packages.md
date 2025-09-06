---
name: Release some packages
about: Use this template for tracking package releases.
title: "Release some packages"
---

### Pending issues/pull-requests to be integrated for this release:

<!--
Mention below all issues/PRs that need to be integrated for the release,
either from the same repository/project or upstream. Please consider
consulting with the package maintainers. This is to help the release engineer
track various things that the progress of release-work depends on, such as
ongoing work in the current repository that has been ordained to be part of the
release, or PRs from upstream dependencies like cardano-base or plutus, that
need to be integrated.
-->

- [ ] [Link](#)
- [ ] [Link](#)

-----

### Release checklist

Once all the pending issues/pull-requests are integrated:

- [ ] Run the following script from [CHaP](https://github.com/IntersectMBO/cardano-haskell-packages) to open a pull-request for releases.
```shellsession
$ ./scripts/add-from-github.sh https://github.com/IntersectMBO/cardano-ledger <COMMIT_HASH> \
  eras/allegra/impl \
  eras/alonzo/impl \
  eras/alonzo/test-suite \
  eras/babbage/impl \
  eras/byron/chain/executable-spec \
  eras/byron/crypto \
  eras/byron/ledger/executable-spec \
  eras/byron/ledger/impl \
  eras/conway/impl \
  eras/mary/impl \
  eras/shelley-ma/test-suite \
  eras/shelley/impl \
  eras/shelley/test-suite \
  libs/cardano-data \
  libs/cardano-ledger-api \
  libs/cardano-ledger-binary \
  libs/cardano-ledger-core \
  libs/cardano-protocol-tpraos \
  libs/non-integral \
  libs/set-algebra \
  libs/small-steps \
  libs/vector-map
```
- [ ] List the pull-request made to [CHaP](https://github.com/IntersectMBO/cardano-haskell-packages) below.
- [ ] [Create Git tags](https://github.com/IntersectMBO/cardano-ledger/blob/master/RELEASING.md#release-to-chap) for the versions of packages released on the respective commit.
- [ ] Open a pull-request to [update the change-logs](https://github.com/IntersectMBO/cardano-ledger/blob/master/RELEASING.md#release-to-chap) with new sections as the "post-release process".
- [ ] If these releases are for a specific version release of `cardano-node`, mention this in the title.

-----

### CHaP PRs

To know the exact versions and packages released, check these pull-requests on [CHaP](https://github.com/IntersectMBO/cardano-haskell-packages). 

- [Link](#)
