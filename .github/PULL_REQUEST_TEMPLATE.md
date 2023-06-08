# Description

<!-- Add your description here, if it fixes a particular issue please provide a
[link](https://docs.github.com/en/issues/tracking-your-work-with-issues/linking-a-pull-request-to-an-issue#linking-a-pull-request-to-an-issue-using-a-keyword=)
to the issue. -->

# Checklist

- [ ] Commit sequence broadly makes sense and commits have useful messages
- [ ] New tests are added if needed and existing tests are updated
- [ ] When applicable, versions are updated in `.cabal` and `CHANGELOG.md` files according to the
      [versioning process](https://github.com/input-output-hk/cardano-ledger/blob/master/RELEASING.md#versioning-process).
- [ ] The version bounds in `.cabal` files for all affected packages are updated. **If you change the bounds in a cabal file, that package itself must have a version increase.** (See [RELEASING.md](https://github.com/input-output-hk/cardano-ledger/blob/master/RELEASING.md#versioning-process))
- [ ] All visible changes are prepended to the latest section of a `CHANGELOG.md` for the affected packages. **New section is never added with the code changes.** (See [RELEASING.md](https://github.com/input-output-hk/cardano-ledger/blob/master/RELEASING.md#changelogmd))
- [x] Code is formatted with [`fourmolu`](https://github.com/fourmolu/fourmolu) (use `scripts/fourmolize.sh`)
- [x] Cabal files are formatted (use `scripts/cabal-format.sh`)
- [x] [`hie.yaml`](https://github.com/input-output-hk/cardano-ledger/blob/master/hie.yaml) has been updated (use `scripts/gen-hie.sh`)
- [ ] Self-reviewed the diff
