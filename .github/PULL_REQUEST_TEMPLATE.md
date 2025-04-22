# Description

<!-- Add your description here, if it fixes a particular issue please provide a
[link](https://docs.github.com/en/issues/tracking-your-work-with-issues/linking-a-pull-request-to-an-issue#linking-a-pull-request-to-an-issue-using-a-keyword=)
to the issue. -->

# Checklist

- [ ] Commits in meaningful sequence and with useful messages.
- [ ] Tests added or updated when needed.
- [ ] `CHANGELOG.md` files updated for packages with externally visible changes.  
      **NOTE: _New section is never added with the code changes._** (See [RELEASING.md](https://github.com/intersectmbo/cardano-ledger/blob/master/RELEASING.md#changelogmd)).
- [ ] Versions updated in `.cabal` and `CHANGELOG.md` files when necessary, according to the
      [versioning process](https://github.com/intersectmbo/cardano-ledger/blob/master/RELEASING.md#versioning-process).
- [ ] Version bounds in `.cabal` files updated when necessary.  
      **NOTE: _If bounds change in a cabal file, that package itself must have a version increase._** (See [RELEASING.md](https://github.com/intersectmbo/cardano-ledger/blob/master/RELEASING.md#versioning-process)).
- [x] Code formatted (use `scripts/fourmolize.sh`).
- [x] Cabal files formatted (use `scripts/cabal-format.sh`).
- [x] CDDL files are up to date (use `scripts/gen-cddl.sh`)
- [x] [`hie.yaml`](https://github.com/intersectmbo/cardano-ledger/blob/master/hie.yaml) updated (use `scripts/gen-hie.sh`).
- [ ] Self-reviewed the diff.
