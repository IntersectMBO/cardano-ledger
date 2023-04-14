# Description

Add your description here, if it fixes a particular issue please provide a
[link](https://docs.github.com/en/issues/tracking-your-work-with-issues/linking-a-pull-request-to-an-issue#linking-a-pull-request-to-an-issue-using-a-keyword=)
to the issue.

# Checklist

- [ ] Commit sequence broadly makes sense and commits have useful messages
- [ ] New tests are added if needed and existing tests are updated
- [ ] Any changes are noted in the `CHANGELOG.md` for affected package
- [ ] The version bounds in `.cabal` files are updated
- [x] Code is formatted with [`fourmolu`](https://github.com/fourmolu/fourmolu) (use `scripts/fourmolize.sh`)
- [x] Cabal files are formatted (use `scripts/cabal-format.sh`)
- [x] [`hie.yaml`](https://github.com/input-output-hk/cardano-ledger/blob/master/hie.yaml) has been updated (use `scripts/gen-hie.sh`)
- [ ] Self-reviewed the diff
