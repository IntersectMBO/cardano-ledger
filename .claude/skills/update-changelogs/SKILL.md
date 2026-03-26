---
name: update-changelogs
description: Update CHANGELOG.md files and cabal version numbers for all packages changed since origin/master, following PVP and project RELEASING.md rules. Use when preparing a PR or when asked to update changelogs or version numbers.
argument-hint: "[package-root]"
disable-model-invocation: true
allowed-tools: Bash(git *), Read, Grep, Glob, Edit
model: opus
effort: max
hooks:
  Stop:
    - hooks:
      - type: command
        command: "scripts/format-changelogs.sh"
---

Update `CHANGELOG.md` files and `.cabal` version numbers for packages changed since `origin/master`.

If `$ARGUMENTS` is non-empty, restrict to that package root (e.g. `libs/cardano-ledger-core`). Otherwise, update all affected packages.

Reference docs:
- **PVP rules**: https://pvp.haskell.org/
- **Project versioning & release process**: `RELEASING.md` (excluded packages, test packages, upper/lower bound policy, changelog conventions, version bump logic)

## Changed files (pre-fetched)

```
!`git diff origin/master... --name-only`
```

## Workflow

### Step 1 — Identify affected packages

Map each changed file to its package root:
- `eras/<era>/impl/` → era implementation package
- `eras/<era>/test-suite/` → era test-suite package
- `libs/<lib>/` → library package

Ignore files in excluded packages (see `RELEASING.md`) and files outside any package. If `$ARGUMENTS` was given, use only that package.

### Step 2 — Read current version

For each package, read the top-most `## <version>` heading in `CHANGELOG.md` (the current unreleased version) and the `version:` field in the `.cabal` file.

The `.cabal` version may lag behind the CHANGELOG. After a CHaP release, `./scripts/bump-changelogs.sh` adds a new empty patch-bump section to the CHANGELOG but does **not** update the `.cabal`. The **CHANGELOG version is authoritative**. When you bump, update both.

### Step 3 — Classify the changes

Read `git diff origin/master -- <package-root>` excluding `CHANGELOG.md` and `.cabal` changes. Classify using PVP rules (see https://pvp.haskell.org/) with the following **project-specific caveats**:

#### Not an API change (no bump, no entry)

These are **never** API changes regardless of where they appear:
- **Value changes** — changing a record field value, a default, a constant, or any expression. Only changes to types, signatures, and the set of exports matter.
- **Testlib `spec` entry-point functions** — not part of the API; downstream packages do not depend on them.
- **Test scenarios** — adding or modifying test cases within existing modules.

#### Project exceptions to PVP

- **Testlib orphan instances**: Minor bump (not major), because they're defined for types local to the package.
- **Test packages** (`cardano-ledger-shelley-test`, `cardano-ledger-shelley-ma-test`, `cardano-ledger-alonzo-test`): Only **breaking changes** get changelog entries. Non-breaking changes get an empty entry (`*`). See `RELEASING.md` § "Test packages".

### Step 4 — Determine the version bump

See `RELEASING.md` § "CHANGELOG.md" for the version bump decision logic. In practice only **B** is bumped for breaking changes (`A.(B+1).0.0`). Bumping **A** is extremely rare — **always ask the user first**.

**No bump needed** if the current unreleased version already reflects a bump at least as large as required.

### Step 5 — Write changelog entries

1. Update `## <version>` heading if the version changed.
2. Prepend entries after the heading, before existing entries.
3. Sub-library changes go in a subsection (`### \`testlib\``, `### \`cddl\``) **after** main library entries.

If no entry is warranted, leave the body as-is but still bump the heading if required.

**Formatting**: Imperative mood ("Add", "Remove", "Change" — not past tense). Backticks around all code identifiers. See existing entries in each `CHANGELOG.md` for style. Common patterns:

```markdown
* Add `someFunction`
* Remove deprecated `oldFunction`
* Renamed:
  - `oldName` -> `newName`
* Changed the type of the following fields to `CompactForm Coin` in `SomePParams`:
  - `fieldA`
  - `fieldB`
* Move validation from `DELEGS` to `DELEG` rule:
  - Add `SomeFailure` to `ShelleyDelegPredFailure`
  - Remove `SomeFailure` from `ShelleyDelegsPredFailure`
* Add `newFunction` and deprecate `oldFunction` in its favor

### `testlib`

* Add `withIssuerAndTxsInBlock_` and `withIssuerAndTxsInBlock`
```

### Step 6 — Update `.cabal` version

For every package whose version changed, update the `version:` field to match the new CHANGELOG version.

### Step 7 — Update dependency bounds

For every bumped package, `grep -rn '<pkg>.*<old-minor>' --include='*.cabal' .` across the **entire repo** to find stale bounds — not just in packages with changed files.

See `RELEASING.md` § "Lower bounds" and "Upper bounds" for general policy. Additional guidance:

- **Sub-library-only bumps**: When the breaking change is confined to a sub-library (e.g. `testlib`) and the main library is unchanged, only packages that **depend on and use the changed sub-library API** need a lower bound raise. Packages that only depend on the main library just need their upper bound widened.
- `^>=X.Y` means `>= X.Y && < X.(Y+1)`, so a bump from `1.18` to `1.19` requires updating all `^>=1.18` constraints to `^>=1.19`.

### Step 8 — Summarise

Print a compact table:

| Package | Old version | New version | Bump type | Reason |
|---|---|---|---|---|
| ... | ... | ... | ... | ... |

List skipped packages in a note below. Flag any uncertain classifications for review.

**If you are unsure whether a change is breaking, stop and ask the user before editing.**
