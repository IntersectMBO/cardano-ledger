# Documentation site

This is a sphinx site. You can build it with sphinx from a `nix-shell`:

```
sphinx-build -n . _build
```

Or via nix at the top-level:

```
nix build -f default.nix docs.site
```

[Read the Docs](https://cardano-ledger.readthedocs.io/en/latest) is building the site on every PR and publishing it on every merge.

## Updating Sphinx dependencies

The direct dependencies are specified in `requirements.in`. This file is only used to generate manually the `requirements.txt` file - which contains the direct and transitive dependencies and is actually used when building the site.

### Regenerating `requirements.txt`

* Update `requirements.in` as desired
* At the top level, enter a nix-shell that includes pip and install pip-tools:
  * ```
      nix-shell -p python3Packages.pip
    ```
  * ```
      python -m venv .venv && source .venv/bin/activate
    ```
  * ```
      python -m pip install pip-tools
    ```
  * ```
      cd doc
      pip-compile requirements.in
    ```
  This should have modified `requirements.txt` to reflect the changes from `requirements.in` and any necessary changes in transitive dependencies.

