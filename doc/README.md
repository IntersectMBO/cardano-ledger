# Documentation site

This is a sphinx site. You can build it with sphinx from a `nix develop` shell:

```
sphinx-build -n . _build
```

Or via nix at the top-level:

```
nix build .#doc.site
```

[Read the Docs](https://cardano-ledger.readthedocs.io/en/latest) is building the site on every PR and publishing it on every merge.

## Updating Sphinx dependencies

The direct dependencies are specified in `requirements.in`. This file is only used to generate manually the `requirements.txt` file - which contains the direct and transitive dependencies and is actually used when building the site.

### Regenerating `requirements.txt`

* Update `requirements.in` as desired
* In `doc`, enter a `nix develop` shell that includes pip-tools:
  * ```
      cd doc
      nix develop
      pip-compile requirements.in
    ```
  This should have modified `requirements.txt` to reflect the changes from `requirements.in` and any necessary changes in transitive dependencies.
