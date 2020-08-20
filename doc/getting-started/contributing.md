## Code formatting

We use [`editorconfig`](https://editorconfig.org/) to ensure consistency in the format of our
Haskell code. There are editorconfig plugins for several text editors, so make sure that your editor
honors the configuration in [`.editorconfig`](.editorconfig).

Additionally, we use [`ormolu`](https://github.com/tweag/ormolu/) for formatting. There is a script [here](https://github.com/input-output-hk/cardano-ledger-specs/blob/master/scripts/ormolise.sh) which uses nix to format the appropriate directories.