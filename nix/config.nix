{ config, ...}: {
  # tasty-hedgehog-coverage wasn't building due to tasty deps not being met.
  packages.tasty-hedgehog-coverage.components.library.doExactConfig = true;
}
