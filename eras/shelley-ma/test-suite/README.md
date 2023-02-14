##### NOTE on ghc-options in the package cabal file.

We set a memory bound here so that we're alerted of potential space
leaks in our generators (or test) code.

The 4 megabytes stack bound and 250 megabytes heap bound were
determined ad-hoc.

> This is here because comments in .cabal files do not survive a re-formatting by `cabal format`.
