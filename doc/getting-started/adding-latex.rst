To add a new LaTeX specification
--------------------------------

To add a new LaTeX specification, the easiest way is to copy from one of
the existing specifications. You will want the ``Makefile`` and
``default.nix`` (say from `the Shelley ledger
spec <./shelley/chain-and-ledger/formal-spec>`__).

1. Copy these files into the root of your new LaTeX specification.
2. Modify the ``DOCNAME`` in the ``Makefile``.
3. Update ``default.nix`` to:

   1. Make sure that the relative path in the first line is pointing to
      (default.nix)[./default.nix]. This is used to pin the ``nixpkgs``
      version used to build the LaTeX specifications.
   2. Update the ``buildInputs`` to add in any LaTeX packages you need
      in your document, and remove any unneeded ones.
   3. Alter the ``meta`` description field to reflect the nature of this
      document.

4. Add a link to the package at the bottom of
   `default.nix <./default.nix>`__, following the existing examples.
5. To require that your specification be built in CI, add it at the end
   of the list in `default.nix <./default.nix>`__ following the existing
   examples.