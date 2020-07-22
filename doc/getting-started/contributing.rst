Contributing
============

Code formatting
---------------

We use ```editorconfig`` <https://editorconfig.org/>`__ to ensure
consistency in the format of our Haskell code. There are editorconfig
plugins for several text editors, so make sure that your editor honors
the configuration in ```.editorconfig`` <.editorconfig>`__.

Additionally, we use
```stylish-haskell`` <https://github.com/jaspervdj/stylish-haskell/>`__
to format grouped imports and language pragmas. There is a
```.stylish-haskell.yaml`` <.stylish-haskell.yaml>`__ configuration file
that determines how ``stylish-haskell`` formats the code. Make sure that
you have a recent version of ``stylish-haskell`` installed and that your
editor enforces the rules defined by the ``.stylish-haskell.yaml``
configuration file.

The ``stylish-haskell`` configuration prioritizes "diff-safety": it
should introduce only minimal changes, to avoid polluting our diffs with
irrelevant information.

For Emacs, we provide `directory
variables <https://www.gnu.org/software/emacs/manual/html_node/emacs/Directory-Variables.html>`__
to set the ``stylish-haskell`` options for this project, so that
``stylish-haskell`` does not need to be enabled globally (see
```.dir-locals.el`` <.dir-locals.el>`__).
