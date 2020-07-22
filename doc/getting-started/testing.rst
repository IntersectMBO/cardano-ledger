Testing the Haskell executable specifications
---------------------------------------------

Change to the directory where the executable specifications are (e.g.
``shelley/chain-and-ledger/executable-spec`` for the executable ledger
specifications corresponding to the Shelley release, or
``byron/ledger/executable-spec`` for the executable ledger
specifications corresponding to the Byron release). Then the tests can
be run by executing:

.. code:: shell

   stack test

**Note** that the tests in ``shelley-spec-ledger`` require two Ruby
gems, `cbor-diag <https://rubygems.org/gems/cbor-diag>`__ and
`cddl <https://rubygems.org/gems/cddl>`__.

For the executable models test suites that use ``tasty`` (e.g. Byron),
it is possible to select which tests to run by passing the ``-p`` flag
to the test program, followed by an ``awk`` pattern. For instance for
running only the ``UTxO`` tests, we can pass the ``-p UTxO`` option.
``tasty`` allows for more `complex
patterns <https://github.com/feuerbach/tasty#patterns>`__, for instance,
to run only the update mechanism tests for the ledger that classify
traces, we can pass the
``-p $1 ~ /Ledger/ && $2 ~ /Update/ && $3 ~ /classified/`` option. Here
each ``$i`` refers to a level in the tests names hierarchy. Passing
``-l`` to ``tasty`` will list the available test names.

When testing using ``stack``, pay special attention to escaping the
right symbols, e.g.:

.. code:: sh

   stack test byron-spec-ledger:test:byron-spec-ledger-test --ta "-p \"\$1 ~ /Ledger/ \&\& \$2 ~ /Update/ \&\& \$3 ~ /classified/\""

Additionally, the Shelley tests are grouped into test scenarios,
``ContinuousIntegration``, ``Development``, ``Nightly``, and ``Fast``,
which can be run with the ``--scenario`` flag. For example:

.. code:: sh

   stack test shelley-spec-ledger --ta --scenario=Nightly

Alternatively, it is also possible to use ``ghcid`` if it is installed
in your system. In this case, it can be helpful to run ghcid in a
separate shell:

.. code:: shell

   make ghcid

or with tests included:

.. code:: shell

   make ghcid-test
