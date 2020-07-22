.. raw:: html

   <p align="center">
     <a href="https://buildkite.com/input-output-hk/cardano-ledger-specs"><img alt="Build Status" src="https://img.shields.io/buildkite/a94c23758aeb2858869d5e256e466fc78e03a5baf1954cb8cc.svg?style=for-the-badge"/></a>
     <a href="https://coveralls.io/github/input-output-hk/cardano-ledger-specs?branch=master"><img alt="Coverage Status" src="https://img.shields.io/coveralls/github/input-output-hk/cardano-ledger-specs.svg?style=for-the-badge"/></a>   </p>
   </p>

*********************************
``shelley-ledger-specs`` Overview
*********************************

Formal and executable specifications for the new features to be
introduced by Shelley.

The documents are built in our CI and can be readily accessed using the
following links:

-  `Shelley design specification`_: the primary design document for
   Cardano Shelley.
-  `Shelley ledger formal specification`_: the formal mathematical
   specification of the Shelley era ledger rules.
-  `Shelley binary format specification (CDDL)`_: the binary formats for
   the Shelley ledger using CBOR CDDL schema notation.
-  `Non-integer calculations specification`_: details on the parts of
   the Shelley specification that use real numbers.
-  `Byron chain specification`_: the formal mathematical specification
   of the Byron era chain-level rules.
-  `Byron ledger specification`_: the formal mathematical specification
   of the Byron era ledger rules.
-  `Byron binary format specification (CDDL)`_: the binary formats for
   the Byron ledger using CBOR CDDL schema notation.
-  `Explanation of the small-step-semantics framework`_: a guide to the
   notation and style used by our ledger rules.

In addition, there is a formalization of the Ledger Specification in
Isabelle/HOL which can be found `here`_.

Repository structure
====================

This repo contains formal (LaTeX) and executable (Haskell model) specs
for both the Byron and Shelley eras of Cardano. The outline of the specs
is as follows:

-  `byron`_

   -  `ledger`_

      -  [formal-spec](./byron/ledg

.. _Shelley design specification: https://hydra.iohk.io/job/Cardano/cardano-ledger-specs/delegationDesignSpec/latest/download-by-type/doc-pdf/delegation_design_spec
.. _Shelley ledger formal specification: https://hydra.iohk.io/job/Cardano/cardano-ledger-specs/shelleyLedgerSpec/latest/download-by-type/doc-pdf/ledger-spec
.. _Shelley binary format specification (CDDL): https://github.com/input-output-hk/cardano-ledger-specs/tree/master/shelley/chain-and-ledger/executable-spec/cddl-files
.. _Non-integer calculations specification: https://hydra.iohk.io/job/Cardano/cardano-ledger-specs/nonIntegerCalculations/latest/download-by-type/doc-pdf/non-integer-calculations
.. _Byron chain specification: https://hydra.iohk.io/job/Cardano/cardano-ledger-specs/byronChainSpec/latest/download-by-type/doc-pdf/blockchain-spec
.. _Byron ledger specification: https://hydra.iohk.io/job/Cardano/cardano-ledger-specs/byronLedgerSpec/latest/download-by-type/doc-pdf/ledger-spec
.. _Byron binary format specification (CDDL): https://hydra.iohk.io/job/Cardano/cardano-ledger-specs/blocksCDDLSpec/latest/download-by-type/doc-pdf/binary
.. _Explanation of the small-step-semantics framework: https://hydra.iohk.io/job/Cardano/cardano-ledger-specs/semanticsSpec/latest/download-by-type/doc-pdf/semantics-spec
.. _here: https://github.com/input-output-hk/fm-ledger-formalization
.. _byron: ./byron
.. _ledger: ./byron/ledger