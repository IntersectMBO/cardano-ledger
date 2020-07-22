.. raw:: html

   <p align="center">
     <a href="https://buildkite.com/input-output-hk/cardano-ledger-specs"><img alt="Build Status" src="https://img.shields.io/buildkite/a94c23758aeb2858869d5e256e466fc78e03a5baf1954cb8cc.svg?style=for-the-badge"/></a>
     <a href="https://coveralls.io/github/input-output-hk/cardano-ledger-specs?branch=master"><img alt="Coverage Status" src="https://img.shields.io/coveralls/github/input-output-hk/cardano-ledger-specs.svg?style=for-the-badge"/></a>
     <a href='https://docs.cardano.org/projects/cardano-ledger-specs/en/latest/?badge=latest'><img src='https://readthedocs.com/projects/cardano-foundation-cardano-ledger-specs/badge/?version=latest?style=for-the-badge' alt='Documentation Status'/></a>
     <a href="https://github.com/input-output-hk/cardano-ledger-specs/blob/master/LICENSE"><img src="https://img.shields.io/github/license/input-output-hk/cardano-ledger-specs.svg?style=for-the-badge"/></a>
   </p>

*********************************
``shelley-ledger-specs`` Overview
*********************************

Formal and executable specifications for the new features to be
introduced by Shelley.

The documents are built in our CI and can be readily accessed using the
following links:

-  `Shelley design
   specification <https://hydra.iohk.io/job/Cardano/cardano-ledger-specs/delegationDesignSpec/latest/download-by-type/doc-pdf/delegation_design_spec>`__:
   the primary design document for Cardano Shelley.
-  `Shelley ledger formal
   specification <https://hydra.iohk.io/job/Cardano/cardano-ledger-specs/shelleyLedgerSpec/latest/download-by-type/doc-pdf/ledger-spec>`__:
   the formal mathematical specification of the Shelley era ledger
   rules.
-  `Shelley binary format specification
   (CDDL) <https://github.com/input-output-hk/cardano-ledger-specs/tree/master/shelley/chain-and-ledger/executable-spec/cddl-files>`__:
   the binary formats for the Shelley ledger using CBOR CDDL schema
   notation.
-  `Non-integer calculations
   specification <https://hydra.iohk.io/job/Cardano/cardano-ledger-specs/nonIntegerCalculations/latest/download-by-type/doc-pdf/non-integer-calculations>`__:
   details on the parts of the Shelley specification that use real
   numbers.
-  `Byron chain
   specification <https://hydra.iohk.io/job/Cardano/cardano-ledger-specs/byronChainSpec/latest/download-by-type/doc-pdf/blockchain-spec>`__:
   the formal mathematical specification of the Byron era chain-level
   rules.
-  `Byron ledger
   specification <https://hydra.iohk.io/job/Cardano/cardano-ledger-specs/byronLedgerSpec/latest/download-by-type/doc-pdf/ledger-spec>`__:
   the formal mathematical specification of the Byron era ledger rules.
-  `Byron binary format specification
   (CDDL) <https://hydra.iohk.io/job/Cardano/cardano-ledger-specs/blocksCDDLSpec/latest/download-by-type/doc-pdf/binary>`__:
   the binary formats for the Byron ledger using CBOR CDDL schema
   notation.
-  `Explanation of the small-step-semantics
   framework <https://hydra.iohk.io/job/Cardano/cardano-ledger-specs/semanticsSpec/latest/download-by-type/doc-pdf/semantics-spec>`__:
   a guide to the notation and style used by our ledger rules.

In addition, there is a formalization of the Ledger Specification in
Isabelle/HOL which can be found
`here <https://github.com/input-output-hk/fm-ledger-formalization>`__.

nix-build Infrastructure
========================

The artifacts in this repository can be built and tested using nix. This
is additionally used by the Hydra CI to test building, including
cross-compilation for other systems.
