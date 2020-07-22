Repository structure
====================

This repo contains formal (LaTeX) and executable (Haskell model) specs
for both the Byron and Shelley eras of Cardano. The outline of the specs
is as follows:

-  `byron <./byron>`__

   -  `ledger <./byron/ledger>`__

      -  `formal-spec <./byron/ledger/formal-spec>`__
      -  `executable-spec <./byron/ledger/executable-spec>`__

   -  `chain <./byron/chain>`__

      -  `formal-spec <./byron/chain/formal-spec>`__
      -  `executable-spec <./byron/chain/executable-spec>`__

-  `shelley <./shelley>`__

   -  `design-spec <./shelley/design-spec>`__
   -  `chain-and-ledger <./shelley/chain-and-ledger>`__ (specs are
      combined in Shelley era)

      -  `formal-spec <./shelley/chain-and-ledger/formal-spec>`__
      -  `executable-spec <./shelley/chain-and-ledger/executable-spec>`__
      -  `dependencies <./shelley/chain-and-ledger/dependencies>`__