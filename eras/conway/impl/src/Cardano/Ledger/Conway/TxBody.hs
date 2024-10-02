module Cardano.Ledger.Conway.TxBody (
  ConwayEraTxBody (..),
  ConwayTxBody (
    ConwayTxBody,
    ctbSpendInputs,
    ctbCollateralInputs,
    ctbReferenceInputs,
    ctbOutputs,
    ctbCollateralReturn,
    ctbTotalCollateral,
    ctbCerts,
    ctbWithdrawals,
    ctbTxfee,
    ctbVldt,
    ctbReqSignerHashes,
    ctbMint,
    ctbScriptIntegrityHash,
    ctbAdHash,
    ctbTxNetworkId,
    ctbVotingProcedures,
    ctbProposalProcedures,
    ctbCurrentTreasuryValue,
    ctbTreasuryDonation
  ),
  ConwayTxBodyRaw,
  conwayTotalDepositsTxBody,
  conwayProposalsDeposits,
) where

import Cardano.Ledger.Conway.TxBody.Internal
