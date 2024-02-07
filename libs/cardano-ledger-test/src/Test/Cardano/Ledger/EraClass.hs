-- | Everything you need to build Era polymorphic transactions, all in one place
module Test.Cardano.Ledger.EraClass (
  Era (EraCrypto),
  -- Tx
  EraTx (Tx, mkBasicTx, bodyTxL, witsTxL),
  AlonzoEraTx (isValidTxL),
  -- TxBody
  EraTxBody (TxBody, mkBasicTxBody, inputsTxBodyL, outputsTxBodyL, feeTxBodyL, withdrawalsTxBodyL, auxDataHashTxBodyL, certsTxBodyL),
  AllegraEraTxBody (vldtTxBodyL),
  MaryEraTxBody (mintTxBodyL, mintValueTxBodyF, mintedTxBodyF),
  AlonzoEraTxBody (
    collateralInputsTxBodyL,
    reqSignerHashesTxBodyL,
    scriptIntegrityHashTxBodyL,
    networkIdTxBodyL,
    redeemerPointer,
    redeemerPointerInverse
  ),
  ConwayEraTxBody (currentTreasuryValueTxBodyL, votingProceduresTxBodyL, proposalProceduresTxBodyL, treasuryDonationTxBodyL),
  -- TxOut
  EraTxOut (TxOut, mkBasicTxOut, valueTxOutL, compactValueTxOutL, valueEitherTxOutL, addrTxOutL, compactAddrTxOutL, addrEitherTxOutL),
  AlonzoEraTxOut (dataHashTxOutL, datumTxOutF),
  BabbageEraTxOut (referenceScriptTxOutL, dataTxOutL, datumTxOutL),
  coinTxOutL,
  -- TxWits
  EraTxWits (TxWits, mkBasicTxWits, addrTxWitsL, bootAddrTxWitsL, scriptTxWitsL),
  AlonzoEraTxWits (datsTxWitsL, rdmrsTxWitsL),
  -- TxAuxData
  EraTxAuxData (TxAuxData, hashTxAuxData, validateTxAuxData),
  -- TxCerts
  EraTxCert (
    TxCert,
    getVKeyWitnessTxCert,
    getScriptWitnessTxCert,
    mkRegPoolTxCert,
    getRegPoolTxCert,
    mkRetirePoolTxCert,
    getRetirePoolTxCert,
    lookupRegStakeTxCert,
    lookupUnRegStakeTxCert,
    getTotalDepositsTxCerts,
    getTotalRefundsTxCerts
  ),
  ConwayEraTxCert (
    mkRegDepositTxCert,
    getRegDepositTxCert,
    mkUnRegDepositTxCert,
    getUnRegDepositTxCert,
    mkDelegTxCert,
    getDelegTxCert,
    mkRegDepositDelegTxCert,
    getRegDepositDelegTxCert,
    mkAuthCommitteeHotKeyTxCert,
    getAuthCommitteeHotKeyTxCert,
    mkResignCommitteeColdTxCert,
    getResignCommitteeColdTxCert,
    mkRegDRepTxCert,
    getRegDRepTxCert,
    mkUnRegDRepTxCert,
    getUnRegDRepTxCert,
    mkUpdateDRepTxCert,
    getUpdateDRepTxCert
  ),
  -- PParams
  EraPParams (ppProtocolVersionL),
  PParams,
  emptyPParams,
  ppMinFeeAL,
  ppMinFeeBL,
  ppMaxBBSizeL,
  ppMaxTxSizeL,
  ppMaxBHSizeL,
  ppKeyDepositL,
  ppPoolDepositL,
  ppEMaxL,
  ppNOptL,
  ppA0L,
  ppRhoL,
  ppTauL,
  ppDL,
  -- PParamUpdate
  PParamsUpdate,
  emptyPParamsUpdate,
  ppuMinFeeAL,
  ppuMinFeeBL,
  ppuMaxBBSizeL,
  ppuMaxTxSizeL,
  ppuMaxBHSizeL,
  ppuKeyDepositL,
  ppuPoolDepositL,
  ppuEMaxL,
  ppuNOptL,
  ppuA0L,
  ppuRhoL,
  ppuTauL,
  ppuDL,
  -- Alonzo
  AlonzoEraPParams,
  ppCoinsPerUTxOWordL,
  ppCostModelsL,
  ppPricesL,
  ppMaxTxExUnitsL,
  ppMaxBlockExUnitsL,
  ppMaxValSizeL,
  ppCollateralPercentageL,
  ppMaxCollateralInputsL,
  ppuCoinsPerUTxOWordL,
  ppuCostModelsL,
  ppuPricesL,
  ppuMaxTxExUnitsL,
  ppuMaxBlockExUnitsL,
  ppuMaxValSizeL,
  ppuCollateralPercentageL,
  ppuMaxCollateralInputsL,
  -- Babbage
  BabbageEraPParams (..),
  ppCoinsPerUTxOByteL,
  ppuCoinsPerUTxOByteL,
  -- Conway
  ConwayEraPParams,
  ppPoolVotingThresholdsL,
  ppDRepVotingThresholdsL,
  ppCommitteeMinSizeL,
  ppCommitteeMaxTermLengthL,
  ppGovActionLifetimeL,
  ppGovActionDepositL,
  ppDRepDepositL,
  ppDRepActivityL,
  ppMinFeeRefScriptCostPerByteL,
  ppuPoolVotingThresholdsL,
  ppuDRepVotingThresholdsL,
  ppuCommitteeMinSizeL,
  ppuCommitteeMaxTermLengthL,
  ppuGovActionLifetimeL,
  ppuGovActionDepositL,
  ppuDRepDepositL,
  ppuDRepActivityL,
  ppuMinFeeRefScriptCostPerByteL,
  -- Scripts
  EraScript (Script, NativeScript, getNativeScript, fromNativeScript),
  hashScript,
) where

import Cardano.Ledger.Allegra.TxBody (AllegraEraTxBody (..))
import Cardano.Ledger.Alonzo.PParams
import Cardano.Ledger.Alonzo.Tx (AlonzoEraTx (..))
import Cardano.Ledger.Alonzo.TxBody (AlonzoEraTxBody (..))
import Cardano.Ledger.Alonzo.TxOut (AlonzoEraTxOut (..))
import Cardano.Ledger.Alonzo.TxWits (AlonzoEraTxWits (..))
import Cardano.Ledger.Babbage.PParams (BabbageEraPParams (..), ppCoinsPerUTxOByteL, ppuCoinsPerUTxOByteL)
import Cardano.Ledger.Babbage.TxOut (BabbageEraTxOut (..))
import Cardano.Ledger.Conway.PParams
import Cardano.Ledger.Conway.TxBody (ConwayEraTxBody (..))
import Cardano.Ledger.Conway.TxCert (ConwayEraTxCert (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Mary.TxBody (MaryEraTxBody (..))
