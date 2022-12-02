{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Pretty.Conway
  ( ppConwayTxBody,
  )
where

import Cardano.Ledger.Babbage.TxBody (BabbageEraTxBody (..))
import Cardano.Ledger.Babbage.TxOut (BabbageTxOut (..))
import Cardano.Ledger.Conway.Core (ConwayEraTxBody, GovernanceActionInfo (..), Vote)
import Cardano.Ledger.Conway.Delegation.Certificates (ConwayDCert (..))
import Cardano.Ledger.Conway.TxBody (ConwayTxBody (..))
import Cardano.Ledger.Core (EraTxBody (..), EraTxOut (..), Value)
import Cardano.Ledger.Pretty
  ( PDoc,
    PrettyA (..),
    ppAuxiliaryDataHash,
    ppCoin,
    ppDelegCert,
    ppGenesisDelegCert,
    ppKeyHash,
    ppNetwork,
    ppPoolCert,
    ppRecord,
    ppSafeHash,
    ppSet,
    ppSexp,
    ppStrictMaybe,
    ppStrictSeq,
    ppTxIn,
    ppTxOut,
    ppWdrl,
  )
import Cardano.Ledger.Pretty.Mary (ppMultiAsset, ppValidityInterval)
import Lens.Micro ((^.))

instance
  ( TxOut era ~ BabbageTxOut era,
    ConwayEraTxBody era,
    PrettyA (Value era),
    TxBody era ~ ConwayTxBody era
  ) =>
  PrettyA (ConwayTxBody era)
  where
  prettyA = ppConwayTxBody

ppConwayDCert :: ConwayDCert c -> PDoc
ppConwayDCert (ConwayDCertDeleg dc) = ppSexp "ConwayDCertDeleg" [ppDelegCert dc]
ppConwayDCert (ConwayDCertPool pc) = ppSexp "ConwayDCertPool" [ppPoolCert pc]
ppConwayDCert (ConwayDCertGenesis gdc) = ppSexp "ConwayDCertGenesis" [ppGenesisDelegCert gdc]

ppConwayTxBody ::
  forall era.
  ( ConwayEraTxBody era,
    PrettyA (Value era),
    TxOut era ~ BabbageTxOut era,
    TxBody era ~ ConwayTxBody era
  ) =>
  ConwayTxBody era ->
  PDoc
ppConwayTxBody txb@ConwayTxBody {..} =
  ppRecord
    "TxBody (Conway)"
    [ ("spending inputs", ppSet ppTxIn ctbSpendInputs),
      ("collateral inputs", ppSet ppTxIn ctbCollateralInputs),
      ("reference inputs", ppSet ppTxIn ctbReferenceInputs),
      ("outputs", ppStrictSeq (ppTxOut @era) (txb ^. outputsTxBodyL)),
      ("collateral return", ppStrictMaybe (ppTxOut @era) (txb ^. collateralReturnTxBodyL)),
      ("total collateral", ppStrictMaybe ppCoin ctbTotalCollateral),
      ("certificates", ppStrictSeq ppConwayDCert ctbCerts),
      ("withdrawals", ppWdrl ctbWdrls),
      ("transaction fee", ppCoin ctbTxfee),
      ("validity interval", ppValidityInterval ctbVldt),
      ("required signer hashes", ppSet ppKeyHash ctbReqSignerHashes),
      ("mint", ppMultiAsset ctbMint),
      ("script integrity hash", ppStrictMaybe ppSafeHash ctbScriptIntegrityHash),
      ("auxiliary data hash", ppStrictMaybe ppAuxiliaryDataHash ctbAdHash),
      ("network id", ppStrictMaybe ppNetwork ctbTxNetworkId),
      ("governance actions", ppStrictSeq ppGovAction ctbGovActions),
      ("votes", ppStrictSeq ppVote ctbVotes)
    ]

ppVote :: Vote era -> PDoc
ppVote = undefined

ppGovAction :: GovernanceActionInfo era -> PDoc
ppGovAction = undefined
