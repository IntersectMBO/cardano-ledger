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
import Cardano.Ledger.Conway.Core (ConwayEraTxBody, GovernanceAction, Vote)
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
    [ ("spending inputs", ppSet ppTxIn _spendInputs),
      ("collateral inputs", ppSet ppTxIn _collateralInputs),
      ("reference inputs", ppSet ppTxIn _referenceInputs),
      ("outputs", ppStrictSeq (ppTxOut @era) (txb ^. outputsTxBodyL)),
      ("collateral return", ppStrictMaybe (ppTxOut @era) (txb ^. collateralReturnTxBodyL)),
      ("total collateral", ppStrictMaybe ppCoin _totalCollateral),
      ("certificates", ppStrictSeq ppConwayDCert _certs),
      ("withdrawals", ppWdrl _wdrls),
      ("transaction fee", ppCoin _txfee),
      ("validity interval", ppValidityInterval _vldt),
      ("required signer hashes", ppSet ppKeyHash _reqSignerHashes),
      ("mint", ppMultiAsset _mint),
      ("script integrity hash", ppStrictMaybe ppSafeHash _scriptIntegrityHash),
      ("auxiliary data hash", ppStrictMaybe ppAuxiliaryDataHash _adHash),
      ("network id", ppStrictMaybe ppNetwork _txNetworkId),
      ("governance actions", ppStrictSeq ppGovAction _govActions),
      ("votes", ppStrictSeq ppVote _votes)
    ]

ppVote :: Vote era -> PDoc
ppVote = undefined

ppGovAction :: GovernanceAction era -> PDoc
ppGovAction = undefined
