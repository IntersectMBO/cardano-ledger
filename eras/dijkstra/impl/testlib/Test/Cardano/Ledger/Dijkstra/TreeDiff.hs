{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Dijkstra.TreeDiff (
  module Test.Cardano.Ledger.Conway.TreeDiff,
) where

import Cardano.Ledger.BaseTypes (PerasCert, StrictMaybe)
import Cardano.Ledger.Conway.Rules (ConwayGovEvent, ConwayUtxosPredFailure)
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Dijkstra.Core (
  AlonzoEraScript (..),
  AsItem,
  AsIx,
  DijkstraBlockBody,
  Era,
  EraPParams (..),
  EraRule,
  EraTx (..),
  EraTxBody (..),
  EraTxCert (..),
  EraTxOut (..),
  PlutusScript,
  TopTx,
  Value,
 )
import Cardano.Ledger.Dijkstra.PParams (DijkstraPParams)
import Cardano.Ledger.Dijkstra.Rules
import Cardano.Ledger.Dijkstra.Scripts (
  AccountBalanceInterval,
  AccountBalanceIntervals,
  DijkstraNativeScript,
  DijkstraNativeScriptRaw,
  DijkstraPlutusPurpose,
 )
import Cardano.Ledger.Dijkstra.Tx (DijkstraTx (..), Tx (..))
import Cardano.Ledger.Dijkstra.TxBody (DijkstraTxBodyRaw (..))
import Cardano.Ledger.Dijkstra.TxCert
import Cardano.Ledger.Dijkstra.TxInfo (DijkstraContextError)
import Control.State.Transition (STS (..))
import Data.Functor.Identity (Identity)
import qualified Data.TreeDiff.OMap as OMap
import Test.Cardano.Ledger.Conway.TreeDiff (Expr (..), ToExpr)
import Test.Cardano.Ledger.TreeDiff (ToExpr (..))

instance
  (forall a b. (ToExpr a, ToExpr b) => ToExpr (f a b)) =>
  ToExpr (DijkstraPlutusPurpose f DijkstraEra)

instance ToExpr (PlutusScript DijkstraEra)

instance ToExpr (DijkstraNativeScript era)

instance ToExpr (DijkstraNativeScriptRaw era)

instance ToExpr (DijkstraPParams Identity DijkstraEra)

instance ToExpr (DijkstraPParams StrictMaybe DijkstraEra)

instance ToExpr (DijkstraTxBodyRaw l DijkstraEra) where
  toExpr = \case
    txBody@(DijkstraTxBodyRaw _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) ->
      let DijkstraTxBodyRaw {..} = txBody
       in Rec "DijkstraTxBodyRaw" $
            OMap.fromList
              [ ("dtbrSpendInputs", toExpr dtbrSpendInputs)
              , ("dtbrCollateralInputs", toExpr dtbrCollateralInputs)
              , ("dtbrReferenceInputs", toExpr dtbrReferenceInputs)
              , ("dtbrOutputs", toExpr dtbrOutputs)
              , ("dtbrCollateralReturn", toExpr dtbrCollateralReturn)
              , ("dtbrTotalCollateral", toExpr dtbrTotalCollateral)
              , ("dtbrCerts", toExpr dtbrCerts)
              , ("dtbrWithdrawals", toExpr dtbrWithdrawals)
              , ("dtbrFee", toExpr dtbrFee)
              , ("dtbrVldt", toExpr dtbrVldt)
              , ("dtbrGuards", toExpr dtbrGuards)
              , ("dtbrMint", toExpr dtbrMint)
              , ("dtbrScriptIntegrityHash", toExpr dtbrScriptIntegrityHash)
              , ("dtbrAuxDataHash", toExpr dtbrAuxDataHash)
              , ("dtbrNetworkId", toExpr dtbrNetworkId)
              , ("dtbrVotingProcedures", toExpr dtbrVotingProcedures)
              , ("dtbrProposalProcedures", toExpr dtbrProposalProcedures)
              , ("dtbrCurrentTreasuryValue", toExpr dtbrCurrentTreasuryValue)
              , ("dtbrTreasuryDonation", toExpr dtbrTreasuryDonation)
              , ("dtbrSubTransactions", toExpr dtbrSubTransactions)
              , ("dtbrDirectDeposits", toExpr dtbrDirectDeposits)
              , ("dtbrAccountBalanceIntervals", toExpr dtbrAccountBalanceIntervals)
              ]
    txBody@(DijkstraSubTxBodyRaw _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) ->
      let DijkstraSubTxBodyRaw {..} = txBody
       in Rec "DijkstraSubTxBodyRaw" $
            OMap.fromList
              [ ("dstbrSpendInputs", toExpr dstbrSpendInputs)
              , ("dstbrReferenceInputs", toExpr dstbrReferenceInputs)
              , ("dstbrOutputs", toExpr dstbrOutputs)
              , ("dstbrCerts", toExpr dstbrCerts)
              , ("dstbrWithdrawals", toExpr dstbrWithdrawals)
              , ("dstbrVldt", toExpr dstbrVldt)
              , ("dstbrGuards", toExpr dstbrGuards)
              , ("dstbrMint", toExpr dstbrMint)
              , ("dstbrScriptIntegrityHash", toExpr dstbrScriptIntegrityHash)
              , ("dstbrAuxDataHash", toExpr dstbrAuxDataHash)
              , ("dstbrNetworkId", toExpr dstbrNetworkId)
              , ("dstbrVotingProcedures", toExpr dstbrVotingProcedures)
              , ("dstbrProposalProcedures", toExpr dstbrProposalProcedures)
              , ("dstbrCurrentTreasuryValue", toExpr dstbrCurrentTreasuryValue)
              , ("dstbrTreasuryDonation", toExpr dstbrTreasuryDonation)
              , ("dstbrRequiredTopLevelGuards", toExpr dstbrRequiredTopLevelGuards)
              , ("dstbrDirectDeposits", toExpr dstbrDirectDeposits)
              , ("dstbrAccountBalanceIntervals", toExpr dstbrAccountBalanceIntervals)
              ]

instance ToExpr (TxBody l DijkstraEra)

instance ToExpr PerasCert

instance (ToExpr (Tx TopTx era), ToExpr PerasCert) => ToExpr (DijkstraBlockBody era)

instance ToExpr (DijkstraTx l DijkstraEra) where
  toExpr = \case
    txBody@(DijkstraTx _ _ _ _) ->
      let DijkstraTx {..} = txBody
       in Rec "DijkstraTx" $
            OMap.fromList
              [ ("dtBody", toExpr dtBody)
              , ("dtWits", toExpr dtWits)
              , ("dtIsValid", toExpr dtIsValid)
              , ("dtAuxData", toExpr dtAuxData)
              ]
    txBody@(DijkstraSubTx _ _ _) ->
      let DijkstraSubTx {..} = txBody
       in Rec "DijkstraSubTx" $
            OMap.fromList
              [ ("dstBody", toExpr dstBody)
              , ("dstWits", toExpr dstWits)
              , ("dstAuxData", toExpr dstAuxData)
              ]

deriving newtype instance ToExpr (Tx l DijkstraEra)

instance ToExpr DijkstraDelegCert

instance ToExpr (DijkstraTxCert era)

instance
  ( Era era
  , ToExpr (PParamsHKD StrictMaybe era)
  , ToExpr (PlutusPurpose AsIx era)
  , ToExpr (PlutusPurpose AsItem era)
  , ToExpr (TxCert era)
  , ToExpr (TxOut era)
  ) =>
  ToExpr (DijkstraContextError era)

instance
  ( Era era
  , ToExpr (PredicateFailure (EraRule "UTXO" era))
  , ToExpr (PlutusPurpose AsIx era)
  , ToExpr (PlutusPurpose AsItem era)
  , ToExpr (TxCert era)
  ) =>
  ToExpr (DijkstraUtxowPredFailure era)

instance
  ( ToExpr (PredicateFailure (EraRule "UTXOW" era))
  , ToExpr (PredicateFailure (EraRule "GOV" era))
  , ToExpr (PredicateFailure (EraRule "CERTS" era))
  , ToExpr (PredicateFailure (EraRule "SUBLEDGERS" era))
  ) =>
  ToExpr (DijkstraLedgerPredFailure era)

instance
  ( ToExpr (Event (EraRule "UTXOW" era))
  , ToExpr (Event (EraRule "CERTS" era))
  , ToExpr (Event (EraRule "GOV" era))
  , ToExpr (Event (EraRule "SUBLEDGERS" era))
  ) =>
  ToExpr (DijkstraLedgerEvent era)

instance
  ( ToExpr (Value era)
  , ToExpr (TxOut era)
  , ToExpr (PredicateFailure (EraRule "UTXOS" era))
  ) =>
  ToExpr (DijkstraUtxoPredFailure era)

instance
  (EraPParams era, ToExpr (PParamsHKD StrictMaybe era)) =>
  ToExpr (DijkstraGovPredFailure era)

instance ToExpr (DijkstraGovCertPredFailure era)

instance
  ToExpr (PredicateFailure (EraRule "LEDGERS" era)) =>
  ToExpr (DijkstraBbodyPredFailure era)

instance
  ToExpr (PredicateFailure (EraRule "LEDGER" era)) =>
  ToExpr (DijkstraMempoolPredFailure era)

instance
  ToExpr (Event (EraRule "LEDGER" era)) =>
  ToExpr (DijkstraMempoolEvent era)

instance
  ( ToExpr (PredicateFailure (EraRule "SUBDELEG" era))
  , ToExpr (PredicateFailure (EraRule "SUBPOOL" era))
  , ToExpr (PredicateFailure (EraRule "SUBGOVCERT" era))
  ) =>
  ToExpr (DijkstraSubCertPredFailure era)

instance
  ToExpr (Event (EraRule "SUBPOOL" era)) =>
  ToExpr (DijkstraSubCertEvent era)

instance
  ToExpr (PredicateFailure (EraRule "SUBCERT" era)) =>
  ToExpr (DijkstraSubCertsPredFailure era)

instance
  ToExpr (Event (EraRule "SUBCERT" era)) =>
  ToExpr (DijkstraSubCertsEvent era)

instance ToExpr (DijkstraSubDelegPredFailure era)

instance
  ( ToExpr (PredicateFailure (EraRule "SUBGOV" era))
  , ToExpr (PredicateFailure (EraRule "SUBCERTS" era))
  , ToExpr (PredicateFailure (EraRule "SUBUTXOW" era))
  ) =>
  ToExpr (DijkstraSubLedgerPredFailure era)

instance
  ( ToExpr (Event (EraRule "SUBGOV" era))
  , ToExpr (Event (EraRule "SUBCERTS" era))
  , ToExpr (Event (EraRule "SUBUTXOW" era))
  ) =>
  ToExpr (DijkstraSubLedgerEvent era)

instance
  ToExpr (PredicateFailure (EraRule "SUBLEDGER" era)) =>
  ToExpr (DijkstraSubLedgersPredFailure era)

instance
  ToExpr (Event (EraRule "SUBLEDGER" era)) =>
  ToExpr (DijkstraSubLedgersEvent era)

instance
  ToExpr (DijkstraGovPredFailure era) =>
  ToExpr (DijkstraSubGovPredFailure era)

instance ToExpr (ConwayGovEvent era) => ToExpr (DijkstraSubGovEvent era)

instance ToExpr (DijkstraSubGovCertPredFailure era)

instance ToExpr (DijkstraSubPoolPredFailure era)

instance ToExpr (DijkstraSubPoolEvent era)

instance
  ( ToExpr (Value era)
  , ToExpr (TxOut era)
  , ToExpr (PredicateFailure (EraRule "SUBUTXOS" era))
  ) =>
  ToExpr (DijkstraSubUtxoPredFailure era)

instance
  ToExpr (Event (EraRule "SUBUTXOS" era)) =>
  ToExpr (DijkstraSubUtxoEvent era)

instance
  ToExpr (ConwayUtxosPredFailure era) =>
  ToExpr (DijkstraSubUtxosPredFailure era)

instance ToExpr (TxOut era) => ToExpr (DijkstraSubUtxosEvent era)

instance
  ( Era era
  , ToExpr (PredicateFailure (EraRule "SUBUTXO" era))
  , ToExpr (PlutusPurpose AsIx era)
  , ToExpr (PlutusPurpose AsItem era)
  , ToExpr (TxCert era)
  ) =>
  ToExpr (DijkstraSubUtxowPredFailure era)

instance
  ToExpr (Event (EraRule "SUBUTXO" era)) =>
  ToExpr (DijkstraSubUtxowEvent era)

instance ToExpr (AccountBalanceInterval era)

instance ToExpr (AccountBalanceIntervals era)
