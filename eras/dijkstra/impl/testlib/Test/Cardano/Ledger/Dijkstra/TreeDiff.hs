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

import Cardano.Ledger.BaseTypes (StrictMaybe)
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Dijkstra.Core (
  AlonzoEraScript (..),
  AsItem,
  AsIx,
  Era,
  EraPParams (..),
  EraTx (..),
  EraTxBody (..),
  EraTxCert (..),
  EraTxOut (..),
  PlutusScript,
 )
import Cardano.Ledger.Dijkstra.PParams (DijkstraPParams)
import Cardano.Ledger.Dijkstra.Scripts (
  DijkstraNativeScript,
  DijkstraNativeScriptRaw,
  DijkstraPlutusPurpose,
 )
import Cardano.Ledger.Dijkstra.Tx (DijkstraTx (..), Tx (..))
import Cardano.Ledger.Dijkstra.TxBody (DijkstraTxBodyRaw (..))
import Cardano.Ledger.Dijkstra.TxCert
import Cardano.Ledger.Dijkstra.TxInfo (DijkstraContextError)
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
    txBody@(DijkstraTxBodyRaw _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) ->
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
              ]
    txBody@(DijkstraSubTxBodyRaw _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) ->
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
              ]

instance ToExpr (TxBody l DijkstraEra)

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
