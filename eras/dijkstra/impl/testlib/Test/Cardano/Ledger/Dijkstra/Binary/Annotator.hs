{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Dijkstra.Binary.Annotator (

) where

import Cardano.Ledger.Address (Withdrawals (..))
import Cardano.Ledger.Allegra.Scripts (invalidBeforeL, invalidHereAfterL)
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Binary
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.Coin (decodePositiveCoin)
import Cardano.Ledger.Conway.Governance (
  VotingProcedures (..),
 )
import Cardano.Ledger.Core
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Dijkstra.Scripts
import Cardano.Ledger.Dijkstra.Tx (DijkstraTx (..), Tx (..))
import Cardano.Ledger.Dijkstra.TxBody
import Cardano.Ledger.MemoBytes (decodeMemoized)
import Cardano.Ledger.Val (Val (..))
import qualified Data.Map.Strict as Map
import qualified Data.OMap.Strict as OMap
import qualified Data.OSet.Strict as OSet
import Data.Typeable (Typeable)
import Lens.Micro
import Test.Cardano.Ledger.Conway.Binary.Annotator ()

deriving newtype instance Typeable l => DecCBOR (TxBody l DijkstraEra)

instance Typeable l => DecCBOR (DijkstraTxBodyRaw l DijkstraEra) where
  decCBOR = withSTxBothLevels @l $ \sTxLevel ->
    decode $
      SparseKeyed
        "TxBodyRaw"
        (basicDijkstraTxBodyRaw sTxLevel)
        (bodyFields sTxLevel)
        (requiredFields sTxLevel)
    where
      bodyFields :: STxBothLevels l DijkstraEra -> Word -> Field (DijkstraTxBodyRaw l DijkstraEra)
      bodyFields sTxLevel = \case
        0 -> field (inputsDijkstraTxBodyRawL .~) From
        1 -> field (outputsDijkstraTxBodyRawL .~) From
        2 | STopTx <- sTxLevel -> field (feeDijkstraTxBodyRawL .~) From
        3 -> ofield (vldtDijkstraTxBodyRawL . invalidHereAfterL .~) From
        4 ->
          fieldGuarded
            (emptyFailure "Certificates" "non-empty")
            OSet.null
            (certsDijkstraTxBodyRawL .~)
            From
        5 ->
          fieldGuarded
            (emptyFailure "Withdrawals" "non-empty")
            (null . unWithdrawals)
            (withdrawalsDijkstraTxBodyRawL .~)
            From
        7 -> ofield (auxDataHashDijkstraTxBodyRawL .~) From
        8 -> ofield (vldtDijkstraTxBodyRawL . invalidBeforeL .~) From
        9 ->
          fieldGuarded
            (emptyFailure "Mint" "non-empty")
            (== mempty)
            (mintDijkstraTxBodyRawL .~)
            From
        11 -> ofield (scriptIntegrityHashDijkstraTxBodyRawL .~) From
        13
          | STopTx <- sTxLevel ->
              fieldGuarded
                (emptyFailure "Collateral Inputs" "non-empty")
                null
                (collateralInputsDijkstraTxBodyRawL .~)
                From
        14 ->
          ofield
            (\x -> guardsDijkstraTxBodyRawL .~ fromSMaybe mempty x)
            (D decodeGuards)
        15 -> ofield (networkIdDijkstraTxBodyRawL .~) From
        16
          | STopTx <- sTxLevel ->
              ofield (collateralReturnDijkstraTxBodyRawL .~) From
        17
          | STopTx <- sTxLevel ->
              ofield (totalCollateralDijkstraTxBodyRawL .~) From
        18 ->
          fieldGuarded
            (emptyFailure "Reference Inputs" "non-empty")
            null
            (referenceInputsDijkstraTxBodyRawL .~)
            From
        19 ->
          fieldGuarded
            (emptyFailure "VotingProcedures" "non-empty")
            (null . unVotingProcedures)
            (votingProceduresDijkstraTxBodyRawL .~)
            From
        20 ->
          fieldGuarded
            (emptyFailure "ProposalProcedures" "non-empty")
            OSet.null
            (proposalProceduresDijkstraTxBodyRawL .~)
            From
        21 -> ofield (currentTreasuryValueDijkstraTxBodyRawL .~) From
        22 ->
          ofield
            (\x -> treasuryDonationDijkstraTxBodyRawL .~ fromSMaybe zero x)
            (D (decodePositiveCoin $ emptyFailure "Treasury Donation" "non-zero"))
        23
          | STopTx <- sTxLevel ->
              fieldGuarded
                (emptyFailure "Subtransactions" "non-empty")
                OMap.null
                (subTransactionsDijkstraTxBodyRawL .~)
                (D $ allowTag setTag >> decCBOR)
        24
          | SSubTx <- sTxLevel ->
              fieldGuarded
                (emptyFailure "RequiredTopLevelGuards" "non-empty")
                Map.null
                (requiredTopLevelGuardsDijkstraTxBodyRawL .~)
                (D (decodeMap decCBOR (decodeNullStrictMaybe decCBOR)))
        n -> invalidField n
      requiredFields :: STxBothLevels l DijkstraEra -> [(Word, String)]
      requiredFields sTxLevel
        | STopTx <- sTxLevel =
            [ (0, "inputs")
            , (1, "outputs")
            , (2, "fee")
            ]
        | SSubTx <- sTxLevel =
            [ (0, "inputs")
            , (1, "outputs")
            ]

      emptyFailure fieldName requirement =
        "TxBody: '" <> fieldName <> "' must be " <> requirement <> " when supplied"

instance Era era => DecCBOR (DijkstraNativeScriptRaw era) where
  decCBOR = decode $ Summands "DijkstraNativeScriptRaw" $ \case
    0 -> SumD DijkstraRequireSignature <! From
    1 -> SumD DijkstraRequireAllOf <! From
    2 -> SumD DijkstraRequireAnyOf <! From
    3 -> SumD DijkstraRequireMOf <! From <! From
    4 -> SumD DijkstraTimeStart <! From
    5 -> SumD DijkstraTimeExpire <! From
    6 -> SumD DijkstraRequireGuard <! From
    n -> Invalid n

instance Era era => DecCBOR (DijkstraNativeScript era) where
  decCBOR = MkDijkstraNativeScript <$> decodeMemoized decCBOR

instance Typeable l => DecCBOR (DijkstraTx l DijkstraEra) where
  decCBOR =
    withSTxBothLevels @l $ \case
      STopTx ->
        decode $
          RecD DijkstraTx
            <! From
            <! From
            <! From
            <! D (decodeNullStrictMaybe decCBOR)
      SSubTx ->
        decode $
          RecD DijkstraSubTx
            <! From
            <! From
            <! D (decodeNullStrictMaybe decCBOR)
  {-# INLINE decCBOR #-}

deriving newtype instance Typeable l => DecCBOR (Tx l DijkstraEra)
