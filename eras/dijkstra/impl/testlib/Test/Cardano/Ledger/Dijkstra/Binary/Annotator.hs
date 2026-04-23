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

import Cardano.Ledger.Allegra.Scripts (invalidBeforeL, invalidHereAfterL)
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Binary
import Cardano.Ledger.Binary.Coders
import Cardano.Ledger.Coin (decodePositiveCoin)
import Cardano.Ledger.Conway.Governance (VotingProcedures (..))
import Cardano.Ledger.Dijkstra (DijkstraEra)
import Cardano.Ledger.Dijkstra.BlockBody.Internal (
  DijkstraBlockBody (..),
  DijkstraBlockBodyRaw (..),
  alignedValidFlags,
 )
import Cardano.Ledger.Dijkstra.Core
import Cardano.Ledger.Dijkstra.Scripts
import Cardano.Ledger.Dijkstra.Tx (DijkstraTx (..), Tx (..))
import Cardano.Ledger.Dijkstra.TxBody
import Cardano.Ledger.MemoBytes (decodeMemoized)
import Cardano.Ledger.Val (Val (..))
import Control.Monad (unless)
import qualified Data.Map.Strict as Map
import qualified Data.OMap.Strict as OMap
import qualified Data.OSet.Strict as OSet
import qualified Data.Sequence as Seq
import qualified Data.Sequence.Strict as StrictSeq
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
        25 ->
          fieldGuarded
            (emptyFailure "DirectDeposits" "non-empty")
            (null . unDirectDeposits)
            (directDepositsDijkstraTxBodyRawL .~)
            From
        26 ->
          fieldGuarded
            (emptyFailure "AccountBalanceIntervals" "non-empty")
            (null . unAccountBalanceIntervals)
            (accountBalanceIntervalsDijkstraTxBodyRawL .~)
            From
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
        decodeListLen >>= \case
          4 -> do
            body <- decCBOR
            wits <- decCBOR
            isValid <-
              decCBOR
                >>= \case
                  True -> pure (IsValid True)
                  False -> fail "value `false` not allowed for `isValid`"
            aux <- decodeNullStrictMaybe decCBOR
            pure $ DijkstraTx body wits isValid aux
          3 -> do
            DijkstraTx
              <$> decCBOR
              <*> decCBOR
              <*> pure (IsValid True)
              <*> decodeNullStrictMaybe decCBOR
          n -> fail $ "Unexpected list length: " <> show n <> ". Expected: 4 or 3."
      SSubTx ->
        decode $
          RecD DijkstraSubTx
            <! From
            <! From
            <! D (decodeNullStrictMaybe decCBOR)
  {-# INLINE decCBOR #-}

deriving newtype instance Typeable l => DecCBOR (Tx l DijkstraEra)

instance DecCBOR (DijkstraBlockBodyRaw DijkstraEra) where
  decCBOR = do
    mLen <- decodeListLenOrIndef
    case mLen of
      Just len | len /= 3 -> fail "expected 3 elements"
      _ -> pure ()
    invalidTxs <- decCBOR
    txs <- decCBOR
    perasCert <- decodeNullStrictMaybe decCBOR
    case mLen of
      Nothing -> do
        isBreak <- decodeBreakOr
        unless isBreak $ fail "expected break"
      _ -> pure ()
    let txsLength = Seq.length txs
        inRange x = 0 <= x && x <= txsLength - 1
    unless (all inRange invalidTxs) $
      fail $
        "Some IsValid index is not in the range: 0 .. "
          ++ show (txsLength - 1)
          ++ ", "
          ++ show invalidTxs
    let
      setValidityFlag tx isValid = tx & isValidTxL .~ isValid
      validityFlags = alignedValidFlags txsLength invalidTxs
      txsWithIsValid = Seq.zipWith setValidityFlag txs validityFlags
    pure $ DijkstraBlockBodyRaw (StrictSeq.forceToStrict txsWithIsValid) perasCert

instance DecCBOR (DijkstraBlockBody DijkstraEra) where
  decCBOR = MkDijkstraBlockBody <$> decodeMemoized decCBOR
