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

import Cardano.Base.Typeable (TypeName (TypeName))
import Cardano.Ledger.Allegra.Scripts (invalidBeforeL, invalidHereAfterL)
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Binary
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
import Control.Monad (forM_, unless, when)
import Data.Coerce (coerce)
import Data.Foldable (Foldable (..))
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import qualified Data.OMap.Strict as OMap
import qualified Data.OSet.Strict as OSet
import qualified Data.Sequence as Seq
import qualified Data.Sequence.Strict as StrictSeq
import Data.Typeable (Typeable)
import Data.Word (Word16)
import Lens.Micro
import Test.Cardano.Ledger.Conway.Binary.Annotator ()

deriving newtype instance Typeable l => DecCBOR (TxBody l DijkstraEra)

instance Typeable l => DecCBOR (DijkstraTxBodyRaw l DijkstraEra) where
  decCBOR = withSTxBothLevels @l $ \sTxLevel ->
    decodeSparseKeyed
      TypeName
      (requiredFields sTxLevel)
      (basicDijkstraTxBodyRaw sTxLevel)
      (decoderByKey sTxLevel)
    where
      decoderByKey ::
        STxBothLevels l DijkstraEra ->
        DijkstraTxBodyRaw l DijkstraEra ->
        Word ->
        Maybe (Decoder s (DijkstraTxBodyRaw l DijkstraEra))
      decoderByKey sTxLevel acc = \case
        0 -> Just $ do
          x <- decCBOR
          pure $ inputsDijkstraTxBodyRawL .~ x $ acc
        1 -> Just $ do
          x <- decCBOR
          pure $ outputsDijkstraTxBodyRawL .~ x $ acc
        2 | STopTx <- sTxLevel -> Just $ do
          x <- decCBOR
          pure $ feeDijkstraTxBodyRawL .~ x $ acc
        3 -> Just $ do
          x <- decCBOR
          pure $ vldtDijkstraTxBodyRawL . invalidHereAfterL .~ SJust x $ acc
        4 -> Just $ do
          x <- decCBOR
          when (OSet.null x) $ fail (emptyFailure "Certificates" "non-empty")
          pure $ certsDijkstraTxBodyRawL .~ x $ acc
        5 -> Just $ do
          x <- decCBOR
          when (null (unWithdrawals x)) $ fail (emptyFailure "Withdrawals" "non-empty")
          pure $ withdrawalsDijkstraTxBodyRawL .~ x $ acc
        7 -> Just $ do
          x <- decCBOR
          pure $ auxDataHashDijkstraTxBodyRawL .~ SJust x $ acc
        8 -> Just $ do
          x <- decCBOR
          pure $ vldtDijkstraTxBodyRawL . invalidBeforeL .~ SJust x $ acc
        9 -> Just $ do
          x <- decCBOR
          when (x == mempty) $ fail (emptyFailure "Mint" "non-empty")
          pure $ mintDijkstraTxBodyRawL .~ x $ acc
        11 -> Just $ do
          x <- decCBOR
          pure $ scriptIntegrityHashDijkstraTxBodyRawL .~ SJust x $ acc
        13 | STopTx <- sTxLevel -> Just $ do
          x <- decCBOR
          when (null x) $ fail (emptyFailure "Collateral Inputs" "non-empty")
          pure $ collateralInputsDijkstraTxBodyRawL .~ x $ acc
        14 -> Just $ do
          x <- decodeGuards
          pure $ guardsDijkstraTxBodyRawL .~ x $ acc
        15 -> Just $ do
          x <- decCBOR
          pure $ networkIdDijkstraTxBodyRawL .~ SJust x $ acc
        16 | STopTx <- sTxLevel -> Just $ do
          x <- decCBOR
          pure $ collateralReturnDijkstraTxBodyRawL .~ SJust x $ acc
        17 | STopTx <- sTxLevel -> Just $ do
          x <- decCBOR
          pure $ totalCollateralDijkstraTxBodyRawL .~ SJust x $ acc
        18 -> Just $ do
          x <- decCBOR
          when (null x) $ fail (emptyFailure "Reference Inputs" "non-empty")
          pure $ referenceInputsDijkstraTxBodyRawL .~ x $ acc
        19 -> Just $ do
          x <- decCBOR
          when (null (unVotingProcedures x)) $ fail (emptyFailure "VotingProcedures" "non-empty")
          pure $ votingProceduresDijkstraTxBodyRawL .~ x $ acc
        20 -> Just $ do
          x <- decCBOR
          when (OSet.null x) $ fail (emptyFailure "ProposalProcedures" "non-empty")
          pure $ proposalProceduresDijkstraTxBodyRawL .~ x $ acc
        21 -> Just $ do
          x <- decCBOR
          pure $ currentTreasuryValueDijkstraTxBodyRawL .~ SJust x $ acc
        22 -> Just $ do
          x <- decodePositiveCoin $ emptyFailure "Treasury Donation" "non-zero"
          pure $ treasuryDonationDijkstraTxBodyRawL .~ x $ acc
        23 | STopTx <- sTxLevel -> Just $ do
          allowTag setTag
          x <- decCBOR
          when (OMap.null x) $ fail (emptyFailure "Subtransactions" "non-empty")
          pure $ subTransactionsDijkstraTxBodyRawL .~ x $ acc
        24 -> Just $ do
          x <- decodeMap decCBOR (decodeNullStrictMaybe decCBOR)
          when (Map.null x) $ fail (emptyFailure "RequiredTopLevelGuards" "non-empty")
          pure $ requiredTopLevelGuardsDijkstraTxBodyRawL .~ x $ acc
        25 -> Just $ do
          x <- decCBOR
          when (null (unDirectDeposits x)) $ fail (emptyFailure "DirectDeposits" "non-empty")
          pure $ directDepositsDijkstraTxBodyRawL .~ x $ acc
        26 -> Just $ do
          x <- decCBOR
          when (null (unAccountBalanceIntervals x)) $
            fail (emptyFailure "AccountBalanceIntervals" "non-empty")
          pure $ accountBalanceIntervalsDijkstraTxBodyRawL .~ x $ acc
        _ -> Nothing
      {-# INLINE decoderByKey #-}

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
  decCBOR = decodeRecordSum "DijkstraNativeScriptRaw" $ \case
    0 -> do
      hash <- decCBOR
      pure (2, DijkstraRequireSignature hash)
    1 -> do
      xs <- decCBOR
      pure (2, DijkstraRequireAllOf xs)
    2 -> do
      xs <- decCBOR
      pure (2, DijkstraRequireAnyOf xs)
    3 -> do
      m <- decCBOR
      xs <- decCBOR
      pure (3, DijkstraRequireMOf m xs)
    4 -> do
      m <- decCBOR
      pure (2, DijkstraTimeStart m)
    5 -> do
      m <- decCBOR
      pure (2, DijkstraTimeExpire m)
    6 -> do
      cred <- decCBOR
      pure (2, DijkstraRequireGuard cred)
    n -> invalidKey n

instance Era era => DecCBOR (DijkstraNativeScript era) where
  decCBOR = MkDijkstraNativeScript <$> decodeMemoized decCBOR

instance Typeable l => DecCBOR (DijkstraTx l DijkstraEra) where
  decCBOR =
    withSTxBothLevels @l $ \case
      STopTx -> decodeDijkstraTopTx True
      SSubTx -> decodeRecordNamed "DijkstraSubTx" (const 3) $ do
        body <- decCBOR
        wits <- decCBOR
        aux <- decodeNullStrictMaybe decCBOR
        pure $ DijkstraSubTx body wits aux
  {-# INLINE decCBOR #-}

deriving newtype instance Typeable l => DecCBOR (Tx l DijkstraEra)

decodeDijkstraTopTx ::
  ( DecCBOR (TxBody TopTx era)
  , DecCBOR (TxWits era)
  , DecCBOR (TxAuxData era)
  ) =>
  Bool -> Decoder s (DijkstraTx TopTx era)
decodeDijkstraTopTx allowIsValid =
  fst <$> do
    let isValidBackwardsCompatibleLength isValidFlagSupplied = if isValidFlagSupplied then 4 else 3
    decodeRecordNamed "DijkstraTx" (isValidBackwardsCompatibleLength . snd) $ do
      body <- decCBOR
      wits <- decCBOR
      isValidFlagSupplied <-
        if allowIsValid
          then
            peekTokenType >>= \case
              TypeBool ->
                decCBOR >>= \case
                  True -> pure True
                  False -> fail "Value `false` not allowed for `isValid`"
              _ -> pure False
          else pure False
      aux <- decodeNullStrictMaybe decCBOR
      pure (DijkstraTx body wits (IsValid True) aux, isValidFlagSupplied)

instance DecCBOR (DijkstraBlockBodyRaw DijkstraEra) where
  decCBOR = decodeRecordNamed "DijkstraBlockBodyRaw" (const 4) $ do
    let
      decodeInvalidTxs =
        decodeNonEmptySetLikeEnforceNoDuplicates
          (IntSet.insert . fromIntegral @Word16 @Int)
          (\x -> (IntSet.size x, x))
          (decCBOR @Word16)
    invalidTxs :: IntSet <- fold <$> decodeNullMaybe decodeInvalidTxs
    txs <- decodeSeq (decodeDijkstraTopTx @DijkstraEra False)
    mbLeiosCert <- decodeNullStrictMaybe decCBOR
    mbPerasCert <- decodeNullStrictMaybe decCBOR

    let txsLength = Seq.length txs
        inRange x = 0 <= x && x <= txsLength - 1
    forM_ (IntSet.toList invalidTxs) $ \i ->
      unless (inRange i) . fail $
        "index is out of range: " <> show i
    let
      validityFlags = alignedValidFlags txsLength invalidTxs
      txsWithIsValid = Seq.zipWith (set isValidTxL) validityFlags (coerce <$> txs)
    pure $ DijkstraBlockBodyRaw (StrictSeq.forceToStrict txsWithIsValid) mbLeiosCert mbPerasCert

instance DecCBOR (DijkstraBlockBody DijkstraEra) where
  decCBOR = MkDijkstraBlockBody <$> decodeMemoized decCBOR
