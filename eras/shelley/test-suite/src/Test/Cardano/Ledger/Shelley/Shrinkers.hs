{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Test.Cardano.Ledger.Shelley.Shrinkers where

import Cardano.Ledger.Block (Block)
import Cardano.Ledger.Coin
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Shelley.PParams
import Cardano.Ledger.Shelley.Scripts
import Cardano.Ledger.Shelley.Tx hiding (TxIn)
import Cardano.Ledger.Shelley.TxBody
import Cardano.Ledger.Slot
import Cardano.Ledger.TxIn (TxIn)
import Cardano.Ledger.Val ((<+>), (<->))
import qualified Cardano.Ledger.Val as Val
import Data.Foldable (toList)
import Data.List (foldl')
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Sequence.Strict (StrictSeq (..), empty, (<|))
import qualified Data.Sequence.Strict as StrictSeq
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace (traceShowId)
import Test.Cardano.Ledger.Shelley.Utils (ShelleyTest)
import Test.QuickCheck (shrinkIntegral, shrinkList)

shrinkBlock ::
  Block h era ->
  [Block h era]
shrinkBlock _ = []

shrinkTx ::
  forall era.
  (ShelleyTest era, Core.TxBody era ~ ShelleyTxBody era) =>
  ShelleyTx era ->
  [ShelleyTx era]
shrinkTx (ShelleyTx _b _ws _md) =
  [ShelleyTx b' _ws _md | b' <- shrinkTxBody _b]

-- NOTE ====
--
-- This is a shrinker for the Shelley TxBody, but has code in it to deal with
-- Values, which will only ever exist in the ShelleyMA TxBody (which is
-- different, and will have its own shrinker).
--
-- This is fine for now, but a better resolution would be: when we have a
-- shrinker for the ShelleyMA TxBody, let's use this logic to allow shrinking
-- the values, and revert this function to its prior incarnation, fixed to Coin
-- ======
shrinkTxBody ::
  forall era.
  ShelleyTest era =>
  ShelleyTxBody era ->
  [ShelleyTxBody era]
-- do not shrink body in case of empty output list
-- this will have to change in case any other part of TxBody will be shrunk
shrinkTxBody (ShelleyTxBody _ Empty _ _ _ _ _ _) = []
-- need to keep all the MA tokens in the transaction, so we need at least one
-- output to remain after the shrinking to preserve invariant
shrinkTxBody (ShelleyTxBody is os@((:<|) (ShelleyTxOut a vs) _) cs ws tf tl tu md) =
  -- shrinking inputs is probably not very beneficial
  -- [ TxBody is' os cs ws tf tl tu | is' <- shrinkSet shrinkTxIn is ] ++

  -- Shrink outputs, add the differing balance of the original and new outputs
  -- to the fees in order to preserve the invariant
  [ ShelleyTxBody is (mvExtraTksnToOut1 os') cs ws (tf <+> extraCoin os') tl tu md
    | os' <- toList $ shrinkStrictSeq shrinkTxOut os
  ]
  where
    -- [ TxBody is os cs' ws tf tl tu | cs' <- shrinkSeq shrinkDCert cs ] ++
    -- [ TxBody is os cs ws' tf tl tu | ws' <- shrinkWdrl ws ] ++
    -- [ TxBody is os cs ws tf' tl tu | tf' <- shrinkCoin tf ] ++
    -- [ TxBody is os cs ws tf tl' tu | tl' <- shrinkSlotNo tl ] ++
    -- [ TxBody is os cs ws tf tl tu' | tu' <- shrinkUpdate tu ]
    outBalance = outputBalance os
    extraTokens sr = outBalance <-> outputBalance sr
    extraCoin sr = traceShowId . Val.coin $ extraTokens sr
    -- put all the non-ada tokens in the head of the outputs, append shrunk list
    mvExtraTksnToOut1 Empty = empty
    mvExtraTksnToOut1 sr =
      ShelleyTxOut a (vs <+> extraTokens sr <-> Val.inject (extraCoin sr)) <| sr

outputBalance :: ShelleyTest era => StrictSeq (ShelleyTxOut era) -> Core.Value era
outputBalance = foldl' (\v (ShelleyTxOut _ c) -> v <+> c) mempty

shrinkTxIn :: TxIn c -> [TxIn c]
shrinkTxIn = const []

shrinkTxOut :: ShelleyTest era => ShelleyTxOut era -> [ShelleyTxOut era]
shrinkTxOut (ShelleyTxOut addr vl) =
  ShelleyTxOut addr <$> shrinkVal vl
  where
    -- we do not shrink value for now
    shrinkVal vl' = [vl']

shrinkCoin :: Coin -> [Coin]
shrinkCoin (Coin x) = Coin <$> shrinkIntegral x

shrinkDCert :: DCert c -> [DCert c]
shrinkDCert = const []

shrinkWdrl :: Wdrl c -> [Wdrl c]
shrinkWdrl (Wdrl m) = Wdrl <$> shrinkMap shrinkRewardAcnt shrinkCoin m

shrinkRewardAcnt :: RewardAcnt c -> [RewardAcnt c]
shrinkRewardAcnt = const []

shrinkSlotNo :: SlotNo -> [SlotNo]
shrinkSlotNo (SlotNo x) = SlotNo <$> shrinkIntegral x

shrinkUpdate :: Update era -> [Update era]
shrinkUpdate = const []

shrinkWitVKey :: WitVKey kr c -> [WitVKey kr c]
shrinkWitVKey = const []

shrinkScriptHash :: ScriptHash c -> [ScriptHash c]
shrinkScriptHash = const []

shrinkMultiSig :: MultiSig c -> [MultiSig c]
shrinkMultiSig = const []

shrinkSet :: Ord a => (a -> [a]) -> Set a -> [Set a]
shrinkSet f = (Set.fromList <$>) . shrinkList f . toList

-- TODO can this be made more efficient?
shrinkSeq :: (a -> [a]) -> Seq a -> [Seq a]
shrinkSeq f = (Seq.fromList <$>) . shrinkList f . toList

-- TODO can this be made more efficient?
shrinkStrictSeq :: (a -> [a]) -> StrictSeq a -> [StrictSeq a]
shrinkStrictSeq f = (StrictSeq.fromList <$>) . shrinkList f . toList

shrinkMap ::
  Ord k =>
  (k -> [k]) ->
  (v -> [v]) ->
  Map k v ->
  [Map k v]
shrinkMap shrinkK shrinkV =
  (Map.fromList <$>) . shrinkList shrinkPair . Map.toList
  where
    shrinkPair (x, y) =
      [(x', y) | x' <- shrinkK x]
        ++ [(x, y') | y' <- shrinkV y]
