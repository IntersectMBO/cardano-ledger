{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Test.Shelley.Spec.Ledger.Shrinkers where

import Cardano.Ledger.Val ((<+>), (<->))
import Data.Foldable (toList)
import Data.List (foldl')
import Data.Map (Map)
import qualified Data.Map as M
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as StrictSeq
import Data.Set (Set)
import qualified Data.Set as S
import Shelley.Spec.Ledger.BlockChain
import Shelley.Spec.Ledger.Coin
import Shelley.Spec.Ledger.PParams
import Shelley.Spec.Ledger.Scripts
import Shelley.Spec.Ledger.Slot
import Shelley.Spec.Ledger.Tx
import Shelley.Spec.Ledger.TxBody
import Test.QuickCheck (shrinkIntegral, shrinkList)
import Test.Shelley.Spec.Ledger.Utils (ShelleyTest)

shrinkBlock ::
  Block h ->
  [Block h]
shrinkBlock _ = []

shrinkTx ::
  forall era.
  ShelleyTest era =>
  Tx era ->
  [Tx era]
shrinkTx (Tx _b _ws _md) =
  [Tx b' _ws _md | b' <- shrinkTxBody _b]

shrinkTxBody ::
  ShelleyTest era =>
  TxBody era ->
  [TxBody era]
shrinkTxBody (TxBody is os cs ws tf tl tu md) =
  -- shrinking inputs is probably not very beneficial
  -- [ TxBody is' os cs ws tf tl tu | is' <- shrinkSet shrinkTxIn is ] ++

  -- Shrink outputs, add the differing balance of the original and new outputs
  -- to the fees in order to preserve the invariant
  [ TxBody is os' cs ws (tf <+> (outBalance <-> outputBalance os')) tl tu md
    | os' <- toList $ shrinkStrictSeq shrinkTxOut os
  ]
  where
    -- [ TxBody is os cs' ws tf tl tu | cs' <- shrinkSeq shrinkDCert cs ] ++
    -- [ TxBody is os cs ws' tf tl tu | ws' <- shrinkWdrl ws ] ++
    -- [ TxBody is os cs ws tf' tl tu | tf' <- shrinkCoin tf ] ++
    -- [ TxBody is os cs ws tf tl' tu | tl' <- shrinkSlotNo tl ] ++
    -- [ TxBody is os cs ws tf tl tu' | tu' <- shrinkUpdate tu ]
    outBalance = outputBalance os

outputBalance :: ShelleyTest era => StrictSeq (TxOut era) -> Coin
outputBalance = foldl' (\v (TxOut _ c) -> v <+> c) (Coin 0)

shrinkTxIn :: TxIn era -> [TxIn era]
shrinkTxIn = const []

shrinkTxOut :: ShelleyTest era => TxOut era -> [TxOut era]
shrinkTxOut (TxOut addr coin) =
  TxOut addr <$> shrinkCoin coin

shrinkCoin :: Coin -> [Coin]
shrinkCoin (Coin x) = Coin <$> shrinkIntegral x

shrinkDCert :: DCert era -> [DCert era]
shrinkDCert = const []

shrinkWdrl :: Wdrl era -> [Wdrl era]
shrinkWdrl (Wdrl m) = Wdrl <$> shrinkMap shrinkRewardAcnt shrinkCoin m

shrinkRewardAcnt :: RewardAcnt era -> [RewardAcnt era]
shrinkRewardAcnt = const []

shrinkSlotNo :: SlotNo -> [SlotNo]
shrinkSlotNo (SlotNo x) = SlotNo <$> shrinkIntegral x

shrinkUpdate :: Update era -> [Update era]
shrinkUpdate = const []

shrinkWitVKey :: WitVKey kr era -> [WitVKey kr era]
shrinkWitVKey = const []

shrinkScriptHash :: ScriptHash era -> [ScriptHash era]
shrinkScriptHash = const []

shrinkMultiSig :: MultiSig era -> [MultiSig era]
shrinkMultiSig = const []

shrinkSet :: Ord a => (a -> [a]) -> Set a -> [Set a]
shrinkSet f = (S.fromList <$>) . shrinkList f . toList

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
  (M.fromList <$>) . shrinkList shrinkPair . M.toList
  where
    shrinkPair (x, y) =
      [(x', y) | x' <- shrinkK x]
        ++ [(x, y') | y' <- shrinkV y]
