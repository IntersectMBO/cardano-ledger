{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Shelley.Spec.Ledger.STS.Bbody
  ( BBODY,
    BbodyState (..),
    BbodyEnv (..),
    PredicateFailure (..),
    State,
  )
where

import Byron.Spec.Ledger.Core ((∈))
import Cardano.Prelude (NoUnexpectedThunks (..))
import Control.State.Transition
import qualified Data.Sequence.Strict as StrictSeq
import Data.Set (Set)
import GHC.Generics (Generic)
import Shelley.Spec.Ledger.BaseTypes
import Shelley.Spec.Ledger.BlockChain
import Shelley.Spec.Ledger.Coin (Coin)
import Shelley.Spec.Ledger.Crypto
import Shelley.Spec.Ledger.EpochBoundary
import Shelley.Spec.Ledger.Keys
import Shelley.Spec.Ledger.LedgerState
import Shelley.Spec.Ledger.PParams
import Shelley.Spec.Ledger.STS.Ledgers
import Shelley.Spec.Ledger.Slot
import Shelley.Spec.Ledger.Tx

data BBODY crypto

data BbodyState crypto
  = BbodyState (LedgerState crypto) (BlocksMade crypto)
  deriving (Eq, Show)

data BbodyEnv = BbodyEnv
  { bbodySlots :: (Set SlotNo),
    bbodyPp :: PParams,
    bbodyReserves :: Coin
  }

instance
  ( Crypto crypto,
    DSignable crypto (Hash crypto (TxBody crypto))
  ) =>
  STS (BBODY crypto)
  where
  type
    State (BBODY crypto) =
      BbodyState crypto

  type
    Signal (BBODY crypto) =
      Block crypto

  type Environment (BBODY crypto) = BbodyEnv

  type BaseM (BBODY crypto) = ShelleyBase

  data PredicateFailure (BBODY crypto)
    = WrongBlockBodySizeBBODY Int Int
    | InvalidBodyHashBBODY (HashBBody crypto) (HashBBody crypto)
    | LedgersFailure (PredicateFailure (LEDGERS crypto))
    deriving (Show, Eq, Generic)

  initialRules = []
  transitionRules = [bbodyTransition]

instance (Crypto crypto) => NoUnexpectedThunks (PredicateFailure (BBODY crypto))

bbodyTransition ::
  forall crypto.
  ( Crypto crypto,
    DSignable crypto (Hash crypto (TxBody crypto))
  ) =>
  TransitionRule (BBODY crypto)
bbodyTransition =
  judgmentContext
    >>= \( TRC
             ( BbodyEnv oslots pp _reserves,
               BbodyState ls b,
               Block (BHeader bhb _) txsSeq
               )
           ) -> do
        let hk = hashKey $ bheaderVk bhb
            TxSeq txs = txsSeq
            actualBodySize = bBodySize txsSeq
            actualBodyHash = bbHash txsSeq

        actualBodySize == fromIntegral (hBbsize bhb)
          ?! WrongBlockBodySizeBBODY actualBodySize (fromIntegral $ hBbsize bhb)

        actualBodyHash == bhash bhb ?! InvalidBodyHashBBODY actualBodyHash (bhash bhb)

        ls' <-
          trans @(LEDGERS crypto) $
            TRC (LedgersEnv (bheaderSlotNo bhb) pp _reserves, ls, StrictSeq.getSeq txs)

        -- Note that this may not actually be a stake pool - it could be a genesis key
        -- delegate. However, this would only entail an overhead of 7 counts, and it's
        -- easier than differentiating here.
        let hkAsStakePool = coerceKeyRole hk
        pure $ BbodyState ls' (incrBlocks (bheaderSlotNo bhb ∈ oslots) hkAsStakePool b)

instance
  ( Crypto crypto,
    DSignable crypto (Hash crypto (TxBody crypto))
  ) =>
  Embed (LEDGERS crypto) (BBODY crypto)
  where
  wrapFailed = LedgersFailure
