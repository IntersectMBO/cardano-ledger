{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module STS.Bbody
  ( BBODY
  , BbodyState (..)
  , BbodyEnv (..)
  , PredicateFailure (..)
  , State
  )
where

import           BaseTypes
import           BlockChain
import           Cardano.Ledger.Shelley.Crypto
import           Cardano.Prelude (NoUnexpectedThunks (..))
import           Coin (Coin)
import           Control.State.Transition
import           Data.Set (Set)
import           EpochBoundary
import           GHC.Generics (Generic)
import           Keys
import           Ledger.Core ((∈))
import           LedgerState
import           PParams
import           Slot
import           STS.Ledgers
import           Tx
import           Scripts

data BBODY crypto

data BbodyState crypto
  = BbodyState (LedgerState crypto) (BlocksMade crypto)
  deriving (Eq, Show)

data BbodyEnv crypto
  = BbodyEnv
    { bbodySlots    :: (Set SlotNo)
    , bbodyPp       :: PParams crypto
    , bbodyReserves :: Value crypto
    }

instance
  ( Crypto crypto
  , Signable (DSIGN crypto) (TxBody crypto)
  )
  => STS (BBODY crypto)
 where
  type State (BBODY crypto)
    = BbodyState crypto

  type Signal (BBODY crypto)
    = Block crypto

  type Environment (BBODY crypto) = BbodyEnv

  type BaseM (BBODY crypto) = ShelleyBase

  data PredicateFailure (BBODY crypto)
    = WrongBlockBodySizeBBODY
    | InvalidBodyHashBBODY
    | LedgersFailure (PredicateFailure (LEDGERS crypto))
    deriving (Show, Eq, Generic)

  initialRules = []
  transitionRules = [bbodyTransition]

instance NoUnexpectedThunks (PredicateFailure (BBODY crypto))

bbodyTransition
  :: forall crypto
   . ( Crypto crypto
     , Signable (DSIGN crypto) (TxBody crypto)
     )
  => TransitionRule (BBODY crypto)
bbodyTransition = do
  TRC ( BbodyEnv oslots pp _reserves
      , BbodyState ls b
      , Block (BHeader bhb _) txsSeq@(TxSeq txs)) <- judgmentContext
  let hk = hashKey $ bvkcold bhb

  bBodySize txsSeq == fromIntegral (hBbsize bhb) ?! WrongBlockBodySizeBBODY

  bbHash txsSeq == bhash bhb ?! InvalidBodyHashBBODY

  ls' <- trans @(LEDGERS crypto)
         $ TRC (LedgersEnv (bheaderSlotNo bhb) pp _reserves, ls, txs)

  pure $ BbodyState ls' (incrBlocks (bheaderSlotNo bhb ∈ oslots) hk b)

instance
  ( Crypto crypto
  , Signable (DSIGN crypto) (TxBody crypto)
  )
  => Embed (LEDGERS crypto) (BBODY crypto)
 where
  wrapFailed = LedgersFailure
