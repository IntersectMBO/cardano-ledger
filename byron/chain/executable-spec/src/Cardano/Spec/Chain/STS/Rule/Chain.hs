{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.Spec.Chain.STS.Rule.Chain where

import Control.Lens ((^.), _1)
import qualified Crypto.Hash
import Data.Bimap (Bimap)
import Data.Bits (shift)
import Data.ByteString (ByteString)
import Data.Sequence (Seq)
-- import qualified Hedgehog.Gen as Gen
-- import qualified Hedgehog.Range as Range
import Numeric.Natural (Natural)

import Control.State.Transition
-- import Control.State.Transition.Generator
import Ledger.Core
-- import Ledger.Core.Generator
import Ledger.Delegation
import Ledger.Update
import Ledger.UTxO (UTxO, TxId)

import Cardano.Spec.Chain.STS.Block
import Cardano.Spec.Chain.STS.Rule.BHead
import Cardano.Spec.Chain.STS.Rule.BBody
import Cardano.Spec.Chain.STS.Rule.Epoch (sEpoch)
import Cardano.Spec.Chain.STS.Rule.Pbft


data CHAIN

instance STS CHAIN where
  type Environment CHAIN =
    ( Slot            -- Current slot
    , UTxO TxId
    )

  type State CHAIN =
    ( Slot
    , Seq VKeyGenesis
    , Hash
    , UTxO TxId
    , DIState
    , UPIState
    )

  type Signal CHAIN = Block

  data PredicateFailure CHAIN
    = BHeadFailure (PredicateFailure BHEAD)
    | BBodyFailure (PredicateFailure BBODY)
    | PBFTFailure (PredicateFailure PBFT)
    | MaximumBlockSize Natural Natural
    deriving (Eq, Show)

  initialRules = []

  transitionRules = [
    do
      TRC (_, _, b) <- judgmentContext
      case bIsEBB b of
        True  -> isEBBRule
        False -> notEBBRule
    ]
   where
    isEBBRule :: TransitionRule CHAIN
    isEBBRule = do
      TRC ((_sNow, _), (sLast, sgs, _, utxo, ds, us), b) <- judgmentContext
      bSize b <= (2 `shift` 21) ?! MaximumBlockSize (bSize b) (2 `shift` 21)
      let h' = bhHash (b ^. bHeader)
      return $! (sLast, sgs, h', utxo, ds, us)

    notEBBRule :: TransitionRule CHAIN
    notEBBRule = do
      TRC ((sNow, utxoGenesis), (sLast, sgs, h, utxo, ds, us), b) <- judgmentContext
      let dm = _dIStateDelegationMap ds :: Bimap VKeyGenesis VKey
      us' <-
        trans @BHEAD $ TRC ((dm, sLast), us, b ^. bHeader)
      let ppsUs' = snd (us' ^. _1)
      (h', sgs') <-
        trans @PBFT  $ TRC ((ppsUs', dm, sLast, sNow), (h, sgs), b ^. bHeader)
      (utxo', ds', us'') <- trans @BBODY $ TRC
        (
          ( ppsUs'
          , sEpoch (b ^. bHeader ^. bhSlot)
          , utxoGenesis
          )
        , (utxo, ds, us')
        , b
        )
      return $! (b ^. bHeader ^. bhSlot, sgs', h', utxo', ds', us'')


instance Embed BHEAD CHAIN where
  wrapFailed = BHeadFailure

instance Embed BBODY CHAIN where
  wrapFailed = BBodyFailure

instance Embed PBFT CHAIN where
  wrapFailed = PBFTFailure

genesisHash :: Hash
-- Not sure we need a concrete hash in the specs ...
genesisHash = Crypto.Hash.hash ("" :: ByteString)
