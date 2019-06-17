{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module Cardano.Spec.Chain.STS.Rule.Chain where

import Control.Lens (Lens', (&), (.~), (^.), _1, _5, to)
import qualified Data.Bimap as Bimap
import Data.Bimap (Bimap)
import Data.Bits (shift)
import Data.ByteString (ByteString)
import qualified Data.Hashable as H
import qualified Data.Map as Map
import Data.Sequence (Seq, fromList)
import qualified Data.Set as Set
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Numeric.Natural (Natural)

import Cardano.Ledger.Spec.STS.UTXOWS (UTXOWS)
import Cardano.Ledger.Spec.STS.UTXO (UTxOEnv(UTxOEnv, pps, utxo0), UTxOState)
import Control.State.Transition
import Control.State.Transition.Generator
import Ledger.Core
import Ledger.Delegation
import Ledger.Update
import qualified Ledger.Update.Generators as UpdateGen
import Ledger.UTxO (UTxO)

import Cardano.Spec.Chain.STS.Block
import Cardano.Spec.Chain.STS.Rule.BHead
import Cardano.Spec.Chain.STS.Rule.BBody
import Cardano.Spec.Chain.STS.Rule.Epoch (sEpoch)
import Cardano.Spec.Chain.STS.Rule.Pbft


data CHAIN

instance STS CHAIN where
  type Environment CHAIN =
    ( Slot            -- Current slot
    , UTxO
    -- Components needed to be able to bootstrap the traces generator. They are
    -- not part of the formal spec. These might be removed once we have decided
    -- on how do we want to model initial states.
    , DSEnv           -- Needed by the initial rules for delegation.
    , PParams         -- Needed to bootstrap this part of the chain state,
                      -- which is used in the delegation rules
    )

  type State CHAIN =
    ( Slot
    , Seq VKeyGenesis
    , Hash
    , UTxOState
    , DIState
    , UPIState
    )

  type Signal CHAIN = Block

  data PredicateFailure CHAIN
    = BHeadFailure (PredicateFailure BHEAD)
    | BBodyFailure (PredicateFailure BBODY)
    | PBFTFailure (PredicateFailure PBFT)
    | MaximumBlockSize Natural Natural
    | LedgerDelegationFailure (PredicateFailure DELEG)
    | LedgerUTxOFailure (PredicateFailure UTXOWS)
    deriving (Eq, Show)

  initialRules =
    [ do
        IRC (_slot, utxo0', dsenv, pps') <- judgmentContext
        let s0 = Slot 0
            -- Since we only test the delegation state we initialize the
            -- remaining fields of the update interface state to (m)empty. Once
            -- the formal spec includes initial rules for update we can use
            -- them here.
            upiState0 = ( (ProtVer 0 0 0, pps')
                        , []
                        , Map.empty
                        , Map.empty
                        , Map.empty
                        , Map.empty
                        , Set.empty
                        , Set.empty
                        , Map.empty
                        )
        utxoSt0 <- trans @UTXOWS $ IRC UTxOEnv {utxo0 = utxo0', pps = pps' }
        ds      <- trans @DELEG $ IRC dsenv
        return $! ( s0
                  , ds ^. delegationMap . to Bimap.keys . to fromList
                  , genesisHash
                  , utxoSt0
                  , ds
                  , upiState0
                  )
    ]

  transitionRules =
    [ do
      TRC (_, _, b) <- judgmentContext
      case bIsEBB b of
        True  -> isEBBRule
        False -> notEBBRule
    ]
   where
    isEBBRule :: TransitionRule CHAIN
    isEBBRule = do
      TRC ((_sNow, _, _, _), (sLast, sgs, _, utxo, ds, us), b) <- judgmentContext
      bSize b <= (2 `shift` 21) ?! MaximumBlockSize (bSize b) (2 `shift` 21)
      let h' = bhHash (b ^. bHeader)
      return $! (sLast, sgs, h', utxo, ds, us)

    notEBBRule :: TransitionRule CHAIN
    notEBBRule = do
      TRC ((sNow, utxoGenesis, _, pps), (sLast, sgs, h, utxoSt, ds, us), b) <- judgmentContext
      let dm = _dIStateDelegationMap ds :: Bimap VKeyGenesis VKey
      us' <-
        trans @BHEAD $ TRC ((dm, sLast, pps ^. stableAfter), us, b ^. bHeader)
      let ppsUs' = snd (us' ^. _1)
      (h', sgs') <-
        trans @PBFT  $ TRC ((ppsUs', dm, sLast, sNow), (h, sgs), b ^. bHeader)
      (utxoSt', ds', us'') <- trans @BBODY $ TRC
        (
          ( ppsUs'
          , sEpoch (b ^. bHeader ^. bhSlot)
          , utxoGenesis
          )
        , (utxoSt, ds, us')
        , b
        )
      return $! (b ^. bHeader ^. bhSlot, sgs', h', utxoSt', ds', us'')


instance Embed BHEAD CHAIN where
  wrapFailed = BHeadFailure

instance Embed BBODY CHAIN where
  wrapFailed = BBodyFailure

instance Embed PBFT CHAIN where
  wrapFailed = PBFTFailure

instance Embed DELEG CHAIN where
  wrapFailed = LedgerDelegationFailure

instance Embed UTXOWS CHAIN where
  wrapFailed = LedgerUTxOFailure

genesisHash :: Hash
-- Not sure we need a concrete hash in the specs ...
genesisHash = Hash $ H.hash ("" :: ByteString)

-- | Lens for the delegation interface state contained in the chain state.
disL :: Lens' (State CHAIN) DIState
disL = _5

instance HasTrace CHAIN where

  initEnvGen = (,,,)
            <$> gCurrentSlot
            <*> (utxo0 <$> initEnvGen @UTXOWS)
            <*> initEnvGen @DELEG
            <*> UpdateGen.pparamsGen
    where
      -- If we want to generate large traces, we need to set up the value of the
      -- current slot to a sufficiently large value.
      gCurrentSlot = Slot <$> Gen.integral (Range.constant 32768 2147483648)

  sigGen = sigGenChain GenDelegation GenUTxO

data ShouldGenDelegation = GenDelegation | NoGenDelegation

data ShouldGenUTxO = GenUTxO | NoGenUTxO

sigGenChain
  :: ShouldGenDelegation
  -> ShouldGenUTxO
  -> Environment CHAIN
  -> State CHAIN
  -> Gen (Signal CHAIN)
sigGenChain shouldGenDelegation shouldGenUTxO (_sNow, utxo0, dsEnv, pps) (Slot s, _sgs, h, utxo, ds, _us)
  = do
    -- Here we do not want to shrink the issuer, since @Gen.element@ shrinks
    -- towards the first element of the list, which in this case won't provide
    -- us with better shrinks.
    vkI         <- Gen.prune $ Gen.element $ Bimap.elems (ds ^. dmsL)
    nextSlot    <- gNextSlot

    delegationPayload <- case shouldGenDelegation of
      GenDelegation   -> dcertsGen dsEnv ds
      NoGenDelegation -> pure []

    utxoPayload <- case shouldGenUTxO of
      GenUTxO   -> sigGen @UTXOWS utxoEnv utxo
      NoGenUTxO -> pure []

    let
      dummySig       = Sig genesisHash (owner vkI)
      unsignedHeader = MkBlockHeader
        h
        nextSlot
        vkI
        dummySig -- Fill with a dummy signature first and then sign afterwards
        (hash utxoPayload)
        (hash delegationPayload)
        (hash (bb ^. bUpdProp, bb ^. bUpdVotes))

      signedHeader =
        unsignedHeader & bhSig .~ Sig (hashHeader unsignedHeader) (owner vkI)

      bb =
        BlockBody
          delegationPayload
          utxoPayload
          Nothing -- Update proposal
          []      -- Votes on update proposals
          (ProtVer 0 0 0)

    pure $ Block signedHeader bb
   where
    -- We'd expect the slot increment to be close to 1, even for large
    -- Gen's size numbers.
    gNextSlot = Slot . (s +) <$> Gen.integral (Range.exponential 1 10)

    utxoEnv   = UTxOEnv {utxo0, pps}
