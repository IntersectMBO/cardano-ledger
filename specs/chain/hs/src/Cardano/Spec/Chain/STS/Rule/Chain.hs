{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.Spec.Chain.STS.Rule.Chain where

import Control.Lens ((^.), _1, _3, _5, Getting)
import qualified Crypto.Hash
import Data.ByteString (ByteString)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import Data.Sequence (Seq)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Control.State.Transition
import Control.State.Transition.Generator
import Ledger.Core
import Ledger.Core.Generator
import Ledger.Delegation
import Ledger.Signatures
import Ledger.Update

import Cardano.Spec.Chain.STS.Block
import Cardano.Spec.Chain.STS.Rule.BHead
import Cardano.Spec.Chain.STS.Rule.BBody

data CHAIN

instance STS CHAIN where
  type Environment CHAIN =
    ( Slot            -- Current slot
    , Set VKeyGenesis -- Genesis keys
    , PParams         -- Initial protocol parameters
    )
    -- TODO: it can be confusing to have this in the environment. It will be
    -- used by the initial rule only, and then we'll have to drag it for
    -- eternity. The state contains the up to date protocol parameters.

  type State CHAIN =
    ( Epoch
    , Slot
    , Hash
    , Seq VKeyGenesis
    , DIState
    , PParams
    )

  type Signal CHAIN = Block

  data PredicateFailure CHAIN
    = BHeadFailure (PredicateFailure BHEAD)
    | BBodyFailure (PredicateFailure BBODY)
    | LedgerFailure (PredicateFailure DELEG)
    deriving (Eq, Show)

  initialRules =
    [ do
        IRC (s, gks, pps) <- judgmentContext
        let dsenv
              = DSEnv
              { _dSEnvAllowedDelegators = gks
              , _dSEnvEpoch = sEpoch s
              , _dSEnvSlot = s
              , _dSEnvLiveness = pps ^. dLiveness
              }
        ds <- trans @DELEG $ IRC dsenv
        return $! ( Epoch 0
                  , Slot 0
                  , genesisHash
                  , []
                  , ds
                  , pps
                  )
    ]

  transitionRules =
    [ do
        TRC
          ( (sNow, gks, _)
          , (eLast, sLast, hLast, sgs, ds, us)
          , b ) <- judgmentContext

        (eNext, sNext, h', sgs', us') <- trans @BHEAD $ TRC ( (sNow, ds ^. dms)
                                                            , (eLast, sLast, hLast, sgs, us)
                                                            , b ^. bHeader )

        ds' <- trans @BBODY $ TRC ( (eNext, sNext, us, gks)
                                  , ds
                                  , b )

        return $! (eNext, sNext, h', sgs', ds', us')
    ]

instance Embed DELEG CHAIN where
  wrapFailed = LedgerFailure

instance Embed BHEAD CHAIN where
  wrapFailed = BHeadFailure

instance Embed BBODY CHAIN where
  wrapFailed = BBodyFailure

genesisHash :: Hash
-- Not sure we need a concrete hash in the specs ...
genesisHash = Crypto.Hash.hash ("" :: ByteString)

--------------------------------------------------------------------------------
-- Chain environment getters.
--------------------------------------------------------------------------------

-- | Getter for the protocol parameters contained in the environment.
--
-- We want to use the getter with lens operations such as '(^.)', however we do
-- not want the getter to be able to modify the environment using lens
-- operators such as `.~`.
--
-- The type of '(^.)', which is just `view` with the arguments flipped, is:
--
-- (^.) :: s -> Getting a s a -> a
--
-- Hence the type we gave to 'getPps'.
--
-- We could have used:
--
-- > Getter (Environment CHAIN) PParams
--
-- which is equivalent to:
--
-- > forall f . (Contravariant f, Functor f)
--   => (PParams -> f PParams) -> (Environment CHAIN) -> f (Environment CHAIN)
--
-- However @Contravariant f@ is a redundant constraint, and GHC will give a warning.
--
-- The same remark applies to the other getters defined in this module.
--
getPps :: Getting PParams (Environment CHAIN) PParams
getPps = _3

--------------------------------------------------------------------------------
-- Chain state getters.
--------------------------------------------------------------------------------

-- | Getter for the epoch contained in the chain state.
getEpoch :: Getting Epoch (State CHAIN) Epoch
getEpoch = _1

-- | Getter for the delegation interface state contained in the chain state.
getDis :: Getting DIState (State CHAIN) DIState
getDis = _5

--------------------------------------------------------------------------------
-- Generators
--------------------------------------------------------------------------------

instance HasTrace CHAIN where
  initEnvGen =
    do
    -- In mainet the maximum header size is set to 2000000 and the maximum
    -- block size is also set to 2000000, so we have to make sure we cover
    -- those values here. The upper bound is arbitrary though.
    mHSz <- Gen.integral (Range.constant 0 4000000)
    mBSz <- Gen.integral (Range.constant 0 4000000)
    -- The delegation liveness parameter is arbitrarily determined.
    d <- SlotCount <$> Gen.integral (Range.linear 0 10)
    -- The size of the rolling widow is arbitrarily determined.
    w <- Gen.integral (Range.linear 0 10)
    -- The percentage of the slots will typically be between 1/5 and 1/4,
    -- however we want to stretch that range a bit for testing purposes.
    t <- Gen.double (Range.constant (1/6) (1/3))
    -- The slots per-epoch is arbitrarily determined.
    spe <- SlotCount <$> Gen.integral (Range.linear 1 1000)
    let initPPs
          = PParams
          { _maxHdrSz = mHSz
          , _maxBkSz = mBSz
          , _dLiveness = d
          , _bkSgnCntW = w
          , _bkSgnCntT = t
          , _bkSlotsPerEpoch = spe
          }
    initGKeys <- Gen.set (Range.constant 1 70) vkgenesisGen
    -- If we want to generate large traces, we need to set up the value of the
    -- "clock-slot" to a sufficiently large value.
    clockSlot <- Slot <$>
      Gen.integral (Range.constant 32768 2147483648)
    return (clockSlot, initGKeys, initPPs)

  sigGen (_, gks, _) (e, Slot s, h, _sgs, ds, us) = do
    -- We'd expect the slot increment to be close to 1, even for large Gen's
    -- size numbers.
    slotInc <- Gen.integral (Range.exponential 0 10)
    -- Get some random issuer from the delegates of the delegation map.
    vkI <- Gen.element $ Map.elems (ds ^. dms)
    let dsEnv
          = DSEnv
          { _dSEnvAllowedDelegators = gks
          , _dSEnvEpoch = e
          , _dSEnvSlot = Slot s
          , _dSEnvLiveness = us ^. dLiveness }
    dCerts <- dcertsGen dsEnv
    let bh
          = BlockHeader
          { _prevHHash = h
          , _bSlot = Slot (s + slotInc)
          , _bIssuer = vkI
          , _bSig = Sig vkI (owner vkI)
          }
        bb
          = BlockBody
          { _bDCerts = dCerts }
    return $ Block bh bb
