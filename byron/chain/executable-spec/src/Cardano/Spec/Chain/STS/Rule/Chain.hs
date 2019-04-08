{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.Spec.Chain.STS.Rule.Chain where

import Control.Lens ((^.), _1, _3, _5, Lens')
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
        IRC (_, gks, pps) <- judgmentContext
        let
          s0 = Slot 0
          dsenv
            = DSEnv
            { _dSEnvAllowedDelegators = gks
            , _dSEnvEpoch = sEpoch s0 (pps ^. bkSlotsPerEpoch)
            , _dSEnvSlot = s0
            , _dSEnvStableAfter = pps ^. Ledger.Update.stableAfter
            }
        ds <- trans @DELEG $ IRC dsenv
        return $! ( Epoch 0
                  , s0
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
-- Chain environment lenses.
--------------------------------------------------------------------------------

-- | Lens for the protocol parameters contained in the environment.
--
ppsL :: Lens' (Environment CHAIN) PParams
ppsL = _3

--------------------------------------------------------------------------------
-- Chain state lenses.
--------------------------------------------------------------------------------

-- | Lens for the epoch contained in the chain state.
epochL :: Lens' (State CHAIN) Epoch
epochL = _1

-- | Lens for the delegation interface state contained in the chain state.
disL :: Lens' (State CHAIN) DIState
disL = _5

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

    mTxSz <- Gen.integral (Range.constant 0 4000000)
    mPSz <- Gen.integral (Range.constant 0 4000000)

    -- Chain stability. We set this to an integer between 1 and twice the value
    -- chosen for the Byron release.
    k <- Gen.integral (Range.constant 1 (2160 * 2))

    -- The percentage of the slots will typically be between 1/5 and 1/4,
    -- however we want to stretch that range a bit for testing purposes.
    t <- pure (1/5) -- Gen.double (Range.constant (1/6) (1/3))
    -- TODO: we make this a constant till we solve the problem with the number
    -- of byzantineNodes being a constant in the implementation.

    -- The number of slots per epoch is computed from 'k':
    -- slots per-epoch = k * 10
    spe <- pure $! SlotCount $ fromIntegral $ k * 10
    -- Update TTL
    uttl <- SlotCount <$> Gen.integral (Range.linear 1 100)
    -- Confirmation threshold
    ct <- Gen.integral (Range.linear 1 7)
    -- Update adoption threshold
    uat <- Gen.integral (Range.linear 1 7)
    let initPPs
          = PParams
          { _maxHdrSz = mHSz
          , _maxBkSz = mBSz
          , _maxTxSz = mTxSz
          , _maxPropSz = mPSz
          , _bkSgnCntT = t
          , _bkSlotsPerEpoch = spe
          , _upTtl = uttl
          , _scriptVersion = 1
          , _cfmThd = ct
          , _upAdptThd = uat
          , _stableAfter = BlockCount k
          }
    initGKeys <- Gen.set (Range.constant 1 30) vkgenesisGen
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
          , _dSEnvStableAfter = us ^. Ledger.Update.stableAfter }
    dCerts <- dcertsGen dsEnv
    let bh
          = MkBlockHeader
          { _bhPrevHash = h
          , _bhSlot = Slot (s + slotInc)
          , _bhIssuer = vkI
          , _bhSig = Sig vkI (owner vkI)
          }
        bb
          = BlockBody
          { _bDCerts = dCerts }
    return $ Block bh bb
