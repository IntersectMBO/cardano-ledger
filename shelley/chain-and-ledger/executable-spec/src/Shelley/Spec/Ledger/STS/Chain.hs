{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Shelley.Spec.Ledger.STS.Chain
  ( CHAIN
  , ChainState (..)
  , PredicateFailure(..)
  , initialShelleyState
  , totalAda
  )
where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Numeric.Natural (Natural)

import           Cardano.Prelude (asks)
import           Cardano.Slotting.Slot (WithOrigin (..))
import           Shelley.Spec.Ledger.BaseTypes (Globals (..), Nonce (..), Seed (..), ShelleyBase)
import           Shelley.Spec.Ledger.BlockChain (BHBody, Block (..), LastAppliedBlock (..),
                     bHeaderSize, bhbody, bheaderSlotNo, hBbsize)
import           Shelley.Spec.Ledger.Coin (Coin (..))
import           Shelley.Spec.Ledger.Delegation.Certificates (PoolDistr (..))
import           Shelley.Spec.Ledger.EpochBoundary (BlocksMade (..), emptySnapShots)
import           Shelley.Spec.Ledger.Keys (GenDelegs (..), GenKeyHash, KESignable, KeyHash,
                     Signable, VKeyES)
import           Shelley.Spec.Ledger.LedgerState (AccountState (..), DPState (..), DState (..),
                     EpochState (..), LedgerState (..), NewEpochState (..), PState (..),
                     UTxOState (..), emptyDState, emptyPState, getGKeys, updateNES, _genDelegs)
import           Shelley.Spec.Ledger.OCert (KESPeriod)
import           Shelley.Spec.Ledger.PParams (PParams, ProtVer (..), _maxBBSize, _maxBHSize,
                     _protocolVersion)
import           Shelley.Spec.Ledger.Rewards (emptyNonMyopic)
import           Shelley.Spec.Ledger.Slot (EpochNo, SlotNo)
import           Shelley.Spec.Ledger.Tx (TxBody)
import           Shelley.Spec.Ledger.Updates (AVUpdate (..), Applications, PPUpdate (..),
                     UpdateState (..))
import           Shelley.Spec.Ledger.UTxO (UTxO (..), balance)

import qualified Cardano.Crypto.VRF as VRF
import           Cardano.Ledger.Shelley.Crypto
import           Control.State.Transition

import           Shelley.Spec.Ledger.STS.Bbody
import           Shelley.Spec.Ledger.STS.Prtcl
import           Shelley.Spec.Ledger.STS.Tick

data CHAIN crypto

data ChainState crypto
  = ChainState
    { chainNes              :: NewEpochState crypto
    , chainOCertIssue       :: Map.Map (KeyHash crypto) Natural
    , chainEpochNonce       :: Nonce
    , chainEvolvingNonce    :: Nonce
    , chainCandidateNonce   :: Nonce
    , chainPrevEpochNonce   :: Nonce
    , chainLastAppliedBlock :: WithOrigin (LastAppliedBlock crypto)
    }
  deriving (Show, Eq)

-- |Creates a valid initial chain state
initialShelleyState
  :: WithOrigin (LastAppliedBlock crypto)
  -> EpochNo
  -> UTxO crypto
  -> Coin
  -> Map (GenKeyHash crypto) (KeyHash crypto)
  -> Map SlotNo (Maybe (GenKeyHash crypto))
  -> Applications crypto
  -> PParams
  -> Nonce
  -> ChainState crypto
initialShelleyState lab e utxo reserves genDelegs os apps pp initNonce =
  ChainState
    (NewEpochState
       e
       (BlocksMade Map.empty)
       (BlocksMade Map.empty)
       (EpochState
         (AccountState (Coin 0) reserves)
         emptySnapShots
         (LedgerState
           (UTxOState
             utxo
             (Coin 0)
             (Coin 0)
             (UpdateState (PPUpdate Map.empty) (AVUpdate Map.empty) Map.empty apps)
           )
           (DPState (emptyDState {_genDelegs = (GenDelegs genDelegs)}) emptyPState)
         )
         pp
         emptyNonMyopic
       )
       Nothing
       (PoolDistr Map.empty)
       os
    )
    cs
    initNonce
    initNonce
    initNonce
    NeutralNonce
    lab
  where
    cs = Map.fromList (fmap (\hk -> (hk,0)) (Map.elems genDelegs))

instance
  ( Crypto crypto
  , Signable (DSIGN crypto) (VKeyES crypto, Natural, KESPeriod)
  , Signable (DSIGN crypto) (TxBody crypto)
  , KESignable crypto (BHBody crypto)
  , VRF.Signable (VRF crypto) Seed
  )
  => STS (CHAIN crypto)
 where
  type State (CHAIN crypto)
    = ChainState crypto

  type Signal (CHAIN crypto)
    = Block crypto

  type Environment (CHAIN crypto) = SlotNo
  type BaseM (CHAIN crypto) = ShelleyBase

  data PredicateFailure (CHAIN crypto)
    = HeaderSizeTooLargeCHAIN
    | BlockSizeTooLargeCHAIN
    | ObsoleteNodeCHAIN Natural Natural
    | BbodyFailure (PredicateFailure (BBODY crypto))
    | TickFailure (PredicateFailure (TICK crypto))
    | PrtclFailure (PredicateFailure (PRTCL crypto))
    deriving (Show, Eq)

  initialRules = []
  transitionRules = [chainTransition]

chainTransition
  :: forall crypto
   . ( Crypto crypto
     , Signable (DSIGN crypto) (VKeyES crypto, Natural, KESPeriod)
     , Signable (DSIGN crypto) (TxBody crypto)
     , KESignable crypto (BHBody crypto)
     , VRF.Signable (VRF crypto) Seed
     )
  => TransitionRule (CHAIN crypto)
chainTransition = do
  TRC (sNow, ChainState nes cs eta0 etaV etaC etaH lab, block@(Block bh _)) <- judgmentContext


  let NewEpochState _ _ _ (EpochState _ _ _ pp _) _ _ _ = nes

  maxpv <- liftSTS $ asks maxMajorPV
  let (ProtVer m _) = _protocolVersion pp
  m <= maxpv ?! ObsoleteNodeCHAIN m maxpv

  let bhb = bhbody bh
  let s = bheaderSlotNo bhb
  fromIntegral (bHeaderSize bh) <= _maxBHSize pp ?! HeaderSizeTooLargeCHAIN
  fromIntegral (hBbsize bhb) <= _maxBBSize pp ?! BlockSizeTooLargeCHAIN
  let gkeys = getGKeys nes

  nes' <-
    trans @(TICK crypto) $ TRC (TickEnv gkeys, nes, s)

  let NewEpochState e1 _ _ _ _ _ _ = nes
      NewEpochState e2 _ bcur es _ _pd osched = nes'
  let EpochState (AccountState _ _reserves) _ ls pp' _                       = es
  let LedgerState _ (DPState (DState _ _ _ _ _ _genDelegs _) (PState _ _ _)) = ls

  PrtclState cs' lab' eta0' etaV' etaC' etaH' <- trans @(PRTCL crypto)
    $ TRC ( PrtclEnv pp' osched _pd _genDelegs sNow (e1 /= e2)
          , PrtclState cs lab eta0 etaV etaC etaH
          , bh)

  BbodyState ls' bcur' <- trans @(BBODY crypto)
    $ TRC (BbodyEnv (Map.keysSet osched) pp' _reserves, BbodyState ls bcur, block)

  let nes'' = updateNES nes' bcur' ls'

  pure $ ChainState nes'' cs' eta0' etaV' etaC' etaH' lab'

instance
  ( Crypto crypto
  , Signable (DSIGN crypto) (VKeyES crypto, Natural, KESPeriod)
  , Signable (DSIGN crypto) (TxBody crypto)
  , KESignable crypto (BHBody crypto)
  , VRF.Signable (VRF crypto) Seed
  )
  => Embed (BBODY crypto) (CHAIN crypto)
 where
  wrapFailed = BbodyFailure

instance
  ( Crypto crypto
  , Signable (DSIGN crypto) (VKeyES crypto, Natural, KESPeriod)
  , Signable (DSIGN crypto) (TxBody crypto)
  , KESignable crypto (BHBody crypto)
  , VRF.Signable (VRF crypto) Seed
  )
  => Embed (TICK crypto) (CHAIN crypto)
 where
  wrapFailed = TickFailure

instance
  ( Crypto crypto
  , Signable (DSIGN crypto) (VKeyES crypto, Natural, KESPeriod)
  , Signable (DSIGN crypto) (TxBody crypto)
  , KESignable crypto (BHBody crypto)
  , VRF.Signable (VRF crypto) Seed
  )
  => Embed (PRTCL crypto) (CHAIN crypto)
 where
  wrapFailed = PrtclFailure

-- |Calculate the total ada in the chain state
totalAda :: ChainState crypto -> Coin
totalAda (ChainState nes _ _ _ _ _ _) =
  treasury_ + reserves_ + rewards_ + circulation + deposits + fees_
  where
    (EpochState (AccountState treasury_ reserves_) _ ls _ _) = nesEs nes
    (UTxOState u deposits fees_ _) = _utxoState ls
    (DPState ds _) = _delegationState ls
    rewards_ = sum (Map.elems (_rewards ds))
    circulation = balance u
