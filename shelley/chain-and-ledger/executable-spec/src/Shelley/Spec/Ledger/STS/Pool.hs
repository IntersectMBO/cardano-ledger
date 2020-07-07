{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Shelley.Spec.Ledger.STS.Pool
  ( POOL,
    PoolEnv (..),
    PredicateFailure (..),
  )
where

import Cardano.Binary (FromCBOR (..), ToCBOR (..), decodeListLen, decodeWord, encodeListLen, matchSize)
import Cardano.Prelude (NoUnexpectedThunks (..))
import Control.Monad.Trans.Reader (asks)
import Control.State.Transition (STS (..), TRC (..), TransitionRule, failBecause, judgmentContext, liftSTS, (?!))
import Data.Kind (Type)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Typeable (Typeable)
import Data.Word (Word64, Word8)
import GHC.Generics (Generic)
import Shelley.Spec.Ledger.BaseTypes (Globals (..), ShelleyBase, invalidKey)
import Shelley.Spec.Ledger.Coin (Coin)
import Shelley.Spec.Ledger.Core (addpair, haskey, removekey)
import Shelley.Spec.Ledger.Crypto (Crypto)
import Shelley.Spec.Ledger.Keys (KeyHash (..), KeyRole (..))
import Shelley.Spec.Ledger.LedgerState (PState (..), emptyPState)
import Shelley.Spec.Ledger.PParams (PParams, PParams' (..))
import Shelley.Spec.Ledger.Slot (EpochNo (..), SlotNo, epochInfoEpoch)
import Shelley.Spec.Ledger.TxData (DCert (..), PoolCert (..), PoolParams (..), StakePools (..))

data POOL (crypto :: Type)

data PoolEnv
  = PoolEnv SlotNo PParams
  deriving (Show, Eq)

instance Typeable crypto => STS (POOL crypto) where
  type State (POOL crypto) = PState crypto

  type Signal (POOL crypto) = DCert crypto

  type Environment (POOL crypto) = PoolEnv

  type BaseM (POOL crypto) = ShelleyBase

  data PredicateFailure (POOL crypto)
    = StakePoolNotRegisteredOnKeyPOOL
        !(KeyHash 'StakePool crypto) -- KeyHash which cannot be retired since it is not registered
    | StakePoolRetirementWrongEpochPOOL
        !Word64 -- Current Epoch
        !Word64 -- The epoch listed in the Pool Retirement Certificate
        !Word64 -- The first epoch that is too far out for retirement
    | WrongCertificateTypePOOL
        !Word8 -- The disallowed certificate (this case should never happen)
    | StakePoolCostTooLowPOOL
        !Coin -- The stake pool cost listed in the Pool Registration Certificate
        !Coin -- The minimum stake pool cost listed in the protocol parameters
    deriving (Show, Eq, Generic)

  initialRules = [pure emptyPState]

  transitionRules = [poolDelegationTransition]

instance NoUnexpectedThunks (PredicateFailure (POOL crypto))

instance
  (Typeable crypto, Crypto crypto) =>
  ToCBOR (PredicateFailure (POOL crypto))
  where
  toCBOR = \case
    StakePoolNotRegisteredOnKeyPOOL kh ->
      encodeListLen 2 <> toCBOR (0 :: Word8) <> toCBOR kh
    StakePoolRetirementWrongEpochPOOL ce e em ->
      encodeListLen 4 <> toCBOR (1 :: Word8) <> toCBOR ce <> toCBOR e <> toCBOR em
    WrongCertificateTypePOOL ct ->
      encodeListLen 2 <> toCBOR (2 :: Word8) <> toCBOR ct
    StakePoolCostTooLowPOOL pc mc ->
      encodeListLen 3 <> toCBOR (3 :: Word8) <> toCBOR pc <> toCBOR mc

instance
  (Crypto crypto) =>
  FromCBOR (PredicateFailure (POOL crypto))
  where
  fromCBOR = do
    n <- decodeListLen
    decodeWord >>= \case
      0 -> do
        matchSize "StakePoolNotRegisteredOnKeyPOOL" 2 n
        kh <- fromCBOR
        pure $ StakePoolNotRegisteredOnKeyPOOL kh
      1 -> do
        ce <- fromCBOR
        e <- fromCBOR
        em <- fromCBOR
        matchSize "StakePoolRetirementWrongEpochPOOL" 4 n
        pure $ StakePoolRetirementWrongEpochPOOL ce e em
      2 -> do
        matchSize "WrongCertificateTypePOOL" 2 n
        ct <- fromCBOR
        pure $ WrongCertificateTypePOOL ct
      3 -> do
        matchSize "StakePoolCostTooLowPOOL" 3 n
        pc <- fromCBOR
        mc <- fromCBOR
        pure $ StakePoolCostTooLowPOOL pc mc
      k -> invalidKey k

poolDelegationTransition :: Typeable crypto => TransitionRule (POOL crypto)
poolDelegationTransition = do
  TRC (PoolEnv slot pp, ps, c) <- judgmentContext
  let StakePools stpools = _stPools ps
  case c of
    DCertPool (RegPool poolParam) -> do
      -- note that pattern match is used instead of cwitness, as in the spec

      let poolCost = _poolCost poolParam
          minPoolCost = _minPoolCost pp
      poolCost >= minPoolCost ?! StakePoolCostTooLowPOOL poolCost minPoolCost

      let hk = _poolPubKey poolParam
      if not (haskey hk stpools) -- hk ∉ (dom stpools)
        then -- register new, Pool-Reg

          pure $
            ps
              { _stPools = StakePools $ stpools ∪ (hk, slot),
                _pParams = _pParams ps ∪ (hk, poolParam)
              }
        else do
          pure $
            ps
              { _fPParams = _fPParams ps ⨃ (hk, poolParam),
                _retiring = removekey hk (_retiring ps) -- Set.singleton hk ⋪ _retiring ps
              }
    DCertPool (RetirePool hk (EpochNo e)) -> do
      -- note that pattern match is used instead of cwitness, as in the spec
      --  hk ∈ dom stpools  -- Specification code translates below
      haskey hk stpools ?! StakePoolNotRegisteredOnKeyPOOL hk
      EpochNo cepoch <- liftSTS $ do
        ei <- asks epochInfo
        epochInfoEpoch ei slot
      let EpochNo maxEpoch = _eMax pp
      cepoch < e && e <= cepoch + maxEpoch
        ?! StakePoolRetirementWrongEpochPOOL cepoch e (cepoch + maxEpoch)
      pure $ ps {_retiring = _retiring ps ⨃ (hk, EpochNo e)}
    DCertDeleg _ -> do
      failBecause $ WrongCertificateTypePOOL 0
      pure ps
    DCertMir _ -> do
      failBecause $ WrongCertificateTypePOOL 1
      pure ps
    DCertGenesis _ -> do
      failBecause $ WrongCertificateTypePOOL 2
      pure ps

-- Note: we avoid using the Relation operators (⨃) and (∪) here because that
-- would require an Ord instance for PParams, which we don't need otherwise.
-- Instead, we just define these operators here.

(⨃) ::
  Map (KeyHash kr crypto) a ->
  (KeyHash kr crypto, a) ->
  Map (KeyHash kr crypto) a
m ⨃ (k, v) = Map.insertWith (\x _ -> x) k v m -- we want this to be left biased, hence (\ x _ -> x)

(∪) ::
  Ord a =>
  Map (KeyHash kr crypto) a ->
  (KeyHash kr crypto, a) ->
  Map (KeyHash kr crypto) a
m ∪ (k, v) = addpair k v m
