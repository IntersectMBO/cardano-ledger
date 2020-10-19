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
    PredicateFailure,
    PoolPredicateFailure (..),
  )
where

import Cardano.Binary
  ( FromCBOR (..),
    ToCBOR (..),
    encodeListLen,
  )
import Cardano.Ledger.Era (Crypto, Era)
import Control.Monad.Trans.Reader (asks)
import Control.SetAlgebra (dom, eval, setSingleton, singleton, (∈), (∉), (∪), (⋪), (⨃))
import Control.State.Transition
  ( STS (..),
    TRC (..),
    TransitionRule,
    failBecause,
    judgmentContext,
    liftSTS,
    (?!),
  )
import Data.Kind (Type)
import Data.Typeable (Typeable)
import Data.Word (Word64, Word8)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))
import Shelley.Spec.Ledger.BaseTypes (Globals (..), ShelleyBase, invalidKey)
import Shelley.Spec.Ledger.Coin (Coin)
import Shelley.Spec.Ledger.Keys (KeyHash (..), KeyRole (..))
import Shelley.Spec.Ledger.LedgerState (PState (..), emptyPState)
import Shelley.Spec.Ledger.PParams (PParams, PParams' (..))
import Shelley.Spec.Ledger.Serialization (decodeRecordSum)
import Shelley.Spec.Ledger.Slot (EpochNo (..), SlotNo, epochInfoEpoch)
import Shelley.Spec.Ledger.TxBody
  ( DCert (..),
    PoolCert (..),
    PoolParams (..),
  )

data POOL (era :: Type)

data PoolEnv era
  = PoolEnv SlotNo (PParams era)
  deriving (Show, Eq)

data PoolPredicateFailure era
  = StakePoolNotRegisteredOnKeyPOOL
      !(KeyHash 'StakePool (Crypto era)) -- KeyHash which cannot be retired since it is not registered
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

instance NoThunks (PoolPredicateFailure era)

instance Typeable era => STS (POOL era) where
  type State (POOL era) = PState era

  type Signal (POOL era) = DCert era

  type Environment (POOL era) = PoolEnv era

  type BaseM (POOL era) = ShelleyBase
  type PredicateFailure (POOL era) = PoolPredicateFailure era

  initialRules = [pure emptyPState]

  transitionRules = [poolDelegationTransition]

instance
  (Typeable era, Era era) =>
  ToCBOR (PoolPredicateFailure era)
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
  (Era era) =>
  FromCBOR (PoolPredicateFailure era)
  where
  fromCBOR = decodeRecordSum "PredicateFailure (POOL era)" $
    \case
      0 -> do
        kh <- fromCBOR
        pure (2, StakePoolNotRegisteredOnKeyPOOL kh)
      1 -> do
        ce <- fromCBOR
        e <- fromCBOR
        em <- fromCBOR
        pure (4, StakePoolRetirementWrongEpochPOOL ce e em)
      2 -> do
        ct <- fromCBOR
        pure (2, WrongCertificateTypePOOL ct)
      3 -> do
        pc <- fromCBOR
        mc <- fromCBOR
        pure (3, StakePoolCostTooLowPOOL pc mc)
      k -> invalidKey k

poolDelegationTransition :: Typeable era => TransitionRule (POOL era)
poolDelegationTransition = do
  TRC (PoolEnv slot pp, ps, c) <- judgmentContext
  let stpools = _pParams ps
  case c of
    DCertPool (RegPool poolParam) -> do
      -- note that pattern match is used instead of cwitness, as in the spec

      let poolCost = _poolCost poolParam
          minPoolCost = _minPoolCost pp
      poolCost >= minPoolCost ?! StakePoolCostTooLowPOOL poolCost minPoolCost

      let hk = _poolPubKey poolParam
      if eval (hk ∉ (dom stpools))
        then -- register new, Pool-Reg

          pure $
            ps
              { _pParams = eval (_pParams ps ∪ (singleton hk poolParam))
              }
        else do
          pure $
            ps
              { _fPParams = eval (_fPParams ps ⨃ (singleton hk poolParam)),
                _retiring = eval (setSingleton hk ⋪ _retiring ps)
              }
    DCertPool (RetirePool hk (EpochNo e)) -> do
      -- note that pattern match is used instead of cwitness, as in the spec
      eval (hk ∈ dom stpools) ?! StakePoolNotRegisteredOnKeyPOOL hk
      EpochNo cepoch <- liftSTS $ do
        ei <- asks epochInfo
        epochInfoEpoch ei slot
      let EpochNo maxEpoch = _eMax pp
      cepoch < e && e <= cepoch + maxEpoch
        ?! StakePoolRetirementWrongEpochPOOL cepoch e (cepoch + maxEpoch)
      pure $ ps {_retiring = eval (_retiring ps ⨃ (singleton hk (EpochNo e)))}
    DCertDeleg _ -> do
      failBecause $ WrongCertificateTypePOOL 0
      pure ps
    DCertMir _ -> do
      failBecause $ WrongCertificateTypePOOL 1
      pure ps
    DCertGenesis _ -> do
      failBecause $ WrongCertificateTypePOOL 2
      pure ps
