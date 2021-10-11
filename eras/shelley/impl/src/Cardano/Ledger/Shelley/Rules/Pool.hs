{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Shelley.Rules.Pool
  ( POOL,
    PoolEvent (..),
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
import Cardano.Crypto.Hash.Class (sizeHash)
import Cardano.Ledger.BaseTypes (Globals (..), Network, ProtVer, ShelleyBase, StrictMaybe (..), epochInfo, invalidKey, networkId)
import Cardano.Ledger.Coin (Coin)
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Crypto as CC (Crypto (HASH))
import Cardano.Ledger.Era (Crypto, Era)
import Cardano.Ledger.Keys (KeyHash (..), KeyRole (..))
import Cardano.Ledger.Serialization (decodeRecordSum)
import qualified Cardano.Ledger.Shelley.HardForks as HardForks
import Cardano.Ledger.Shelley.LedgerState (PState (..))
import qualified Cardano.Ledger.Shelley.SoftForks as SoftForks
import Cardano.Ledger.Shelley.TxBody
  ( DCert (..),
    PoolCert (..),
    PoolMetadata (..),
    PoolParams (..),
    getRwdNetwork,
  )
import Cardano.Ledger.Slot (EpochNo (..), SlotNo, epochInfoEpoch)
import Control.Monad (when)
import Control.Monad.Trans.Reader (asks)
import Control.SetAlgebra (dom, eval, setSingleton, singleton, (∈), (∉), (∪), (⋪), (⨃))
import Control.State.Transition
  ( STS (..),
    TRC (..),
    TransitionRule,
    failBecause,
    judgmentContext,
    liftSTS,
    tellEvent,
    (?!),
  )
import qualified Data.ByteString as BS
import Data.Kind (Type)
import Data.Typeable (Typeable)
import Data.Word (Word64, Word8)
import GHC.Generics (Generic)
import GHC.Records (HasField (getField))
import NoThunks.Class (NoThunks (..))

data POOL (era :: Type)

data PoolEnv era
  = PoolEnv SlotNo (Core.PParams era)

deriving instance (Show (Core.PParams era)) => Show (PoolEnv era)

deriving instance (Eq (Core.PParams era)) => Eq (PoolEnv era)

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
  | WrongNetworkPOOL
      !Network -- Actual Network ID
      !Network -- Network ID listed in Pool Registration Certificate
      !(KeyHash 'StakePool (Crypto era)) -- Stake Pool ID
  | PoolMedataHashTooBig
      !(KeyHash 'StakePool (Crypto era)) -- Stake Pool ID
      !Int -- Size of the metadata hash
  deriving (Show, Eq, Generic)

instance NoThunks (PoolPredicateFailure era)

instance
  ( Era era,
    HasField "_minPoolCost" (Core.PParams era) Coin,
    HasField "_eMax" (Core.PParams era) EpochNo,
    HasField "_protocolVersion" (Core.PParams era) ProtVer
  ) =>
  STS (POOL era)
  where
  type State (POOL era) = PState (Crypto era)

  type Signal (POOL era) = DCert (Crypto era)

  type Environment (POOL era) = PoolEnv era

  type BaseM (POOL era) = ShelleyBase
  type PredicateFailure (POOL era) = PoolPredicateFailure era
  type Event (POOL era) = PoolEvent era

  transitionRules = [poolDelegationTransition]

data PoolEvent era
  = RegisterPool (KeyHash 'StakePool (Crypto era))
  | ReregisterPool (KeyHash 'StakePool (Crypto era))

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
    WrongNetworkPOOL a b c ->
      encodeListLen 4 <> toCBOR (4 :: Word8) <> toCBOR a <> toCBOR b <> toCBOR c
    PoolMedataHashTooBig a b ->
      encodeListLen 3 <> toCBOR (5 :: Word8) <> toCBOR a <> toCBOR b

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
      4 -> do
        actualNetID <- fromCBOR
        suppliedNetID <- fromCBOR
        poolID <- fromCBOR
        pure (4, WrongNetworkPOOL actualNetID suppliedNetID poolID)
      5 -> do
        poolID <- fromCBOR
        s <- fromCBOR
        pure (3, PoolMedataHashTooBig poolID s)
      k -> invalidKey k

poolDelegationTransition ::
  forall era.
  ( Era era,
    HasField "_minPoolCost" (Core.PParams era) Coin,
    HasField "_eMax" (Core.PParams era) EpochNo,
    HasField "_protocolVersion" (Core.PParams era) ProtVer
  ) =>
  TransitionRule (POOL era)
poolDelegationTransition = do
  TRC (PoolEnv slot pp, ps, c) <- judgmentContext
  let stpools = _pParams ps
  case c of
    DCertPool (RegPool poolParam) -> do
      -- note that pattern match is used instead of cwitness, as in the spec

      when (HardForks.validatePoolRewardAccountNetID pp) $ do
        actualNetID <- liftSTS $ asks networkId
        let suppliedNetID = getRwdNetwork (_poolRAcnt poolParam)
        actualNetID == suppliedNetID
          ?! WrongNetworkPOOL actualNetID suppliedNetID (_poolId poolParam)

      when (SoftForks.restrictPoolMetadataHash pp) $ do
        case _poolMD poolParam of
          SNothing -> pure ()
          SJust pmd ->
            let s = BS.length (_poolMDHash pmd)
             in s <= fromIntegral (sizeHash ([] @(CC.HASH (Crypto era))))
                  ?! PoolMedataHashTooBig (_poolId poolParam) s

      let poolCost = _poolCost poolParam
          minPoolCost = getField @"_minPoolCost" pp
      poolCost >= minPoolCost ?! StakePoolCostTooLowPOOL poolCost minPoolCost

      let hk = _poolId poolParam
      if eval (hk ∉ dom stpools)
        then do
          -- register new, Pool-Reg
          tellEvent $ RegisterPool hk
          pure $
            ps
              { _pParams = eval (_pParams ps ∪ singleton hk poolParam)
              }
        else do
          tellEvent $ ReregisterPool hk
          pure $
            ps
              { _fPParams = eval (_fPParams ps ⨃ singleton hk poolParam),
                _retiring = eval (setSingleton hk ⋪ _retiring ps)
              }
    DCertPool (RetirePool hk (EpochNo e)) -> do
      -- note that pattern match is used instead of cwitness, as in the spec
      eval (hk ∈ dom stpools) ?! StakePoolNotRegisteredOnKeyPOOL hk
      EpochNo cepoch <- liftSTS $ do
        ei <- asks epochInfo
        epochInfoEpoch ei slot
      let EpochNo maxEpoch = getField @"_eMax" pp
      cepoch < e && e <= cepoch + maxEpoch
        ?! StakePoolRetirementWrongEpochPOOL cepoch e (cepoch + maxEpoch)
      pure $ ps {_retiring = eval (_retiring ps ⨃ singleton hk (EpochNo e))}
    DCertDeleg _ -> do
      failBecause $ WrongCertificateTypePOOL 0
      pure ps
    DCertMir _ -> do
      failBecause $ WrongCertificateTypePOOL 1
      pure ps
    DCertGenesis _ -> do
      failBecause $ WrongCertificateTypePOOL 2
      pure ps
