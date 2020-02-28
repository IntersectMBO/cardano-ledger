{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module STS.Overlay
  ( OVERLAY
  , OverlayEnv(..)
  )
where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           GHC.Generics (Generic)
import           Ledger.Core (dom, range)
import           Numeric.Natural (Natural)

import           BaseTypes
import           BlockChain
import           Delegation.Certificates
import           Keys
import           OCert
import           PParams
import           Slot

import           STS.Ocert

import qualified Cardano.Crypto.VRF as VRF
import           Cardano.Ledger.Shelley.Crypto
import           Cardano.Prelude (NoUnexpectedThunks (..))
import           Control.State.Transition

data OVERLAY crypto

data OverlayEnv crypto
  = OverlayEnv
      PParams
      (Map SlotNo (Maybe (GenKeyHash crypto)))
      Nonce
      (PoolDistr crypto)
      (GenDelegs crypto)
  deriving Generic

instance NoUnexpectedThunks (OverlayEnv crypto)

instance
  ( Crypto crypto
  , Signable (DSIGN crypto) (VKeyES crypto, Natural, KESPeriod)
  , KESignable crypto (BHBody crypto)
  , VRF.Signable (VRF crypto) Seed
  )
  => STS (OVERLAY crypto)
 where
  type State (OVERLAY crypto)
    = Map (KeyHash crypto) Natural

  type Signal (OVERLAY crypto)
    = BHeader crypto

  type Environment (OVERLAY crypto) = OverlayEnv crypto
  type BaseM (OVERLAY crypto) = ShelleyBase

  data PredicateFailure (OVERLAY crypto)
    = NotPraosLeaderOVERLAY
    | NotActiveSlotOVERLAY
    | WrongGenesisColdKeyOVERLAY (KeyHash crypto) (KeyHash crypto)
    | NoGenesisStakingOVERLAY
    | OcertFailure (PredicateFailure (OCERT crypto))
    deriving (Show, Eq, Generic)

  initialRules = []

  transitionRules = [overlayTransition]

overlayTransition
  :: forall crypto
   . ( Crypto crypto
     , Signable (DSIGN crypto) (VKeyES crypto, Natural, KESPeriod)
     , KESignable crypto (BHBody crypto)
     , VRF.Signable (VRF crypto) Seed
     )
  => TransitionRule (OVERLAY crypto)
overlayTransition = do
  TRC ( OverlayEnv pp osched eta0 pd (GenDelegs genDelegs)
      , cs
      , bh@(BHeader bhb _)) <- judgmentContext
  let vk = bvkcold bhb
      vkh = hashKey vk

  case Map.lookup (bheaderSlotNo bhb) osched of
    Nothing ->
      vrfChecks eta0 pd ((activeSlotVal . _activeSlotCoeff) pp) bhb ?! NotPraosLeaderOVERLAY
    Just Nothing ->
      failBecause NotActiveSlotOVERLAY
    Just (Just gkey) ->
      case Map.lookup gkey genDelegs of
        Nothing ->
          failBecause NoGenesisStakingOVERLAY
        Just genDelegsKey ->
          vkh == genDelegsKey ?! WrongGenesisColdKeyOVERLAY vkh genDelegsKey

  let
    oce = OCertEnv
      { ocertEnvStPools = dom pd, ocertEnvGenDelegs = range genDelegs }

  trans @(OCERT crypto) $ TRC (oce, cs, bh)

instance NoUnexpectedThunks (PredicateFailure (OVERLAY crypto))

instance
  ( Crypto crypto
  , Signable (DSIGN crypto) (VKeyES crypto, Natural, KESPeriod)
  , KESignable crypto (BHBody crypto)
  , VRF.Signable (VRF crypto) Seed
  )
  => Embed (OCERT crypto) (OVERLAY crypto)
 where
  wrapFailed = OcertFailure
