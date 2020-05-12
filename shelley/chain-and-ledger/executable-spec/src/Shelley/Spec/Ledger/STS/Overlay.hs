{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Shelley.Spec.Ledger.STS.Overlay
  ( OVERLAY,
    PredicateFailure (..),
    OverlayEnv (..),
  )
where

import Byron.Spec.Ledger.Core (dom, range)
import qualified Cardano.Crypto.VRF as VRF
import Cardano.Prelude (asks)
import Cardano.Prelude (NoUnexpectedThunks (..))
import Control.State.Transition
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import Shelley.Spec.Ledger.BaseTypes (Nonce, Seed, ShelleyBase, activeSlotCoeff)
import Shelley.Spec.Ledger.BlockChain (BHBody (..), BHeader (..), vrfChecks)
import Shelley.Spec.Ledger.Crypto
import Shelley.Spec.Ledger.Delegation.Certificates (PoolDistr)
import Shelley.Spec.Ledger.Keys
  ( DSignable,
    GenDelegs (..),
    KESignable,
    KeyHash,
    KeyRole (..),
    VerKeyKES,
    coerceKeyRole,
    hashKey,
  )
import Shelley.Spec.Ledger.LedgerState (OBftSlot (..))
import Shelley.Spec.Ledger.OCert (KESPeriod)
import Shelley.Spec.Ledger.STS.Ocert (OCERT, OCertEnv (..))
import Shelley.Spec.Ledger.Slot (SlotNo)

data OVERLAY crypto

data OverlayEnv crypto
  = OverlayEnv
      (Map SlotNo (OBftSlot crypto))
      Nonce
      (PoolDistr crypto)
      (GenDelegs crypto)
  deriving (Generic)

instance NoUnexpectedThunks (OverlayEnv crypto)

instance
  ( Crypto crypto,
    DSignable crypto (VerKeyKES crypto, Natural, KESPeriod),
    KESignable crypto (BHBody crypto),
    VRF.Signable (VRF crypto) Seed
  ) =>
  STS (OVERLAY crypto)
  where
  type
    State (OVERLAY crypto) =
      Map (KeyHash 'BlockIssuer crypto) Natural

  type
    Signal (OVERLAY crypto) =
      BHeader crypto

  type Environment (OVERLAY crypto) = OverlayEnv crypto
  type BaseM (OVERLAY crypto) = ShelleyBase

  data PredicateFailure (OVERLAY crypto)
    = NotPraosLeaderOVERLAY
    | NotActiveSlotOVERLAY
    | WrongGenesisColdKeyOVERLAY (KeyHash 'BlockIssuer crypto) (KeyHash 'GenesisDelegate crypto)
    | NoGenesisStakingOVERLAY
    | OcertFailure (PredicateFailure (OCERT crypto))
    deriving (Show, Eq, Generic)

  initialRules = []

  transitionRules = [overlayTransition]

overlayTransition ::
  forall crypto.
  ( Crypto crypto,
    DSignable crypto (VerKeyKES crypto, Natural, KESPeriod),
    KESignable crypto (BHBody crypto),
    VRF.Signable (VRF crypto) Seed
  ) =>
  TransitionRule (OVERLAY crypto)
overlayTransition =
  judgmentContext
    >>= \( TRC
             ( OverlayEnv osched eta0 pd (GenDelegs genDelegs),
               cs,
               bh@(BHeader bhb _)
               )
           ) -> do
        let vk = bheaderVk bhb
            vkh = hashKey vk

        asc <- liftSTS $ asks activeSlotCoeff

        case Map.lookup (bheaderSlotNo bhb) osched of
          Nothing ->
            vrfChecks eta0 pd asc bhb ?! NotPraosLeaderOVERLAY
          Just NonActiveSlot ->
            failBecause NotActiveSlotOVERLAY
          Just (ActiveSlot gkey) ->
            case Map.lookup gkey genDelegs of
              Nothing ->
                failBecause NoGenesisStakingOVERLAY
              Just genDelegsKey ->
                vkh == coerceKeyRole genDelegsKey ?! WrongGenesisColdKeyOVERLAY vkh genDelegsKey

        let oce =
              OCertEnv
                { ocertEnvStPools = dom pd,
                  ocertEnvGenDelegs = range genDelegs
                }

        trans @(OCERT crypto) $ TRC (oce, cs, bh)

instance NoUnexpectedThunks (PredicateFailure (OVERLAY crypto))

instance
  ( Crypto crypto,
    DSignable crypto (VerKeyKES crypto, Natural, KESPeriod),
    KESignable crypto (BHBody crypto),
    VRF.Signable (VRF crypto) Seed
  ) =>
  Embed (OCERT crypto) (OVERLAY crypto)
  where
  wrapFailed = OcertFailure
