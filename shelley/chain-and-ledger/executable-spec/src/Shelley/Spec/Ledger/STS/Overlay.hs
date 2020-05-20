{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
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
import Cardano.Prelude (MonadError (..), NoUnexpectedThunks (..), asks, unless)
import Control.State.Transition
import Data.Coerce (coerce)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import Shelley.Spec.Ledger.BaseTypes (ActiveSlotCoeff, Nonce, Seed, ShelleyBase, UnitInterval, activeSlotCoeff)
import Shelley.Spec.Ledger.BlockChain (BHBody (..), BHeader (..), checkVRFValue, mkSeed, seedEta, seedL)
import Shelley.Spec.Ledger.Crypto
import Shelley.Spec.Ledger.Delegation.Certificates (PoolDistr (..))
import Shelley.Spec.Ledger.Keys
  ( DSignable,
    GenDelegs (..),
    Hash,
    KESignable,
    KeyHash,
    KeyRole (..),
    VerKeyKES,
    VerKeyVRF,
    coerceKeyRole,
    hashKey,
    hashVerKeyVRF,
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
    = VRFKeyUnknown (KeyHash 'StakePool crypto)
    | VRFKeyWrongVRFKey
        (Hash crypto (VerKeyVRF crypto))
        -- ^ Registered/correct VRF key hash (exists in stake pool distribution).
        (Hash crypto (VerKeyVRF crypto))
        -- ^ Unregistered/incorrect VRF key hash (does not exist in
        -- stake pool distribution).
    | VRFKeyBadNonce
        Nonce
        -- ^ Seed nonce.
        SlotNo
        -- ^ Current slot number.
        Nonce
        -- ^ Previous header hash as nonce.
        (VRF.CertifiedVRF (VRF crypto) Nonce)
        -- ^ Block nonce.
    | VRFKeyBadLeaderValue
        Nonce
        -- ^ Seed nonce.
        SlotNo
        -- ^ Current slot number.
        Nonce
        -- ^ Previous header hash as nonce.
        (VRF.CertifiedVRF (VRF crypto) UnitInterval)
        -- ^ Leader election value.
    | VRFLeaderValueTooBig
        Natural
        -- ^ Leader Election value
        Rational
        -- ^ "Weight"/sigma of delegation pool
        ActiveSlotCoeff
    | NotActiveSlotOVERLAY SlotNo
    | WrongGenesisColdKeyOVERLAY (KeyHash 'BlockIssuer crypto) (KeyHash 'GenesisDelegate crypto)
    | UnknownGenesisKeyOVERLAY (KeyHash 'Genesis crypto)
    | OcertFailure (PredicateFailure (OCERT crypto))
    deriving (Generic)

  initialRules = []

  transitionRules = [overlayTransition]

deriving instance (VRF.VRFAlgorithm (VRF crypto)) => Show (PredicateFailure (OVERLAY crypto))

deriving instance (VRF.VRFAlgorithm (VRF crypto)) => Eq (PredicateFailure (OVERLAY crypto))

vrfChecks ::
  forall crypto.
  ( Crypto crypto,
    VRF.Signable (VRF crypto) Seed,
    VRF.ContextVRF (VRF crypto) ~ ()
  ) =>
  Nonce ->
  PoolDistr crypto ->
  ActiveSlotCoeff ->
  BHBody crypto ->
  Either (PredicateFailure (OVERLAY crypto)) ()
vrfChecks eta0 (PoolDistr pd) f bhb = do
  let sigma' = Map.lookup hk pd
  case sigma' of
    Nothing -> throwError $ VRFKeyUnknown hk
    Just (sigma, vrfHK) -> do
      unless
        (vrfHK == hashVerKeyVRF vrfK)
        (throwError $ VRFKeyWrongVRFKey vrfHK (hashVerKeyVRF vrfK))
      unless
        ( VRF.verifyCertified
            ()
            vrfK
            (mkSeed seedEta slot eta0)
            (coerce $ bheaderEta bhb)
        )
        (throwError $ VRFKeyBadNonce seedEta slot eta0 (coerce $ bheaderEta bhb))
      unless
        ( VRF.verifyCertified
            ()
            vrfK
            (mkSeed seedL slot eta0)
            (coerce $ bheaderL bhb)
        )
        (throwError $ VRFKeyBadLeaderValue seedEta slot eta0 (coerce $ bheaderL bhb))
      unless
        (checkVRFValue (VRF.certifiedNatural $ bheaderL bhb) sigma f)
        (throwError $ VRFLeaderValueTooBig (VRF.certifiedNatural $ bheaderL bhb) sigma f)
      pure ()
  where
    hk = coerceKeyRole . hashKey $ bheaderVk bhb
    vrfK = bheaderVrfVk bhb
    slot = bheaderSlotNo bhb

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
            vrfChecks eta0 pd asc bhb ?!: id
          Just NonActiveSlot ->
            failBecause $ NotActiveSlotOVERLAY (bheaderSlotNo bhb)
          Just (ActiveSlot gkey) ->
            case Map.lookup gkey genDelegs of
              Nothing ->
                failBecause $ UnknownGenesisKeyOVERLAY gkey
              Just genDelegsKey ->
                vkh == coerceKeyRole genDelegsKey ?! WrongGenesisColdKeyOVERLAY vkh genDelegsKey

        let oce =
              OCertEnv
                { ocertEnvStPools = dom pd,
                  ocertEnvGenDelegs = range genDelegs
                }

        trans @(OCERT crypto) $ TRC (oce, cs, bh)

instance (VRF.VRFAlgorithm (VRF crypto)) => NoUnexpectedThunks (PredicateFailure (OVERLAY crypto))

instance
  ( Crypto crypto,
    DSignable crypto (VerKeyKES crypto, Natural, KESPeriod),
    KESignable crypto (BHBody crypto),
    VRF.Signable (VRF crypto) Seed
  ) =>
  Embed (OCERT crypto) (OVERLAY crypto)
  where
  wrapFailed = OcertFailure
