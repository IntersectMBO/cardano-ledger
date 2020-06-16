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

import qualified Cardano.Crypto.VRF as VRF
import Cardano.Prelude
  ( MonadError (..),
    NoUnexpectedThunks (..),
    asks,
    unless,
  )
import Control.State.Transition
import Data.Coerce (coerce)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import Shelley.Spec.Ledger.BaseTypes
  ( ActiveSlotCoeff,
    Nonce,
    Seed,
    ShelleyBase,
    activeSlotCoeff,
  )
import Shelley.Spec.Ledger.BlockChain
  ( BHBody (..),
    BHeader (..),
    checkVRFValue,
    mkSeed,
    seedEta,
    seedL,
  )
import Shelley.Spec.Ledger.Core (dom, range)
import Shelley.Spec.Ledger.Crypto
import Shelley.Spec.Ledger.Delegation.Certificates (PoolDistr (..))
import Shelley.Spec.Ledger.Keys
  ( DSignable,
    GenDelegPair (..),
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
    = VRFKeyUnknown
        !(KeyHash 'StakePool crypto) -- unknown VRF keyhash (not registered)
    | VRFKeyWrongVRFKey
        !(KeyHash 'StakePool crypto) -- KeyHash of block issuer
        !(Hash crypto (VerKeyVRF crypto)) --VRF KeyHash registered with stake pool
        !(Hash crypto (VerKeyVRF crypto)) --VRF KeyHash from Header
    | VRFKeyBadNonce
        !Nonce -- Nonce constant to distinguish VRF nonce values
        !SlotNo -- Slot used for VRF calculation
        !Nonce -- Epoch nonce used for VRF calculation
        !(VRF.CertifiedVRF (VRF crypto) Nonce) -- VRF calculated nonce value
    | VRFKeyBadLeaderValue
        !Nonce -- Leader constant to distinguish VRF leader values
        !SlotNo -- Slot used for VRF calculation
        !Nonce -- Epoch nonce used for VRF calculation
        !(VRF.CertifiedVRF (VRF crypto) Nonce) -- VRF calculated leader value
    | VRFLeaderValueTooBig
        !Natural -- VRF Leader value
        !Rational -- stake pool's relative stake
        !ActiveSlotCoeff -- Praos active slot coefficient value
    | NotActiveSlotOVERLAY
        !SlotNo -- Slot which is supposed to be silent
    | WrongGenesisColdKeyOVERLAY
        !(KeyHash 'BlockIssuer crypto) -- KeyHash of block issuer
        !(KeyHash 'GenesisDelegate crypto) -- KeyHash genesis delegate keyhash assigned to this slot
    | WrongGenesisVRFKeyOVERLAY
        !(KeyHash 'BlockIssuer crypto) -- KeyHash of block issuer
        !(Hash crypto (VerKeyVRF crypto)) --VRF KeyHash registered with genesis delegation
        !(Hash crypto (VerKeyVRF crypto)) --VRF KeyHash from Header
    | UnknownGenesisKeyOVERLAY
        !(KeyHash 'Genesis crypto) -- KeyHash which does not correspond to o genesis node
    | OcertFailure (PredicateFailure (OCERT crypto)) -- Subtransition Failures
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
  BHBody crypto ->
  Either (PredicateFailure (OVERLAY crypto)) ()
vrfChecks eta0 bhb = do
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
  where
    vrfK = bheaderVrfVk bhb
    slot = bheaderSlotNo bhb

praosVrfChecks ::
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
praosVrfChecks eta0 (PoolDistr pd) f bhb = do
  let sigma' = Map.lookup hk pd
  case sigma' of
    Nothing -> throwError $ VRFKeyUnknown hk
    Just (sigma, vrfHK) -> do
      unless
        (vrfHK == hashVerKeyVRF vrfK)
        (throwError $ VRFKeyWrongVRFKey hk vrfHK (hashVerKeyVRF vrfK))
      vrfChecks eta0 bhb
      unless
        (checkVRFValue (VRF.certifiedNatural $ bheaderL bhb) sigma f)
        (throwError $ VRFLeaderValueTooBig (VRF.certifiedNatural $ bheaderL bhb) sigma f)
      pure ()
  where
    hk = coerceKeyRole . hashKey $ bheaderVk bhb
    vrfK = bheaderVrfVk bhb

-- pbftVrfChecks ::
--   forall crypto.
--   ( Crypto crypto,
--     VRF.Signable (VRF crypto) Seed,
--     VRF.ContextVRF (VRF crypto) ~ ()
--   ) =>
--   Hash crypto (VerKeyVRF crypto) ->
--   Nonce ->
--   BHBody crypto ->
--   Either (PredicateFailure (OVERLAY crypto)) ()
-- pbftVrfChecks vrfHK eta0 bhb = do
--   unless
--     (vrfHK == hashVerKeyVRF vrfK)
--     (throwError $ WrongGenesisVRFKeyOVERLAY hk vrfHK (hashVerKeyVRF vrfK))
--   vrfChecks eta0 bhb
--   pure ()
--   where
--     hk = coerceKeyRole . hashKey $ bheaderVk bhb
--     vrfK = bheaderVrfVk bhb

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
            praosVrfChecks eta0 pd asc bhb ?!: id
          Just NonActiveSlot ->
            failBecause $ NotActiveSlotOVERLAY (bheaderSlotNo bhb)
          Just (ActiveSlot gkey) ->
            case Map.lookup gkey genDelegs of
              Nothing ->
                failBecause $ UnknownGenesisKeyOVERLAY gkey
              Just (GenDelegPair genDelegsKey _genesisVrfKH) -> do
                vkh == coerceKeyRole genDelegsKey ?! WrongGenesisColdKeyOVERLAY vkh genDelegsKey
        -- pbftVrfChecks genesisVrfKH eta0 bhb ?!: id

        let oce =
              OCertEnv
                { ocertEnvStPools = dom pd,
                  ocertEnvGenDelegs = Set.map genDelegKeyHash $ range genDelegs
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
