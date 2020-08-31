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
import Cardano.Ledger.Crypto (VRF)
import Cardano.Ledger.Era
import Cardano.Prelude
  ( MonadError (..),
    NoUnexpectedThunks (..),
    asks,
    unless,
  )
import Control.Iterate.SetAlgebra (dom, eval, range)
import Control.State.Transition
import Data.Coerce (coerce)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Word (Word64)
import GHC.Generics (Generic)
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
    checkLeaderValue,
    mkSeed,
    poolIDfromBHBody,
    seedEta,
    seedL,
  )
import Shelley.Spec.Ledger.Delegation.Certificates
  ( IndividualPoolStake (..),
    PoolDistr (..),
  )
import Shelley.Spec.Ledger.Keys
  ( DSignable,
    GenDelegPair (..),
    GenDelegs (..),
    Hash,
    KESignable,
    KeyHash,
    KeyRole (..),
    VerKeyVRF,
    coerceKeyRole,
    hashKey,
    hashVerKeyVRF,
  )
import Shelley.Spec.Ledger.OCert (OCertSignable)
import Shelley.Spec.Ledger.OverlaySchedule
  ( OBftSlot (..),
    OverlaySchedule,
    lookupInOverlaySchedule,
  )
import Shelley.Spec.Ledger.STS.Ocert (OCERT, OCertEnv (..))
import Shelley.Spec.Ledger.Slot (SlotNo)

data OVERLAY era

data OverlayEnv era
  = OverlayEnv
      (OverlaySchedule era)
      (PoolDistr era)
      (GenDelegs era)
      Nonce
  deriving (Generic)

instance NoUnexpectedThunks (OverlayEnv era)

instance
  ( Era era,
    DSignable era (OCertSignable era),
    KESignable era (BHBody era),
    VRF.Signable (VRF (Crypto era)) Seed
  ) =>
  STS (OVERLAY era)
  where
  type
    State (OVERLAY era) =
      Map (KeyHash 'BlockIssuer era) Word64

  type
    Signal (OVERLAY era) =
      BHeader era

  type Environment (OVERLAY era) = OverlayEnv era
  type BaseM (OVERLAY era) = ShelleyBase

  data PredicateFailure (OVERLAY era)
    = VRFKeyUnknown
        !(KeyHash 'StakePool era) -- unknown VRF keyhash (not registered)
    | VRFKeyWrongVRFKey
        !(KeyHash 'StakePool era) -- KeyHash of block issuer
        !(Hash era (VerKeyVRF era)) --VRF KeyHash registered with stake pool
        !(Hash era (VerKeyVRF era)) --VRF KeyHash from Header
    | VRFKeyBadNonce
        !Nonce -- Nonce constant to distinguish VRF nonce values
        !SlotNo -- Slot used for VRF calculation
        !Nonce -- Epoch nonce used for VRF calculation
        !(VRF.CertifiedVRF (VRF (Crypto era)) Nonce) -- VRF calculated nonce value
    | VRFKeyBadLeaderValue
        !Nonce -- Leader constant to distinguish VRF leader values
        !SlotNo -- Slot used for VRF calculation
        !Nonce -- Epoch nonce used for VRF calculation
        !(VRF.CertifiedVRF (VRF (Crypto era)) Nonce) -- VRF calculated leader value
    | VRFLeaderValueTooBig
        !(VRF.OutputVRF (VRF (Crypto era))) -- VRF Leader value
        !Rational -- stake pool's relative stake
        !ActiveSlotCoeff -- Praos active slot coefficient value
    | NotActiveSlotOVERLAY
        !SlotNo -- Slot which is supposed to be silent
    | WrongGenesisColdKeyOVERLAY
        !(KeyHash 'BlockIssuer era) -- KeyHash of block issuer
        !(KeyHash 'GenesisDelegate era) -- KeyHash genesis delegate keyhash assigned to this slot
    | WrongGenesisVRFKeyOVERLAY
        !(KeyHash 'BlockIssuer era) -- KeyHash of block issuer
        !(Hash era (VerKeyVRF era)) --VRF KeyHash registered with genesis delegation
        !(Hash era (VerKeyVRF era)) --VRF KeyHash from Header
    | UnknownGenesisKeyOVERLAY
        !(KeyHash 'Genesis era) -- KeyHash which does not correspond to o genesis node
    | OcertFailure (PredicateFailure (OCERT era)) -- Subtransition Failures
    deriving (Generic)

  initialRules = []

  transitionRules = [overlayTransition]

deriving instance (VRF.VRFAlgorithm (VRF (Crypto era))) => Show (PredicateFailure (OVERLAY era))

deriving instance (VRF.VRFAlgorithm (VRF (Crypto era))) => Eq (PredicateFailure (OVERLAY era))

vrfChecks ::
  forall era.
  ( Era era,
    VRF.Signable (VRF (Crypto era)) Seed,
    VRF.ContextVRF (VRF (Crypto era)) ~ ()
  ) =>
  Nonce ->
  BHBody era ->
  Either (PredicateFailure (OVERLAY era)) ()
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
    (throwError $ VRFKeyBadLeaderValue seedL slot eta0 (coerce $ bheaderL bhb))
  where
    vrfK = bheaderVrfVk bhb
    slot = bheaderSlotNo bhb

praosVrfChecks ::
  forall era.
  ( Era era,
    VRF.Signable (VRF (Crypto era)) Seed,
    VRF.ContextVRF (VRF (Crypto era)) ~ ()
  ) =>
  Nonce ->
  PoolDistr era ->
  ActiveSlotCoeff ->
  BHBody era ->
  Either (PredicateFailure (OVERLAY era)) ()
praosVrfChecks eta0 (PoolDistr pd) f bhb = do
  let sigma' = Map.lookup hk pd
  case sigma' of
    Nothing -> throwError $ VRFKeyUnknown hk
    Just (IndividualPoolStake sigma vrfHK) -> do
      unless
        (vrfHK == hashVerKeyVRF vrfK)
        (throwError $ VRFKeyWrongVRFKey hk vrfHK (hashVerKeyVRF vrfK))
      vrfChecks eta0 bhb
      unless
        (checkLeaderValue (VRF.certifiedOutput $ bheaderL bhb) sigma f)
        (throwError $ VRFLeaderValueTooBig (VRF.certifiedOutput $ bheaderL bhb) sigma f)
      pure ()
  where
    hk = coerceKeyRole . poolIDfromBHBody $ bhb
    vrfK = bheaderVrfVk bhb

pbftVrfChecks ::
  forall era.
  ( Era era,
    VRF.Signable (VRF (Crypto era)) Seed,
    VRF.ContextVRF (VRF (Crypto era)) ~ ()
  ) =>
  Hash era (VerKeyVRF era) ->
  Nonce ->
  BHBody era ->
  Either (PredicateFailure (OVERLAY era)) ()
pbftVrfChecks vrfHK eta0 bhb = do
  unless
    (vrfHK == hashVerKeyVRF vrfK)
    (throwError $ WrongGenesisVRFKeyOVERLAY hk vrfHK (hashVerKeyVRF vrfK))
  vrfChecks eta0 bhb
  pure ()
  where
    hk = poolIDfromBHBody bhb
    vrfK = bheaderVrfVk bhb

overlayTransition ::
  forall era.
  ( Era era,
    DSignable era (OCertSignable era),
    KESignable era (BHBody era),
    VRF.Signable (VRF (Crypto era)) Seed
  ) =>
  TransitionRule (OVERLAY era)
overlayTransition =
  judgmentContext
    >>= \( TRC
             ( OverlayEnv osched pd (GenDelegs genDelegs) eta0,
               cs,
               bh@(BHeader bhb _)
               )
           ) -> do
        let vk = bheaderVk bhb
            vkh = hashKey vk

        asc <- liftSTS $ asks activeSlotCoeff

        case lookupInOverlaySchedule (bheaderSlotNo bhb) osched of
          Nothing ->
            praosVrfChecks eta0 pd asc bhb ?!: id
          Just NonActiveSlot ->
            failBecause $ NotActiveSlotOVERLAY (bheaderSlotNo bhb)
          Just (ActiveSlot gkey) ->
            case Map.lookup gkey genDelegs of
              Nothing ->
                failBecause $ UnknownGenesisKeyOVERLAY gkey
              Just (GenDelegPair genDelegsKey genesisVrfKH) -> do
                vkh == coerceKeyRole genDelegsKey ?! WrongGenesisColdKeyOVERLAY vkh genDelegsKey
                pbftVrfChecks genesisVrfKH eta0 bhb ?!: id

        let oce =
              OCertEnv
                { ocertEnvStPools = eval (dom pd),
                  ocertEnvGenDelegs = Set.map genDelegKeyHash $ (range genDelegs)
                }

        trans @(OCERT era) $ TRC (oce, cs, bh)

instance (VRF.VRFAlgorithm (VRF (Crypto era))) => NoUnexpectedThunks (PredicateFailure (OVERLAY era))

instance
  ( Era era,
    DSignable era (OCertSignable era),
    KESignable era (BHBody era),
    VRF.Signable (VRF (Crypto era)) Seed
  ) =>
  Embed (OCERT era) (OVERLAY era)
  where
  wrapFailed = OcertFailure
