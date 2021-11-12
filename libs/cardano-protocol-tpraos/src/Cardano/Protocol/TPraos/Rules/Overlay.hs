{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Protocol.TPraos.Rules.Overlay
  ( OVERLAY,
    PredicateFailure,
    OverlayEnv (..),
    OverlayPredicateFailure (..),
    OBftSlot (..),
    classifyOverlaySlot,
    lookupInOverlaySchedule,
    overlaySlots,
  )
where

import Cardano.Binary
  ( FromCBOR (..),
    ToCBOR (..),
    TokenType (TypeNull),
    decodeNull,
    encodeNull,
    peekTokenType,
  )
import qualified Cardano.Crypto.VRF as VRF
import Cardano.Ledger.BHeaderView (isOverlaySlot)
import Cardano.Ledger.BaseTypes
  ( ActiveSlotCoeff,
    BoundedRational (..),
    Nonce,
    Seed,
    ShelleyBase,
    UnitInterval,
    activeSlotCoeff,
    activeSlotVal,
    epochInfo,
  )
import Cardano.Ledger.Crypto
import Cardano.Ledger.Keys
  ( DSignable,
    GenDelegPair (..),
    GenDelegs (..),
    Hash,
    KESignable,
    KeyHash (..),
    KeyRole (..),
    VerKeyVRF,
    coerceKeyRole,
    hashKey,
    hashVerKeyVRF,
  )
import Cardano.Ledger.PoolDistr
  ( IndividualPoolStake (..),
    PoolDistr (..),
  )
import Cardano.Ledger.Slot (epochInfoEpoch, epochInfoFirst, (-*))
import Cardano.Protocol.TPraos.BHeader
  ( BHBody (..),
    BHeader (BHeader),
    checkLeaderValue,
    issuerIDfromBHBody,
    mkSeed,
    seedEta,
    seedL,
  )
import Cardano.Protocol.TPraos.OCert (OCertSignable)
import Cardano.Protocol.TPraos.Rules.OCert (OCERT, OCertEnv (..))
import Cardano.Slotting.Slot
import Control.DeepSeq (NFData)
import Control.Monad (unless)
import Control.Monad.Except (throwError)
import Control.Monad.Trans.Reader (asks)
import Control.SetAlgebra (dom, eval, range)
import Control.State.Transition
import Data.Coerce (coerce)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Word (Word64)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))

data OVERLAY crypto

data OverlayEnv crypto
  = OverlayEnv
      UnitInterval -- the decentralization paramater @d@ from the protocal parameters
      (PoolDistr crypto)
      (GenDelegs crypto)
      Nonce
  deriving (Generic)

instance NoThunks (OverlayEnv crypto)

data OverlayPredicateFailure crypto
  = VRFKeyUnknown
      !(KeyHash 'StakePool crypto) -- unknown VRF keyhash (not registered)
  | VRFKeyWrongVRFKey
      !(KeyHash 'StakePool crypto) -- KeyHash of block issuer
      !(Hash crypto (VerKeyVRF crypto)) -- VRF KeyHash registered with stake pool
      !(Hash crypto (VerKeyVRF crypto)) -- VRF KeyHash from Header
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
      !(VRF.OutputVRF (VRF crypto)) -- VRF Leader value
      !Rational -- stake pool's relative stake
      !ActiveSlotCoeff -- Praos active slot coefficient value
  | NotActiveSlotOVERLAY
      !SlotNo -- Slot which is supposed to be silent
  | WrongGenesisColdKeyOVERLAY
      !(KeyHash 'BlockIssuer crypto) -- KeyHash of block issuer
      !(KeyHash 'GenesisDelegate crypto) -- KeyHash genesis delegate keyhash assigned to this slot
  | WrongGenesisVRFKeyOVERLAY
      !(KeyHash 'BlockIssuer crypto) -- KeyHash of block issuer
      !(Hash crypto (VerKeyVRF crypto)) -- VRF KeyHash registered with genesis delegation
      !(Hash crypto (VerKeyVRF crypto)) -- VRF KeyHash from Header
  | UnknownGenesisKeyOVERLAY
      !(KeyHash 'Genesis crypto) -- KeyHash which does not correspond to o genesis node
  | OcertFailure (PredicateFailure (OCERT crypto)) -- Subtransition Failures
  deriving (Generic)

instance
  ( Crypto crypto,
    DSignable crypto (OCertSignable crypto),
    KESignable crypto (BHBody crypto),
    VRF.Signable (VRF crypto) Seed
  ) =>
  STS (OVERLAY crypto)
  where
  type
    State (OVERLAY crypto) =
      Map (KeyHash 'BlockIssuer crypto) Word64

  type
    Signal (OVERLAY crypto) =
      BHeader crypto

  type Environment (OVERLAY crypto) = OverlayEnv crypto
  type BaseM (OVERLAY crypto) = ShelleyBase
  type PredicateFailure (OVERLAY crypto) = OverlayPredicateFailure crypto

  initialRules = []

  transitionRules = [overlayTransition]

deriving instance
  (VRF.VRFAlgorithm (VRF crypto)) =>
  Show (OverlayPredicateFailure crypto)

deriving instance
  (VRF.VRFAlgorithm (VRF crypto)) =>
  Eq (OverlayPredicateFailure crypto)

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
    (throwError $ VRFKeyBadLeaderValue seedL slot eta0 (coerce $ bheaderL bhb))
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
    Just (IndividualPoolStake sigma vrfHK) -> do
      unless
        (vrfHK == hashVerKeyVRF vrfK)
        (throwError $ VRFKeyWrongVRFKey hk vrfHK (hashVerKeyVRF vrfK))
      vrfChecks eta0 bhb
      unless
        (checkLeaderValue (VRF.certifiedOutput $ bheaderL bhb) sigma f)
        (throwError $ VRFLeaderValueTooBig (VRF.certifiedOutput $ bheaderL bhb) sigma f)
  where
    hk = coerceKeyRole . issuerIDfromBHBody $ bhb
    vrfK = bheaderVrfVk bhb

pbftVrfChecks ::
  forall crypto.
  ( Crypto crypto,
    VRF.Signable (VRF crypto) Seed,
    VRF.ContextVRF (VRF crypto) ~ ()
  ) =>
  Hash crypto (VerKeyVRF crypto) ->
  Nonce ->
  BHBody crypto ->
  Either (PredicateFailure (OVERLAY crypto)) ()
pbftVrfChecks vrfHK eta0 bhb = do
  unless
    (vrfHK == hashVerKeyVRF vrfK)
    (throwError $ WrongGenesisVRFKeyOVERLAY hk vrfHK (hashVerKeyVRF vrfK))
  vrfChecks eta0 bhb
  pure ()
  where
    hk = issuerIDfromBHBody bhb
    vrfK = bheaderVrfVk bhb

overlayTransition ::
  forall crypto.
  ( Crypto crypto,
    DSignable crypto (OCertSignable crypto),
    KESignable crypto (BHBody crypto),
    VRF.Signable (VRF crypto) Seed
  ) =>
  TransitionRule (OVERLAY crypto)
overlayTransition =
  judgmentContext
    >>= \( TRC
             ( OverlayEnv dval pd (GenDelegs genDelegs) eta0,
               cs,
               bh@(BHeader bhb _)
               )
           ) -> do
        let vk = bheaderVk bhb
            vkh = hashKey vk
            slot = bheaderSlotNo bhb
            gkeys = Map.keysSet genDelegs

        asc <- liftSTS $ asks activeSlotCoeff
        firstSlotNo <- liftSTS $ do
          ei <- asks epochInfo
          e <- epochInfoEpoch ei slot
          epochInfoFirst ei e

        case (lookupInOverlaySchedule firstSlotNo gkeys dval asc slot :: Maybe (OBftSlot crypto)) of
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
                  ocertEnvGenDelegs = Set.map genDelegKeyHash $ range genDelegs
                }

        trans @(OCERT crypto) $ TRC (oce, cs, bh)

instance
  (VRF.VRFAlgorithm (VRF crypto)) =>
  NoThunks (OverlayPredicateFailure crypto)

instance
  ( Crypto crypto,
    DSignable crypto (OCertSignable crypto),
    KESignable crypto (BHBody crypto),
    VRF.Signable (VRF crypto) Seed
  ) =>
  Embed (OCERT crypto) (OVERLAY crypto)
  where
  wrapFailed = OcertFailure

data OBftSlot crypto
  = NonActiveSlot
  | ActiveSlot !(KeyHash 'Genesis crypto)
  deriving (Show, Eq, Ord, Generic)

instance
  Crypto crypto =>
  ToCBOR (OBftSlot crypto)
  where
  toCBOR NonActiveSlot = encodeNull
  toCBOR (ActiveSlot k) = toCBOR k

instance
  Crypto crypto =>
  FromCBOR (OBftSlot crypto)
  where
  fromCBOR = do
    peekTokenType >>= \case
      TypeNull -> do
        decodeNull
        pure NonActiveSlot
      _ -> ActiveSlot <$> fromCBOR

instance NoThunks (OBftSlot crypto)

instance NFData (OBftSlot crypto)

classifyOverlaySlot ::
  SlotNo -> -- first slot of the epoch
  Set (KeyHash 'Genesis crypto) -> -- genesis Nodes
  UnitInterval -> -- decentralization parameter
  ActiveSlotCoeff -> -- active slot coefficent
  SlotNo -> -- overlay slot to classify
  OBftSlot crypto
classifyOverlaySlot firstSlotNo gkeys dval ascValue slot =
  if isActive
    then
      let genesisIdx = (position `div` ascInv) `mod` fromIntegral (length gkeys)
       in gkeys `getAtIndex` genesisIdx
    else NonActiveSlot
  where
    d = unboundRational dval
    position = ceiling (fromIntegral (slot -* firstSlotNo) * d)
    isActive = position `mod` ascInv == 0
    getAtIndex gs i = if i < length gs then ActiveSlot (Set.elemAt i gs) else NonActiveSlot
    ascInv = floor (1 / unboundRational (activeSlotVal ascValue))

lookupInOverlaySchedule ::
  SlotNo -> -- first slot of the epoch
  Set (KeyHash 'Genesis crypto) -> -- genesis Nodes
  UnitInterval -> -- decentralization parameter
  ActiveSlotCoeff -> -- active slot coefficent
  SlotNo -> -- slot to lookup
  Maybe (OBftSlot crypto)
lookupInOverlaySchedule firstSlotNo gkeys dval ascValue slot =
  if isOverlaySlot firstSlotNo dval slot
    then Just $ classifyOverlaySlot firstSlotNo gkeys dval ascValue slot
    else Nothing

-- | Return the list of overlaySlots for a given epoch.
-- Note that this linear in the size of the epoch, and should probably
-- only be used for testing.
-- If something more performant is needed, we could probably use
-- [start + floor(x/d) | x <- [0 .. (spe -1)], floor(x/d) < spe]
-- but we would need to make sure that this is equivalent.
overlaySlots ::
  SlotNo -> -- starting slot
  UnitInterval -> -- decentralization parameter
  EpochSize ->
  [SlotNo]
overlaySlots start d (EpochSize spe) =
  [SlotNo x | x <- [unSlotNo start .. end], isOverlaySlot start d (SlotNo x)]
  where
    end = unSlotNo start + spe - 1
