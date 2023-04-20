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

module Cardano.Protocol.TPraos.Rules.Overlay (
  OVERLAY,
  PredicateFailure,
  OverlayEnv (..),
  OverlayPredicateFailure (..),
  OBftSlot (..),
  classifyOverlaySlot,
  lookupInOverlaySchedule,
  overlaySlots,
  toPoolStakeVRF,
  fromPoolStakeVRF,
  hashPoolStakeVRF,
  toGenesisVRF,
  fromGenesisVRF,
  hashGenesisVRF,
)
where

import qualified Cardano.Crypto.VRF as VRF
import Cardano.Ledger.BHeaderView (isOverlaySlot)
import Cardano.Ledger.BaseTypes (
  ActiveSlotCoeff,
  BoundedRational (..),
  Nonce,
  Seed,
  ShelleyBase,
  UnitInterval,
  activeSlotCoeff,
  activeSlotVal,
  epochInfoPure,
 )
import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (..),
  TokenType (TypeNull),
  decodeNull,
  encodeNull,
  peekTokenType,
 )
import Cardano.Ledger.Crypto
import Cardano.Ledger.Keys (
  DSignable,
  GenDelegPair (..),
  GenDelegs (..),
  Hash,
  KeyHash (..),
  KeyRole (..),
  coerceKeyRole,
  fromGenesisVRF,
  hashGenesisVRF,
  hashKey,
  hashVerKeyVRF,
  toGenesisVRF,
 )
import Cardano.Ledger.PoolDistr (
  IndividualPoolStake (..),
  PoolDistr (..),
  fromPoolStakeVRF,
  hashPoolStakeVRF,
  toPoolStakeVRF,
 )
import Cardano.Ledger.Slot (epochInfoEpoch, epochInfoFirst, (-*))
import Cardano.Protocol.HeaderCrypto
import Cardano.Protocol.HeaderKeys (
  KESignable,
  VerKeyVRF,
 )
import Cardano.Protocol.TPraos.BHeader (
  BHBody (..),
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

data OVERLAY c hc

data OverlayEnv c
  = OverlayEnv
      UnitInterval -- the decentralization paramater @d@ from the protocal parameters
      (PoolDistr c)
      (GenDelegs c)
      Nonce
  deriving (Generic)

instance NoThunks (OverlayEnv c)

data OverlayPredicateFailure c hc
  = VRFKeyUnknown
      !(KeyHash 'StakePool c) -- unknown VRF keyhash (not registered)
  | VRFKeyWrongVRFKey
      !(KeyHash 'StakePool c) -- KeyHash of block issuer
      !(Hash c (VerKeyVRF hc)) -- VRF KeyHash registered with stake pool
      !(Hash c (VerKeyVRF hc)) -- VRF KeyHash from Header
  | VRFKeyBadNonce
      !Nonce -- Nonce constant to distinguish VRF nonce values
      !SlotNo -- Slot used for VRF calculation
      !Nonce -- Epoch nonce used for VRF calculation
      !(VRF.CertifiedVRF (VRF hc) Nonce) -- VRF calculated nonce value
  | VRFKeyBadLeaderValue
      !Nonce -- Leader constant to distinguish VRF leader values
      !SlotNo -- Slot used for VRF calculation
      !Nonce -- Epoch nonce used for VRF calculation
      !(VRF.CertifiedVRF (VRF hc) Nonce) -- VRF calculated leader value
  | VRFLeaderValueTooBig
      !(VRF.OutputVRF (VRF hc)) -- VRF Leader value
      !Rational -- stake pool's relative stake
      !ActiveSlotCoeff -- Praos active slot coefficient value
  | NotActiveSlotOVERLAY
      !SlotNo -- Slot which is supposed to be silent
  | WrongGenesisColdKeyOVERLAY
      !(KeyHash 'BlockIssuer c) -- KeyHash of block issuer
      !(KeyHash 'GenesisDelegate c) -- KeyHash genesis delegate keyhash assigned to this slot
  | WrongGenesisVRFKeyOVERLAY
      !(KeyHash 'BlockIssuer c) -- KeyHash of block issuer
      !(Hash c (VerKeyVRF hc)) -- VRF KeyHash registered with genesis delegation
      !(Hash c (VerKeyVRF hc)) -- VRF KeyHash from Header
  | UnknownGenesisKeyOVERLAY
      !(KeyHash 'Genesis c) -- KeyHash which does not correspond to o genesis node
  | OcertFailure (PredicateFailure (OCERT c hc)) -- Subtransition Failures
  deriving (Generic)

instance
  ( Crypto c
  , HeaderCrypto hc
  , DSignable c (OCertSignable hc)
  , KESignable hc (BHBody c hc)
  , VRF.Signable (VRF hc) Seed
  ) =>
  STS (OVERLAY c hc)
  where
  type
    State (OVERLAY c hc) =
      Map (KeyHash 'BlockIssuer c) Word64

  type
    Signal (OVERLAY c hc) =
      BHeader c hc

  type Environment (OVERLAY c hc) = OverlayEnv c
  type BaseM (OVERLAY c hc) = ShelleyBase
  type PredicateFailure (OVERLAY c hc) = OverlayPredicateFailure c hc

  initialRules = []

  transitionRules = [overlayTransition]

deriving instance
  (VRF.VRFAlgorithm (VRF hc)) =>
  Show (OverlayPredicateFailure c hc)

deriving instance
  (VRF.VRFAlgorithm (VRF hc)) =>
  Eq (OverlayPredicateFailure c hc)

vrfChecks ::
  forall c hc.
  ( HeaderCrypto hc
  , VRF.Signable (VRF hc) Seed
  ) =>
  Nonce ->
  BHBody c hc ->
  Either (PredicateFailure (OVERLAY c hc)) ()
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
  forall c hc.
  ( Crypto c
  , HeaderCrypto hc
  , VRF.Signable (VRF hc) Seed
  ) =>
  Nonce ->
  PoolDistr c ->
  ActiveSlotCoeff ->
  BHBody c hc ->
  Either (PredicateFailure (OVERLAY c hc)) ()
praosVrfChecks eta0 (PoolDistr pd) f bhb = do
  let sigma' = Map.lookup hk pd
  case sigma' of
    Nothing -> throwError $ VRFKeyUnknown hk
    Just (IndividualPoolStake sigma vrfHK) -> do
      let vrfHKfromHeader = hashVerKeyVRF vrfK
          vrfHKregistered = fromPoolStakeVRF vrfHK
      unless
        (vrfHKregistered == vrfHKfromHeader)
        (throwError $ VRFKeyWrongVRFKey hk vrfHKregistered vrfHKfromHeader)
      vrfChecks eta0 bhb
      unless
        (checkLeaderValue (VRF.certifiedOutput $ bheaderL bhb) sigma f)
        (throwError $ VRFLeaderValueTooBig (VRF.certifiedOutput $ bheaderL bhb) sigma f)
  where
    hk = coerceKeyRole . issuerIDfromBHBody $ bhb
    vrfK = bheaderVrfVk bhb

pbftVrfChecks ::
  forall c hc.
  ( Crypto c
  , HeaderCrypto hc
  , VRF.Signable (VRF hc) Seed
  ) =>
  Hash c (VerKeyVRF hc) ->
  Nonce ->
  BHBody c hc ->
  Either (PredicateFailure (OVERLAY c hc)) ()
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
  forall c hc.
  ( Crypto c
  , HeaderCrypto hc
  , DSignable c (OCertSignable hc)
  , KESignable hc (BHBody c hc)
  , VRF.Signable (VRF hc) Seed
  ) =>
  TransitionRule (OVERLAY c hc)
overlayTransition =
  judgmentContext
    >>= \( TRC
            ( OverlayEnv dval pd (GenDelegs genDelegs) eta0
              , cs
              , bh@(BHeader bhb _)
              )
          ) -> do
        let vk = bheaderVk bhb
            vkh = hashKey vk
            slot = bheaderSlotNo bhb
            gkeys = Map.keysSet genDelegs

        asc <- liftSTS $ asks activeSlotCoeff
        firstSlotNo <- liftSTS $ do
          ei <- asks epochInfoPure
          e <- epochInfoEpoch ei slot
          epochInfoFirst ei e

        case (lookupInOverlaySchedule firstSlotNo gkeys dval asc slot :: Maybe (OBftSlot c)) of
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
                pbftVrfChecks (fromGenesisVRF genesisVrfKH) eta0 bhb ?!: id

        let oce =
              OCertEnv
                { ocertEnvStPools = eval (dom pd)
                , ocertEnvGenDelegs = Set.map genDelegKeyHash $ range genDelegs
                }

        trans @(OCERT c hc) $ TRC (oce, cs, bh)

instance
  (VRF.VRFAlgorithm (VRF hc)) =>
  NoThunks (OverlayPredicateFailure c hc)

instance
  ( Crypto c
  , HeaderCrypto hc
  , DSignable c (OCertSignable hc)
  , KESignable hc (BHBody c hc)
  , VRF.Signable (VRF hc) Seed
  ) =>
  Embed (OCERT c hc) (OVERLAY c hc)
  where
  wrapFailed = OcertFailure

data OBftSlot c
  = NonActiveSlot
  | ActiveSlot !(KeyHash 'Genesis c)
  deriving (Show, Eq, Ord, Generic)

instance
  Crypto c =>
  EncCBOR (OBftSlot c)
  where
  encCBOR NonActiveSlot = encodeNull
  encCBOR (ActiveSlot k) = encCBOR k

instance
  Crypto c =>
  DecCBOR (OBftSlot c)
  where
  decCBOR = do
    peekTokenType >>= \case
      TypeNull -> do
        decodeNull
        pure NonActiveSlot
      _ -> ActiveSlot <$> decCBOR

instance NoThunks (OBftSlot c)

instance NFData (OBftSlot c)

classifyOverlaySlot ::
  SlotNo -> -- first slot of the epoch
  Set (KeyHash 'Genesis c) -> -- genesis Nodes
  UnitInterval -> -- decentralization parameter
  ActiveSlotCoeff -> -- active slot coefficent
  SlotNo -> -- overlay slot to classify
  OBftSlot c
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
  Set (KeyHash 'Genesis c) -> -- genesis Nodes
  UnitInterval -> -- decentralization parameter
  ActiveSlotCoeff -> -- active slot coefficent
  SlotNo -> -- slot to lookup
  Maybe (OBftSlot c)
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
