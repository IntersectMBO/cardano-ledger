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
)
where

import qualified Cardano.Crypto.KES as KES
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
import Cardano.Ledger.Keys (
  GenDelegPair (..),
  GenDelegs (..),
  KeyHash (..),
  KeyRole (..),
  coerceKeyRole,
  hashKey,
 )
import Cardano.Ledger.PoolDistr (
  IndividualPoolStake (..),
  PoolDistr (..),
 )
import Cardano.Ledger.Slot (epochInfoEpoch, epochInfoFirst, (-*))
import Cardano.Protocol.Crypto
import Cardano.Protocol.TPraos.BHeader (
  BHBody (..),
  BHeader (BHeader),
  checkLeaderValue,
  issuerIDfromBHBody,
  mkSeed,
  seedEta,
  seedL,
 )
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

data OVERLAY c

data OverlayEnv
  = OverlayEnv
      UnitInterval -- the decentralization paramater @d@ from the protocal parameters
      PoolDistr
      GenDelegs
      Nonce
  deriving (Generic)

instance NoThunks OverlayEnv

data OverlayPredicateFailure c
  = VRFKeyUnknown
      !(KeyHash 'StakePool) -- unknown VRF keyhash (not registered)
  | VRFKeyWrongVRFKey
      !(KeyHash 'StakePool) -- KeyHash of block issuer
      !(VRFVerKeyHash 'StakePoolVRF) -- VRF KeyHash registered with stake pool
      !(VRFVerKeyHash 'BlockIssuerVRF) -- VRF KeyHash from Header
  | VRFKeyBadNonce
      !Nonce -- Nonce constant to distinguish VRF nonce values
      !SlotNo -- Slot used for VRF calculation
      !Nonce -- Epoch nonce used for VRF calculation
      !(VRF.CertifiedVRF (VRF c) Nonce) -- VRF calculated nonce value
  | VRFKeyBadLeaderValue
      !Nonce -- Leader constant to distinguish VRF leader values
      !SlotNo -- Slot used for VRF calculation
      !Nonce -- Epoch nonce used for VRF calculation
      !(VRF.CertifiedVRF (VRF c) Nonce) -- VRF calculated leader value
  | VRFLeaderValueTooBig
      !(VRF.OutputVRF (VRF c)) -- VRF Leader value
      !Rational -- stake pool's relative stake
      !ActiveSlotCoeff -- Praos active slot coefficient value
  | NotActiveSlotOVERLAY
      !SlotNo -- Slot which is supposed to be silent
  | WrongGenesisColdKeyOVERLAY
      !(KeyHash 'BlockIssuer) -- KeyHash of block issuer
      !(KeyHash 'GenesisDelegate) -- KeyHash genesis delegate keyhash assigned to this slot
  | WrongGenesisVRFKeyOVERLAY
      !(KeyHash 'BlockIssuer) -- KeyHash of block issuer
      !(VRFVerKeyHash 'GenDelegVRF) -- VRF KeyHash registered with genesis delegation
      !(VRFVerKeyHash 'BlockIssuerVRF) -- VRF KeyHash from Header
  | UnknownGenesisKeyOVERLAY
      !(KeyHash 'Genesis) -- KeyHash which does not correspond to o genesis node
  | OcertFailure (PredicateFailure (OCERT c)) -- Subtransition Failures
  deriving (Generic)

instance
  ( Crypto c
  , KES.Signable (KES c) (BHBody c)
  , VRF.Signable (VRF c) Seed
  ) =>
  STS (OVERLAY c)
  where
  type State (OVERLAY c) = Map (KeyHash 'BlockIssuer) Word64
  type Signal (OVERLAY c) = BHeader c
  type Environment (OVERLAY c) = OverlayEnv
  type BaseM (OVERLAY c) = ShelleyBase
  type PredicateFailure (OVERLAY c) = OverlayPredicateFailure c

  initialRules = []

  transitionRules = [overlayTransition]

deriving instance
  VRF.VRFAlgorithm (VRF c) =>
  Show (OverlayPredicateFailure c)

deriving instance
  VRF.VRFAlgorithm (VRF c) =>
  Eq (OverlayPredicateFailure c)

vrfChecks ::
  forall c.
  ( Crypto c
  , VRF.Signable (VRF c) Seed
  ) =>
  Nonce ->
  BHBody c ->
  Either (PredicateFailure (OVERLAY c)) ()
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
  forall c.
  ( Crypto c
  , VRF.Signable (VRF c) Seed
  ) =>
  Nonce ->
  PoolDistr ->
  ActiveSlotCoeff ->
  BHBody c ->
  Either (PredicateFailure (OVERLAY c)) ()
praosVrfChecks eta0 (PoolDistr pd _tot) f bhb = do
  let sigma' = Map.lookup hk pd
  case sigma' of
    Nothing -> throwError $ VRFKeyUnknown hk
    Just (IndividualPoolStake sigma _ stakePoolVRFVerKeyHash) -> do
      unless
        (unVRFVerKeyHash stakePoolVRFVerKeyHash == unVRFVerKeyHash blockIssuerVRFVerKeyHash)
        (throwError $ VRFKeyWrongVRFKey hk stakePoolVRFVerKeyHash blockIssuerVRFVerKeyHash)
      vrfChecks eta0 bhb
      unless
        (checkLeaderValue (VRF.certifiedOutput $ bheaderL bhb) sigma f)
        (throwError $ VRFLeaderValueTooBig (VRF.certifiedOutput $ bheaderL bhb) sigma f)
  where
    hk = coerceKeyRole . issuerIDfromBHBody $ bhb
    blockIssuerVRFVerKeyHash = hashVerKeyVRF @c (bheaderVrfVk bhb)

pbftVrfChecks ::
  forall c.
  ( Crypto c
  , VRF.Signable (VRF c) Seed
  ) =>
  VRFVerKeyHash 'GenDelegVRF ->
  Nonce ->
  BHBody c ->
  Either (PredicateFailure (OVERLAY c)) ()
pbftVrfChecks genDelegVRFVerKeyHash eta0 bhb = do
  unless
    (unVRFVerKeyHash genDelegVRFVerKeyHash == unVRFVerKeyHash blockIssuerVRFVerKeyHash)
    (throwError $ WrongGenesisVRFKeyOVERLAY hk genDelegVRFVerKeyHash blockIssuerVRFVerKeyHash)
  vrfChecks eta0 bhb
  pure ()
  where
    hk = issuerIDfromBHBody bhb
    blockIssuerVRFVerKeyHash = hashVerKeyVRF @c (bheaderVrfVk bhb)

overlayTransition ::
  forall c.
  ( Crypto c
  , KES.Signable (KES c) (BHBody c)
  , VRF.Signable (VRF c) Seed
  ) =>
  TransitionRule (OVERLAY c)
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
          pure $ epochInfoFirst ei $ epochInfoEpoch ei slot

        case lookupInOverlaySchedule firstSlotNo gkeys dval asc slot :: Maybe OBftSlot of
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
                { ocertEnvStPools = eval (dom $ unPoolDistr pd)
                , ocertEnvGenDelegs = Set.map genDelegKeyHash $ range genDelegs
                }

        trans @(OCERT c) $ TRC (oce, cs, bh)

instance
  VRF.VRFAlgorithm (VRF c) =>
  NoThunks (OverlayPredicateFailure c)

instance
  ( Crypto c
  , KES.Signable (KES c) (BHBody c)
  , VRF.Signable (VRF c) Seed
  ) =>
  Embed (OCERT c) (OVERLAY c)
  where
  wrapFailed = OcertFailure

data OBftSlot
  = NonActiveSlot
  | ActiveSlot !(KeyHash 'Genesis)
  deriving (Show, Eq, Ord, Generic)

instance EncCBOR OBftSlot where
  encCBOR NonActiveSlot = encodeNull
  encCBOR (ActiveSlot k) = encCBOR k

instance DecCBOR OBftSlot where
  decCBOR = do
    peekTokenType >>= \case
      TypeNull -> do
        decodeNull
        pure NonActiveSlot
      _ -> ActiveSlot <$> decCBOR

instance NoThunks OBftSlot

instance NFData OBftSlot

classifyOverlaySlot ::
  SlotNo -> -- first slot of the epoch
  Set (KeyHash 'Genesis) -> -- genesis Nodes
  UnitInterval -> -- decentralization parameter
  ActiveSlotCoeff -> -- active slot coefficent
  SlotNo -> -- overlay slot to classify
  OBftSlot
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
  Set (KeyHash 'Genesis) -> -- genesis Nodes
  UnitInterval -> -- decentralization parameter
  ActiveSlotCoeff -> -- active slot coefficent
  SlotNo -> -- slot to lookup
  Maybe OBftSlot
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
