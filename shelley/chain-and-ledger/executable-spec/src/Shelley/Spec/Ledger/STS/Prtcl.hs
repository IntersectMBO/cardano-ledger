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

module Shelley.Spec.Ledger.STS.Prtcl
  ( PRTCL,
    State,
    PrtclEnv (..),
    PrtclState (..),
    PredicateFailure (..),
  )
where

import Cardano.Binary
  ( FromCBOR (fromCBOR),
    ToCBOR (toCBOR),
    decodeListLenOf,
    encodeListLen,
  )
import qualified Cardano.Crypto.VRF as VRF
import Cardano.Prelude (NoUnexpectedThunks (..))
import Control.State.Transition
import Data.Map.Strict (Map)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import Shelley.Spec.Ledger.BaseTypes (Nonce, Seed, ShelleyBase)
import Shelley.Spec.Ledger.BlockChain
  ( BHBody (..),
    BHeader (..),
    bhHash,
    bhbody,
    hashHeaderToNonce,
  )
import Shelley.Spec.Ledger.Crypto (Crypto)
import Shelley.Spec.Ledger.Delegation.Certificates (PoolDistr)
import Shelley.Spec.Ledger.Keys
  ( DSignable,
    GenDelegs (..),
    KESignable,
    KeyHash,
    KeyRole (..),
    VRFSignable,
    VerKeyKES,
    fromNatural,
  )
import Shelley.Spec.Ledger.LedgerState (OBftSlot)
import Shelley.Spec.Ledger.OCert (KESPeriod)
import Shelley.Spec.Ledger.PParams (PParams)
import Shelley.Spec.Ledger.STS.Overlay (OVERLAY, OverlayEnv (..))
import Shelley.Spec.Ledger.STS.Updn (UPDN, UpdnEnv (..), UpdnState (..))
import Shelley.Spec.Ledger.Slot (SlotNo)

data PRTCL crypto

data PrtclState crypto
  = PrtclState
      !(Map (KeyHash 'BlockIssuer crypto) Natural)
      !Nonce
      -- ^ Current previous hash nonce
      !Nonce
      -- ^ Current epoch nonce
      !Nonce
      -- ^ Evolving nonce
      !Nonce
      -- ^ Candidate nonce
      !Nonce
      -- ^ Prev epoch hash nonce
  deriving (Generic, Show, Eq)

instance Crypto crypto => ToCBOR (PrtclState crypto) where
  toCBOR (PrtclState m n1 n2 n3 n4 n5) =
    mconcat
      [ encodeListLen 6,
        toCBOR m,
        toCBOR n1,
        toCBOR n2,
        toCBOR n3,
        toCBOR n4,
        toCBOR n5
      ]

instance Crypto crypto => FromCBOR (PrtclState crypto) where
  fromCBOR =
    decodeListLenOf 6
      >> PrtclState
      <$> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR

instance Crypto crypto => NoUnexpectedThunks (PrtclState crypto)

data PrtclEnv crypto
  = -- | New epoch marker
    PrtclEnv
      PParams
      (Map SlotNo (OBftSlot crypto))
      (PoolDistr crypto)
      (GenDelegs crypto)
      Bool
  deriving (Generic)

instance NoUnexpectedThunks (PrtclEnv crypto)

instance
  ( Crypto crypto,
    DSignable crypto (VerKeyKES crypto, Natural, KESPeriod),
    KESignable crypto (BHBody crypto),
    VRFSignable crypto Seed
  ) =>
  STS (PRTCL crypto)
  where
  type
    State (PRTCL crypto) =
      PrtclState crypto

  type
    Signal (PRTCL crypto) =
      BHeader crypto

  type
    Environment (PRTCL crypto) =
      PrtclEnv crypto

  type BaseM (PRTCL crypto) = ShelleyBase

  data PredicateFailure (PRTCL crypto)
    = OverlayFailure (PredicateFailure (OVERLAY crypto))
    | UpdnFailure (PredicateFailure (UPDN crypto))
    deriving (Show, Eq, Generic)

  initialRules = []

  transitionRules = [prtclTransition]

prtclTransition ::
  forall crypto.
  ( Crypto crypto,
    DSignable crypto (VerKeyKES crypto, Natural, KESPeriod),
    KESignable crypto (BHBody crypto),
    VRFSignable crypto Seed
  ) =>
  TransitionRule (PRTCL crypto)
prtclTransition = do
  TRC
    ( PrtclEnv pp osched pd dms ne,
      PrtclState cs etaPH eta0 etaV etaC etaH,
      bh
      ) <-
    judgmentContext
  let bhb = bhbody bh
      slot = bheaderSlotNo bhb
      eta = fromNatural . VRF.certifiedNatural $ bheaderEta bhb

  UpdnState eta0' etaV' etaC' etaH' <-
    trans @(UPDN crypto) $
      TRC
        ( UpdnEnv eta pp etaPH ne,
          UpdnState eta0 etaV etaC etaH,
          slot
        )
  cs' <-
    trans @(OVERLAY crypto) $
      TRC (OverlayEnv osched eta0' pd dms, cs, bh)

  pure $
    PrtclState
      cs'
      (hashHeaderToNonce (bhHash bh))
      eta0'
      etaV'
      etaC'
      etaH'

instance (Crypto crypto) => NoUnexpectedThunks (PredicateFailure (PRTCL crypto))

instance
  ( Crypto crypto,
    DSignable crypto (VerKeyKES crypto, Natural, KESPeriod),
    KESignable crypto (BHBody crypto),
    VRFSignable crypto Seed
  ) =>
  Embed (OVERLAY crypto) (PRTCL crypto)
  where
  wrapFailed = OverlayFailure

instance
  ( Crypto crypto,
    DSignable crypto (VerKeyKES crypto, Natural, KESPeriod),
    KESignable crypto (BHBody crypto),
    VRFSignable crypto Seed
  ) =>
  Embed (UPDN crypto) (PRTCL crypto)
  where
  wrapFailed = UpdnFailure
