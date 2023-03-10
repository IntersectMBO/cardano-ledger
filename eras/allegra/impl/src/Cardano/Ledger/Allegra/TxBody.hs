{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Allegra.TxBody (
  AllegraTxBody (
    AllegraTxBody,
    atbAuxDataHash,
    atbCerts,
    atbInputs,
    atbOutputs,
    atbTxFee,
    atbUpdate,
    atbValidityInterval,
    atbWithdrawals
  ),
  AllegraEraTxBody (..),
  emptyAllegraTxBodyRaw,
  AllegraTxBodyRaw (..),
  StrictMaybe (..),
  ValidityInterval (..),
)
where

import Cardano.Ledger.Allegra.Core (AllegraEraTxBody (..))
import Cardano.Ledger.Allegra.Delegation ()
import Cardano.Ledger.Allegra.Era (AllegraEra)
import Cardano.Ledger.Allegra.Scripts (ValidityInterval (..))
import Cardano.Ledger.Allegra.TxOut ()
import Cardano.Ledger.AuxiliaryData (AuxiliaryDataHash)
import Cardano.Ledger.BaseTypes (StrictMaybe (SJust, SNothing))
import Cardano.Ledger.Binary (Annotator, DecCBOR (..), EncCBOR (..), ToCBOR)
import Cardano.Ledger.Binary.Coders (
  Decode (..),
  Encode (..),
  Field,
  decode,
  encode,
  encodeKeyedStrictMaybe,
  field,
  invalidField,
  ofield,
  (!>),
 )
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Compactible (Compactible (..))
import Cardano.Ledger.Core
import Cardano.Ledger.Crypto (Crypto, StandardCrypto)
import Cardano.Ledger.MemoBytes (
  Mem,
  MemoBytes,
  MemoHashIndex,
  Memoized (RawType),
  getMemoRawType,
  getMemoSafeHash,
  lensMemoRawType,
  mkMemoized,
 )
import Cardano.Ledger.SafeHash (HashAnnotated (..), SafeToHash)
import Cardano.Ledger.Shelley.PParams (Update)
import Cardano.Ledger.Shelley.TxBody (
  ShelleyEraTxBody (..),
  Withdrawals (..),
 )
import Cardano.Ledger.TxIn (TxIn (..))
import Control.DeepSeq (NFData (..))
import qualified Data.Map.Strict as Map
import Data.Sequence.Strict (StrictSeq, fromList)
import Data.Set (Set, empty)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))

-- =======================================================

data AllegraTxBodyRaw ma era = AllegraTxBodyRaw
  { atbrInputs :: !(Set (TxIn (EraCrypto era)))
  , atbrOutputs :: !(StrictSeq (TxOut era))
  , atbrCerts :: !(StrictSeq (DCert era))
  , atbrWithdrawals :: !(Withdrawals (EraCrypto era))
  , atbrTxFee :: !Coin
  , atbrValidityInterval :: !ValidityInterval -- imported from Timelocks
  , atbrUpdate :: !(StrictMaybe (Update era))
  , atbrAuxDataHash :: !(StrictMaybe (AuxiliaryDataHash (EraCrypto era)))
  , atbrMint :: !ma
  }

deriving instance
  (Era era, NFData (TxOut era), NFData (DCert era), NFData (PParamsUpdate era), NFData ma) =>
  NFData (AllegraTxBodyRaw ma era)

deriving instance
  (Era era, Eq (PParamsUpdate era), Eq (TxOut era), Eq (DCert era), Eq ma) =>
  Eq (AllegraTxBodyRaw ma era)

deriving instance
  (Era era, Show (TxOut era), Show (DCert era), Show (PParamsUpdate era), Show ma) =>
  Show (AllegraTxBodyRaw ma era)

deriving instance Generic (AllegraTxBodyRaw ma era)

deriving instance
  (Era era, NoThunks (TxOut era), NoThunks (DCert era), NoThunks (PParamsUpdate era), NoThunks ma) =>
  NoThunks (AllegraTxBodyRaw ma era)

instance (DecCBOR ma, Monoid ma, AllegraEraTxBody era) => DecCBOR (AllegraTxBodyRaw ma era) where
  decCBOR =
    decode
      ( SparseKeyed
          "AllegraTxBodyRaw"
          emptyAllegraTxBodyRaw
          bodyFields
          [(0, "atbrInputs"), (1, "atbrOutputs"), (2, "atbrTxFee")]
      )

instance AllegraEraTxBody era => DecCBOR (Annotator (AllegraTxBodyRaw () era)) where
  decCBOR = pure <$> decCBOR

-- Sparse encodings of AllegraTxBodyRaw, the key values are fixed by backward compatibility
-- concerns as we want the ShelleyTxBody to deserialise as AllegraTxBody.
-- txXparse and bodyFields should be Duals, visual inspection helps ensure this.
instance
  (EraTxOut era, EraDCert era, Eq ma, EncCBOR ma, Monoid ma) =>
  EncCBOR (AllegraTxBodyRaw ma era)
  where
  encCBOR (AllegraTxBodyRaw inp out cert wdrl fee (ValidityInterval bot top) up hash frge) =
    encode $
      Keyed
        ( \i o f topx c w u h botx forg ->
            AllegraTxBodyRaw i o c w f (ValidityInterval botx topx) u h forg
        )
        !> Key 0 (To inp) -- We don't have to send these in TxBodyX order
        !> Key 1 (To out) -- Just hack up a fake constructor with the lambda.
        !> Key 2 (To fee)
        !> encodeKeyedStrictMaybe 3 top
        !> Omit null (Key 4 (To cert))
        !> Omit (null . unWithdrawals) (Key 5 (To wdrl))
        !> encodeKeyedStrictMaybe 6 up
        !> encodeKeyedStrictMaybe 7 hash
        !> encodeKeyedStrictMaybe 8 bot
        !> Omit (== mempty) (Key 9 (To frge))

bodyFields :: (DecCBOR ma, EraTxOut era, EraDCert era) => Word -> Field (AllegraTxBodyRaw ma era)
bodyFields 0 = field (\x tx -> tx {atbrInputs = x}) From
bodyFields 1 = field (\x tx -> tx {atbrOutputs = x}) From
bodyFields 2 = field (\x tx -> tx {atbrTxFee = x}) From
bodyFields 3 =
  ofield
    ( \x tx ->
        tx
          { atbrValidityInterval =
              (atbrValidityInterval tx) {invalidHereafter = x}
          }
    )
    From
bodyFields 4 = field (\x tx -> tx {atbrCerts = x}) From
bodyFields 5 = field (\x tx -> tx {atbrWithdrawals = x}) From
bodyFields 6 = ofield (\x tx -> tx {atbrUpdate = x}) From
bodyFields 7 = ofield (\x tx -> tx {atbrAuxDataHash = x}) From
bodyFields 8 =
  ofield
    ( \x tx ->
        tx
          { atbrValidityInterval =
              (atbrValidityInterval tx) {invalidBefore = x}
          }
    )
    From
bodyFields 9 = field (\x tx -> tx {atbrMint = x}) From
bodyFields n = invalidField n

emptyAllegraTxBodyRaw :: Monoid ma => AllegraTxBodyRaw ma era
emptyAllegraTxBodyRaw =
  AllegraTxBodyRaw
    empty
    (fromList [])
    (fromList [])
    (Withdrawals Map.empty)
    (Coin 0)
    (ValidityInterval SNothing SNothing)
    SNothing
    SNothing
    mempty

-- ===========================================================================
-- Wrap it all up in a newtype, hiding the insides with a pattern construtor.

newtype AllegraTxBody e = TxBodyConstr (MemoBytes (AllegraTxBodyRaw ()) e)
  deriving newtype (SafeToHash, ToCBOR)

instance Memoized AllegraTxBody where
  type RawType AllegraTxBody = AllegraTxBodyRaw ()

deriving instance
  (Era era, Eq (PParamsUpdate era), Eq (TxOut era), Eq (DCert era)) =>
  Eq (AllegraTxBody era)

deriving instance
  (Era era, Show (TxOut era), Show (DCert era), Compactible (Value era), Show (PParamsUpdate era)) =>
  Show (AllegraTxBody era)

deriving instance Generic (AllegraTxBody era)

deriving newtype instance
  (Era era, NoThunks (TxOut era), NoThunks (DCert era), NoThunks (PParamsUpdate era)) =>
  NoThunks (AllegraTxBody era)

deriving newtype instance
  ( NFData (TxOut era)
  , NFData (DCert era)
  , NFData (PParamsUpdate era)
  , Era era
  ) =>
  NFData (AllegraTxBody era)

-- | Encodes memoized bytes created upon construction.
instance Era era => EncCBOR (AllegraTxBody era)

deriving via
  Mem (AllegraTxBodyRaw ()) era
  instance
    AllegraEraTxBody era => DecCBOR (Annotator (AllegraTxBody era))

type instance MemoHashIndex (AllegraTxBodyRaw c) = EraIndependentTxBody

instance (c ~ EraCrypto era, Era era) => HashAnnotated (AllegraTxBody era) EraIndependentTxBody c where
  hashAnnotated = getMemoSafeHash

-- | A pattern to keep the newtype and the MemoBytes hidden
pattern AllegraTxBody ::
  (EraTxOut era, EraDCert era) =>
  Set (TxIn (EraCrypto era)) ->
  StrictSeq (TxOut era) ->
  StrictSeq (DCert era) ->
  Withdrawals (EraCrypto era) ->
  Coin ->
  ValidityInterval ->
  StrictMaybe (Update era) ->
  StrictMaybe (AuxiliaryDataHash (EraCrypto era)) ->
  AllegraTxBody era
pattern AllegraTxBody
  { atbInputs
  , atbOutputs
  , atbCerts
  , atbWithdrawals
  , atbTxFee
  , atbValidityInterval
  , atbUpdate
  , atbAuxDataHash
  } <-
  ( getMemoRawType ->
      AllegraTxBodyRaw
        { atbrInputs = atbInputs
        , atbrOutputs = atbOutputs
        , atbrCerts = atbCerts
        , atbrWithdrawals = atbWithdrawals
        , atbrTxFee = atbTxFee
        , atbrValidityInterval = atbValidityInterval
        , atbrUpdate = atbUpdate
        , atbrAuxDataHash = atbAuxDataHash
        }
    )
  where
    AllegraTxBody
      inputs
      outputs
      certs
      withdrawals
      txFee
      validityInterval
      update
      auxDataHash =
        mkMemoized $
          AllegraTxBodyRaw
            { atbrInputs = inputs
            , atbrOutputs = outputs
            , atbrCerts = certs
            , atbrWithdrawals = withdrawals
            , atbrTxFee = txFee
            , atbrValidityInterval = validityInterval
            , atbrUpdate = update
            , atbrAuxDataHash = auxDataHash
            , atbrMint = ()
            }

{-# COMPLETE AllegraTxBody #-}

instance Crypto c => EraTxBody (AllegraEra c) where
  {-# SPECIALIZE instance EraTxBody (AllegraEra StandardCrypto) #-}

  type TxBody (AllegraEra c) = AllegraTxBody (AllegraEra c)

  mkBasicTxBody = mkMemoized emptyAllegraTxBodyRaw

  inputsTxBodyL =
    lensMemoRawType atbrInputs $ \txBodyRaw inputs -> txBodyRaw {atbrInputs = inputs}
  {-# INLINEABLE inputsTxBodyL #-}

  outputsTxBodyL =
    lensMemoRawType atbrOutputs $ \txBodyRaw outputs -> txBodyRaw {atbrOutputs = outputs}
  {-# INLINEABLE outputsTxBodyL #-}

  feeTxBodyL =
    lensMemoRawType atbrTxFee $ \txBodyRaw fee -> txBodyRaw {atbrTxFee = fee}
  {-# INLINEABLE feeTxBodyL #-}

  auxDataHashTxBodyL =
    lensMemoRawType atbrAuxDataHash $
      \txBodyRaw auxDataHash -> txBodyRaw {atbrAuxDataHash = auxDataHash}
  {-# INLINEABLE auxDataHashTxBodyL #-}

  allInputsTxBodyF = inputsTxBodyL
  {-# INLINEABLE allInputsTxBodyF #-}

  withdrawalsTxBodyL =
    lensMemoRawType atbrWithdrawals $
      \txBodyRaw withdrawals -> txBodyRaw {atbrWithdrawals = withdrawals}
  {-# INLINEABLE withdrawalsTxBodyL #-}

  certsTxBodyL =
    lensMemoRawType atbrCerts $ \txBodyRaw certs -> txBodyRaw {atbrCerts = certs}
  {-# INLINEABLE certsTxBodyL #-}

instance Crypto c => ShelleyEraTxBody (AllegraEra c) where
  {-# SPECIALIZE instance ShelleyEraTxBody (AllegraEra StandardCrypto) #-}

  ttlTxBodyL = notSupportedInThisEraL
  {-# INLINEABLE ttlTxBodyL #-}

  updateTxBodyL =
    lensMemoRawType atbrUpdate $ \txBodyRaw update -> txBodyRaw {atbrUpdate = update}
  {-# INLINEABLE updateTxBodyL #-}

instance Crypto c => AllegraEraTxBody (AllegraEra c) where
  {-# SPECIALIZE instance AllegraEraTxBody (AllegraEra StandardCrypto) #-}

  vldtTxBodyL =
    lensMemoRawType atbrValidityInterval $
      \txBodyRaw vldt -> txBodyRaw {atbrValidityInterval = vldt}
  {-# INLINEABLE vldtTxBodyL #-}
