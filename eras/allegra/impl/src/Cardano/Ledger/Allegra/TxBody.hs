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
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Cardano.Ledger.Allegra.TxBody (
  AllegraEraTxBody (..),
  TxBody (
    MkAllegraTxBody,
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
  emptyAllegraTxBodyRaw,
  AllegraTxBodyRaw (..),
  StrictMaybe (..),
  ValidityInterval (..),
) where

import Cardano.Ledger.Allegra.Era (AllegraEra)
import Cardano.Ledger.Allegra.Scripts (ValidityInterval (..))
import Cardano.Ledger.Allegra.TxCert ()
import Cardano.Ledger.Allegra.TxOut ()
import Cardano.Ledger.BaseTypes (SlotNo, StrictMaybe (SJust, SNothing))
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..), ToCBOR)
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
import Cardano.Ledger.Core
import Cardano.Ledger.MemoBytes (
  EqRaw,
  MemoBytes,
  MemoHashIndex,
  Memoized (RawType),
  getMemoRawType,
  getMemoSafeHash,
  lensMemoRawType,
  mkMemoizedEra,
 )
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.PParams (Update (..), upgradeUpdate)
import Cardano.Ledger.Shelley.TxBody (getShelleyGenesisKeyHashCountTxBody)
import Cardano.Ledger.TxIn (TxIn (..))
import Control.DeepSeq (NFData (..))
import qualified Data.Map.Strict as Map
import Data.Sequence.Strict (StrictSeq, fromList)
import Data.Set (Set, empty)
import GHC.Generics (Generic)
import Lens.Micro
import NoThunks.Class (NoThunks (..))

class EraTxBody era => AllegraEraTxBody era where
  vldtTxBodyL :: Lens' (TxBody era) ValidityInterval

-- =======================================================

data AllegraTxBodyRaw ma era = AllegraTxBodyRaw
  { atbrInputs :: !(Set TxIn)
  , atbrOutputs :: !(StrictSeq (TxOut era))
  , atbrCerts :: !(StrictSeq (TxCert era))
  , atbrWithdrawals :: !Withdrawals
  , atbrFee :: !Coin
  , atbrValidityInterval :: !ValidityInterval
  , atbrUpdate :: !(StrictMaybe (Update era))
  , atbrAuxDataHash :: !(StrictMaybe TxAuxDataHash)
  , atbrMint :: !ma
  }

deriving instance
  (Era era, NFData (TxOut era), NFData (TxCert era), NFData (PParamsUpdate era), NFData ma) =>
  NFData (AllegraTxBodyRaw ma era)

deriving instance
  (Era era, Eq (PParamsUpdate era), Eq (TxOut era), Eq (TxCert era), Eq ma) =>
  Eq (AllegraTxBodyRaw ma era)

deriving instance
  (Era era, Show (TxOut era), Show (TxCert era), Show (PParamsUpdate era), Show ma) =>
  Show (AllegraTxBodyRaw ma era)

deriving instance Generic (AllegraTxBodyRaw ma era)

deriving instance
  (Era era, NoThunks (TxOut era), NoThunks (TxCert era), NoThunks (PParamsUpdate era), NoThunks ma) =>
  NoThunks (AllegraTxBodyRaw ma era)

instance (DecCBOR ma, Monoid ma, AllegraEraTxBody era) => DecCBOR (AllegraTxBodyRaw ma era) where
  decCBOR =
    decode
      ( SparseKeyed
          "AllegraTxBodyRaw"
          emptyAllegraTxBodyRaw
          bodyFields
          [(0, "atbrInputs"), (1, "atbrOutputs"), (2, "atbrFee")]
      )

-- Sparse encodings of AllegraTxBodyRaw, the key values are fixed by backward compatibility
-- concerns as we want the ShelleyTxBody to deserialise as AllegraTxBody.
-- txXparse and bodyFields should be Duals, visual inspection helps ensure this.
instance
  (EraTxOut era, EraTxCert era, Eq ma, EncCBOR ma, Monoid ma) =>
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

bodyFields :: (DecCBOR ma, EraTxOut era, EraTxCert era) => Word -> Field (AllegraTxBodyRaw ma era)
bodyFields 0 = field (\x tx -> tx {atbrInputs = x}) From
bodyFields 1 = field (\x tx -> tx {atbrOutputs = x}) From
bodyFields 2 = field (\x tx -> tx {atbrFee = x}) From
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

instance Memoized (TxBody AllegraEra) where
  type RawType (TxBody AllegraEra) = AllegraTxBodyRaw () AllegraEra

deriving instance Eq (TxBody AllegraEra)

deriving instance Show (TxBody AllegraEra)

deriving instance Generic (TxBody AllegraEra)

deriving newtype instance NoThunks (TxBody AllegraEra)

deriving newtype instance NFData (TxBody AllegraEra)

-- | Encodes memoized bytes created upon construction.
instance EncCBOR (TxBody AllegraEra)

type instance MemoHashIndex (AllegraTxBodyRaw c era) = EraIndependentTxBody

instance HashAnnotated (TxBody AllegraEra) EraIndependentTxBody where
  hashAnnotated = getMemoSafeHash

-- | A pattern to keep the newtype and the MemoBytes hidden
pattern AllegraTxBody ::
  (EraTxOut AllegraEra, EraTxCert AllegraEra) =>
  Set TxIn ->
  StrictSeq (TxOut AllegraEra) ->
  StrictSeq (TxCert AllegraEra) ->
  Withdrawals ->
  Coin ->
  ValidityInterval ->
  StrictMaybe (Update AllegraEra) ->
  StrictMaybe TxAuxDataHash ->
  TxBody AllegraEra
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
        , atbrFee = atbTxFee
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
        mkMemoizedEra @AllegraEra $
          AllegraTxBodyRaw
            { atbrInputs = inputs
            , atbrOutputs = outputs
            , atbrCerts = certs
            , atbrWithdrawals = withdrawals
            , atbrFee = txFee
            , atbrValidityInterval = validityInterval
            , atbrUpdate = update
            , atbrAuxDataHash = auxDataHash
            , atbrMint = ()
            }

{-# COMPLETE AllegraTxBody #-}

instance EraTxBody AllegraEra where
  newtype TxBody AllegraEra = MkAllegraTxBody (MemoBytes (AllegraTxBodyRaw () AllegraEra))
    deriving newtype (SafeToHash, ToCBOR, DecCBOR)

  mkBasicTxBody = mkMemoizedEra @AllegraEra emptyAllegraTxBodyRaw

  inputsTxBodyL =
    lensMemoRawType @AllegraEra atbrInputs $
      \txBodyRaw inputs -> txBodyRaw {atbrInputs = inputs}
  {-# INLINEABLE inputsTxBodyL #-}

  outputsTxBodyL =
    lensMemoRawType @AllegraEra atbrOutputs $
      \txBodyRaw outputs -> txBodyRaw {atbrOutputs = outputs}
  {-# INLINEABLE outputsTxBodyL #-}

  feeTxBodyL =
    lensMemoRawType @AllegraEra atbrFee $ \txBodyRaw fee -> txBodyRaw {atbrFee = fee}
  {-# INLINEABLE feeTxBodyL #-}

  auxDataHashTxBodyL =
    lensMemoRawType @AllegraEra atbrAuxDataHash $
      \txBodyRaw auxDataHash -> txBodyRaw {atbrAuxDataHash = auxDataHash}
  {-# INLINEABLE auxDataHashTxBodyL #-}

  spendableInputsTxBodyF = inputsTxBodyL
  {-# INLINE spendableInputsTxBodyF #-}

  allInputsTxBodyF = inputsTxBodyL
  {-# INLINEABLE allInputsTxBodyF #-}

  withdrawalsTxBodyL =
    lensMemoRawType @AllegraEra atbrWithdrawals $
      \txBodyRaw withdrawals -> txBodyRaw {atbrWithdrawals = withdrawals}
  {-# INLINEABLE withdrawalsTxBodyL #-}

  certsTxBodyL =
    lensMemoRawType @AllegraEra atbrCerts $
      \txBodyRaw certs -> txBodyRaw {atbrCerts = certs}
  {-# INLINEABLE certsTxBodyL #-}

  getGenesisKeyHashCountTxBody = getShelleyGenesisKeyHashCountTxBody

  upgradeTxBody txBody = do
    certs <- traverse upgradeTxCert (txBody ^. certsTxBodyL)
    pure $
      AllegraTxBody
        { atbInputs = txBody ^. inputsTxBodyL
        , atbOutputs = upgradeTxOut <$> (txBody ^. outputsTxBodyL)
        , atbCerts = certs
        , atbWithdrawals = txBody ^. withdrawalsTxBodyL
        , atbTxFee = txBody ^. feeTxBodyL
        , atbValidityInterval = ttlToValidityInterval (txBody ^. ttlTxBodyL)
        , atbUpdate = upgradeUpdate () <$> (txBody ^. updateTxBodyL)
        , atbAuxDataHash = txBody ^. auxDataHashTxBodyL
        }
    where
      ttlToValidityInterval :: SlotNo -> ValidityInterval
      ttlToValidityInterval ttl = ValidityInterval SNothing (SJust ttl)

instance ShelleyEraTxBody AllegraEra where
  ttlTxBodyL = notSupportedInThisEraL
  {-# INLINEABLE ttlTxBodyL #-}

  updateTxBodyL =
    lensMemoRawType @AllegraEra atbrUpdate $
      \txBodyRaw update -> txBodyRaw {atbrUpdate = update}
  {-# INLINEABLE updateTxBodyL #-}

instance AllegraEraTxBody AllegraEra where
  vldtTxBodyL =
    lensMemoRawType @AllegraEra atbrValidityInterval $
      \txBodyRaw vldt -> txBodyRaw {atbrValidityInterval = vldt}
  {-# INLINEABLE vldtTxBodyL #-}

instance EqRaw (TxBody AllegraEra)
