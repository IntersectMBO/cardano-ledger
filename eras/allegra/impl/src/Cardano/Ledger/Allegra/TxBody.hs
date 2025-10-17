{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

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
  basicAllegraTxBody,
  emptyAllegraTxBodyRaw,
  AllegraTxBodyRaw (..),
  StrictMaybe (..),
  ValidityInterval (..),
) where

import Cardano.Ledger.Allegra.Era (AllegraEra)
import Cardano.Ledger.Allegra.Scripts (ValidityInterval (..))
import Cardano.Ledger.Allegra.TxCert ()
import Cardano.Ledger.Allegra.TxOut ()
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
import Cardano.Ledger.Core
import Cardano.Ledger.MemoBytes (
  EqRaw,
  Mem,
  MemoBytes,
  MemoHashIndex,
  Memoized (RawType),
  getMemoRawType,
  getMemoSafeHash,
  lensMemoRawType,
  mkMemoizedEra,
 )
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.PParams (Update (..))
import Cardano.Ledger.Shelley.TxBody (getShelleyGenesisKeyHashCountTxBody)
import Cardano.Ledger.TxIn (TxIn (..))
import Control.DeepSeq (NFData (..), deepseq)
import qualified Data.Map.Strict as Map
import Data.Sequence.Strict (StrictSeq, fromList)
import Data.Set (Set, empty)
import Data.Typeable
import GHC.Generics (Generic)
import Lens.Micro
import NoThunks.Class (InspectHeap (..), NoThunks (..))

class EraTxBody era => AllegraEraTxBody era where
  vldtTxBodyL :: Lens' (TxBody l era) ValidityInterval

-- =======================================================

data AllegraTxBodyRaw ma l era where
  AllegraTxBodyRaw ::
    { atbrInputs :: !(Set TxIn)
    , atbrOutputs :: !(StrictSeq (TxOut era))
    , atbrCerts :: !(StrictSeq (TxCert era))
    , atbrWithdrawals :: !Withdrawals
    , atbrFee :: !Coin
    , atbrValidityInterval :: !ValidityInterval
    , atbrUpdate :: !(StrictMaybe (Update era))
    , atbrAuxDataHash :: !(StrictMaybe TxAuxDataHash)
    , atbrMint :: !ma
    } ->
    AllegraTxBodyRaw ma TopTx era

deriving instance
  (Era era, Eq (PParamsUpdate era), Eq (TxOut era), Eq (TxCert era), Eq ma) =>
  Eq (AllegraTxBodyRaw ma l era)

deriving instance
  (Era era, Show (TxOut era), Show (TxCert era), Show (PParamsUpdate era), Show ma) =>
  Show (AllegraTxBodyRaw ma l era)

instance
  (Era era, NFData (TxOut era), NFData (TxCert era), NFData (PParamsUpdate era), NFData ma) =>
  NFData (AllegraTxBodyRaw ma l era)
  where
  rnf AllegraTxBodyRaw {..} =
    atbrInputs `deepseq`
      atbrOutputs `deepseq`
        atbrCerts `deepseq`
          atbrWithdrawals `deepseq`
            atbrFee `deepseq`
              atbrValidityInterval `deepseq`
                atbrUpdate `deepseq`
                  atbrAuxDataHash `deepseq`
                    rnf atbrMint

deriving via
  InspectHeap (AllegraTxBodyRaw ma l era)
  instance
    (Typeable era, Typeable ma, Typeable l) => NoThunks (AllegraTxBodyRaw ma l era)

instance
  ( DecCBOR ma
  , Monoid ma
  , AllegraEraTxBody era
  , HasEraTxLevel (AllegraTxBodyRaw ma) era
  , STxLevel l era ~ STxTopLevel l era
  , Typeable l
  ) =>
  DecCBOR (AllegraTxBodyRaw ma l era)
  where
  decCBOR =
    mkSTxTopLevelM @l $
      decode $
        SparseKeyed
          "AllegraTxBodyRaw"
          emptyAllegraTxBodyRaw
          bodyFields
          [(0, "atbrInputs"), (1, "atbrOutputs"), (2, "atbrFee")]

instance
  ( DecCBOR ma
  , Monoid ma
  , AllegraEraTxBody era
  , HasEraTxLevel (AllegraTxBodyRaw ma) era
  , STxLevel l era ~ STxTopLevel l era
  , Typeable l
  ) =>
  DecCBOR (Annotator (AllegraTxBodyRaw ma l era))
  where
  decCBOR = pure <$> decCBOR

instance HasEraTxLevel (AllegraTxBodyRaw m) AllegraEra where
  toSTxLevel AllegraTxBodyRaw {} = STopTxOnly @AllegraEra

-- Sparse encodings of AllegraTxBodyRaw, the key values are fixed by backward compatibility
-- concerns as we want the ShelleyTxBody to deserialise as AllegraTxBody.
-- txXparse and bodyFields should be Duals, visual inspection helps ensure this.
instance
  (EraTxOut era, EraTxCert era, Eq ma, EncCBOR ma, Monoid ma) =>
  EncCBOR (AllegraTxBodyRaw ma l era)
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

bodyFields ::
  (DecCBOR ma, EraTxOut era, EraTxCert era) => Word -> Field (AllegraTxBodyRaw ma TopTx era)
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

basicAllegraTxBody ::
  forall era l ma.
  ( EraTxBody era
  , Memoized (TxBody l era)
  , RawType (TxBody l era) ~ AllegraTxBodyRaw ma l era
  , HasEraTxLevel (AllegraTxBodyRaw ma) era
  , STxLevel l era ~ STxTopLevel l era
  , Typeable l
  , EncCBOR ma
  , Monoid ma
  , Eq ma
  ) =>
  TxBody l era
basicAllegraTxBody =
  mkMemoizedEra @era $ asSTxTopLevel (emptyAllegraTxBodyRaw @ma @era)

emptyAllegraTxBodyRaw :: Monoid ma => AllegraTxBodyRaw ma TopTx era
emptyAllegraTxBodyRaw =
  AllegraTxBodyRaw
    { atbrInputs = empty
    , atbrOutputs = fromList []
    , atbrCerts = fromList []
    , atbrWithdrawals = Withdrawals Map.empty
    , atbrFee = Coin 0
    , atbrValidityInterval = ValidityInterval SNothing SNothing
    , atbrUpdate = SNothing
    , atbrAuxDataHash = SNothing
    , atbrMint = mempty
    }

instance Memoized (TxBody l AllegraEra) where
  type RawType (TxBody l AllegraEra) = AllegraTxBodyRaw () l AllegraEra

deriving via
  Mem (AllegraTxBodyRaw () l AllegraEra)
  instance
    Typeable l => DecCBOR (Annotator (TxBody l AllegraEra))

deriving instance Eq (TxBody l AllegraEra)

deriving instance Show (TxBody l AllegraEra)

deriving instance Generic (TxBody l AllegraEra)

type instance MemoHashIndex (AllegraTxBodyRaw c l era) = EraIndependentTxBody

instance HashAnnotated (TxBody l AllegraEra) EraIndependentTxBody where
  hashAnnotated = getMemoSafeHash

instance HasEraTxLevel TxBody AllegraEra where
  toSTxLevel = toSTxLevel . getMemoRawType

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
  TxBody TopTx AllegraEra
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
  newtype TxBody l AllegraEra = MkAllegraTxBody (MemoBytes (AllegraTxBodyRaw () l AllegraEra))
    deriving newtype (SafeToHash, ToCBOR, EncCBOR, NoThunks, NFData)

  mkBasicTxBody = basicAllegraTxBody @AllegraEra

  inputsTxBodyL =
    lensMemoRawType @AllegraEra (\AllegraTxBodyRaw {atbrInputs} -> atbrInputs) $
      \txBodyRaw@AllegraTxBodyRaw {} inputs -> txBodyRaw {atbrInputs = inputs}
  {-# INLINEABLE inputsTxBodyL #-}

  outputsTxBodyL =
    lensMemoRawType @AllegraEra (\AllegraTxBodyRaw {atbrOutputs} -> atbrOutputs) $
      \txBodyRaw@AllegraTxBodyRaw {} outputs -> txBodyRaw {atbrOutputs = outputs}
  {-# INLINEABLE outputsTxBodyL #-}

  feeTxBodyL =
    lensMemoRawType @AllegraEra atbrFee $ \txBodyRaw fee -> txBodyRaw {atbrFee = fee}
  {-# INLINEABLE feeTxBodyL #-}

  auxDataHashTxBodyL =
    lensMemoRawType @AllegraEra (\AllegraTxBodyRaw {atbrAuxDataHash} -> atbrAuxDataHash) $
      \txBodyRaw@AllegraTxBodyRaw {} auxDataHash -> txBodyRaw {atbrAuxDataHash = auxDataHash}
  {-# INLINEABLE auxDataHashTxBodyL #-}

  spendableInputsTxBodyF = inputsTxBodyL
  {-# INLINE spendableInputsTxBodyF #-}

  allInputsTxBodyF = inputsTxBodyL
  {-# INLINEABLE allInputsTxBodyF #-}

  withdrawalsTxBodyL =
    lensMemoRawType @AllegraEra (\AllegraTxBodyRaw {atbrWithdrawals} -> atbrWithdrawals) $
      \txBodyRaw@AllegraTxBodyRaw {} withdrawals -> txBodyRaw {atbrWithdrawals = withdrawals}
  {-# INLINEABLE withdrawalsTxBodyL #-}

  certsTxBodyL =
    lensMemoRawType @AllegraEra (\AllegraTxBodyRaw {atbrCerts} -> atbrCerts) $
      \txBodyRaw@AllegraTxBodyRaw {} certs -> txBodyRaw {atbrCerts = certs}
  {-# INLINEABLE certsTxBodyL #-}

  getGenesisKeyHashCountTxBody = getShelleyGenesisKeyHashCountTxBody

instance ShelleyEraTxBody AllegraEra where
  ttlTxBodyL = notSupportedInThisEraL
  {-# INLINEABLE ttlTxBodyL #-}

  updateTxBodyL =
    lensMemoRawType @AllegraEra atbrUpdate $
      \txBodyRaw update -> txBodyRaw {atbrUpdate = update}
  {-# INLINEABLE updateTxBodyL #-}

instance AllegraEraTxBody AllegraEra where
  vldtTxBodyL =
    lensMemoRawType @AllegraEra (\AllegraTxBodyRaw {atbrValidityInterval} -> atbrValidityInterval) $
      \txBodyRaw@AllegraTxBodyRaw {} vldt -> txBodyRaw {atbrValidityInterval = vldt}
  {-# INLINEABLE vldtTxBodyL #-}

instance EqRaw (TxBody l AllegraEra)
