{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Mary.TxBody (
  MaryTxBody (
    MaryTxBody,
    mtbAuxDataHash,
    mtbCerts,
    mtbInputs,
    mtbOutputs,
    mtbTxFee,
    mtbUpdate,
    mtbValidityInterval,
    mtbWithdrawals,
    mtbMint
  ),
  MaryEraTxBody (..),
  StrictMaybe (..),
  ValidityInterval (..),
)
where

import Cardano.Ledger.Allegra.TxBody
import Cardano.Ledger.AuxiliaryData (AuxiliaryDataHash)
import Cardano.Ledger.Binary (Annotator, DecCBOR (..), EncCBOR (..), ToCBOR (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Crypto (Crypto, StandardCrypto)
import Cardano.Ledger.Mary.Core
import Cardano.Ledger.Mary.Delegation ()
import Cardano.Ledger.Mary.Era (MaryEra)
import Cardano.Ledger.Mary.TxOut ()
import Cardano.Ledger.Mary.Value
import Cardano.Ledger.MemoBytes (
  Mem,
  MemoBytes (Memo),
  MemoHashIndex,
  Memoized (RawType),
  getMemoRawType,
  getMemoSafeHash,
  lensMemoRawType,
  mkMemoized,
 )
import Cardano.Ledger.SafeHash (HashAnnotated (..), SafeToHash)
import Cardano.Ledger.Shelley.PParams (Update)
import Cardano.Ledger.TxIn (TxIn (..))
import Control.DeepSeq (NFData (..))
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Set as Set (Set, map)
import GHC.Generics (Generic)
import Lens.Micro
import NoThunks.Class (NoThunks (..))

-- ===========================================================================
-- Wrap it all up in a newtype, hiding the insides with a pattern constructor.

newtype MaryTxBodyRaw era = MaryTxBodyRaw (AllegraTxBodyRaw (MultiAsset (EraCrypto era)) era)

deriving newtype instance
  (Era era, NFData (TxOut era), NFData (DCert era), NFData (PParamsUpdate era)) =>
  NFData (MaryTxBodyRaw era)

deriving newtype instance
  (Era era, Eq (TxOut era), Eq (DCert era), Eq (PParamsUpdate era)) =>
  Eq (MaryTxBodyRaw era)

deriving newtype instance
  (Era era, Show (TxOut era), Show (DCert era), Show (PParamsUpdate era)) =>
  Show (MaryTxBodyRaw era)

deriving instance Generic (MaryTxBodyRaw era)

deriving newtype instance
  (Era era, NoThunks (TxOut era), NoThunks (DCert era), NoThunks (PParamsUpdate era)) =>
  NoThunks (MaryTxBodyRaw era)

deriving newtype instance AllegraEraTxBody era => DecCBOR (MaryTxBodyRaw era)

newtype MaryTxBody era = TxBodyConstr (MemoBytes MaryTxBodyRaw era)
  deriving newtype (SafeToHash, ToCBOR)

-- | Encodes memoized bytes created upon construction.
instance Era era => EncCBOR (MaryTxBody era)

instance AllegraEraTxBody era => DecCBOR (Annotator (MaryTxBodyRaw era)) where
  decCBOR = pure <$> decCBOR

deriving newtype instance (EraTxOut era, EraDCert era) => EncCBOR (MaryTxBodyRaw era)

instance Memoized MaryTxBody where
  type RawType MaryTxBody = MaryTxBodyRaw

deriving newtype instance
  (Era era, Eq (TxOut era), Eq (DCert era), Eq (PParamsUpdate era)) =>
  Eq (MaryTxBody era)

deriving newtype instance
  (Era era, Show (TxOut era), Show (DCert era), Show (PParamsUpdate era)) =>
  Show (MaryTxBody era)

deriving instance Generic (MaryTxBody era)

deriving newtype instance
  (Era era, NoThunks (TxOut era), NoThunks (DCert era), NoThunks (PParamsUpdate era)) =>
  NoThunks (MaryTxBody era)

deriving newtype instance
  (Era era, NFData (TxOut era), NFData (DCert era), NFData (PParamsUpdate era)) =>
  NFData (MaryTxBody era)

deriving via
  Mem MaryTxBodyRaw era
  instance
    MaryEraTxBody era => DecCBOR (Annotator (MaryTxBody era))

type instance MemoHashIndex MaryTxBodyRaw = EraIndependentTxBody

instance (c ~ EraCrypto era, Era era) => HashAnnotated (MaryTxBody era) EraIndependentTxBody c where
  hashAnnotated = getMemoSafeHash

-- | A pattern to keep the newtype and the MemoBytes hidden
pattern MaryTxBody ::
  (EraTxOut era, EraDCert era) =>
  Set.Set (TxIn (EraCrypto era)) ->
  StrictSeq (TxOut era) ->
  StrictSeq (DCert era) ->
  Withdrawals (EraCrypto era) ->
  Coin ->
  ValidityInterval ->
  StrictMaybe (Update era) ->
  StrictMaybe (AuxiliaryDataHash (EraCrypto era)) ->
  MultiAsset (EraCrypto era) ->
  MaryTxBody era
pattern MaryTxBody
  { mtbInputs
  , mtbOutputs
  , mtbCerts
  , mtbWithdrawals
  , mtbTxFee
  , mtbValidityInterval
  , mtbUpdate
  , mtbAuxDataHash
  , mtbMint
  } <-
  ( getMemoRawType ->
      MaryTxBodyRaw
        ( AllegraTxBodyRaw
            { atbrInputs = mtbInputs
            , atbrOutputs = mtbOutputs
            , atbrCerts = mtbCerts
            , atbrWithdrawals = mtbWithdrawals
            , atbrTxFee = mtbTxFee
            , atbrValidityInterval = mtbValidityInterval
            , atbrUpdate = mtbUpdate
            , atbrAuxDataHash = mtbAuxDataHash
            , atbrMint = mtbMint
            }
          )
    )
  where
    MaryTxBody
      inputs
      outputs
      certs
      withdrawals
      txFee
      validityInterval
      update
      auxDataHash
      mint =
        mkMemoized $
          MaryTxBodyRaw $
            AllegraTxBodyRaw
              { atbrInputs = inputs
              , atbrOutputs = outputs
              , atbrCerts = certs
              , atbrWithdrawals = withdrawals
              , atbrTxFee = txFee
              , atbrValidityInterval = validityInterval
              , atbrUpdate = update
              , atbrAuxDataHash = auxDataHash
              , atbrMint = mint
              }

{-# COMPLETE MaryTxBody #-}

-- | This is a helper Lens creator for any Memoized type.
lensMaryTxBodyRaw ::
  (EraTxOut era, EraDCert era) =>
  (AllegraTxBodyRaw (MultiAsset (EraCrypto era)) era -> a) ->
  ( AllegraTxBodyRaw (MultiAsset (EraCrypto era)) era ->
    b ->
    AllegraTxBodyRaw (MultiAsset (EraCrypto era)) era
  ) ->
  Lens (MaryTxBody era) (MaryTxBody era) a b
lensMaryTxBodyRaw getter setter =
  lensMemoRawType
    (\(MaryTxBodyRaw atbr) -> getter atbr)
    (\(MaryTxBodyRaw atbr) a -> MaryTxBodyRaw (setter atbr a))
{-# INLINEABLE lensMaryTxBodyRaw #-}

instance Crypto c => EraTxBody (MaryEra c) where
  {-# SPECIALIZE instance EraTxBody (MaryEra StandardCrypto) #-}

  type TxBody (MaryEra c) = MaryTxBody (MaryEra c)

  mkBasicTxBody = mkMemoized $ MaryTxBodyRaw emptyAllegraTxBodyRaw

  inputsTxBodyL =
    lensMaryTxBodyRaw atbrInputs $ \txBodyRaw inputs -> txBodyRaw {atbrInputs = inputs}
  {-# INLINEABLE inputsTxBodyL #-}

  outputsTxBodyL =
    lensMaryTxBodyRaw atbrOutputs $ \txBodyRaw outputs -> txBodyRaw {atbrOutputs = outputs}
  {-# INLINEABLE outputsTxBodyL #-}

  feeTxBodyL =
    lensMaryTxBodyRaw atbrTxFee $ \txBodyRaw fee -> txBodyRaw {atbrTxFee = fee}
  {-# INLINEABLE feeTxBodyL #-}

  auxDataHashTxBodyL =
    lensMaryTxBodyRaw atbrAuxDataHash $
      \txBodyRaw auxDataHash -> txBodyRaw {atbrAuxDataHash = auxDataHash}
  {-# INLINEABLE auxDataHashTxBodyL #-}

  allInputsTxBodyF = inputsTxBodyL
  {-# INLINEABLE allInputsTxBodyF #-}

  withdrawalsTxBodyL =
    lensMaryTxBodyRaw atbrWithdrawals $ \txBodyRaw withdrawals -> txBodyRaw {atbrWithdrawals = withdrawals}
  {-# INLINEABLE withdrawalsTxBodyL #-}

  certsTxBodyL =
    lensMaryTxBodyRaw atbrCerts $ \txBodyRaw certs -> txBodyRaw {atbrCerts = certs}
  {-# INLINEABLE certsTxBodyL #-}

instance Crypto c => ShelleyEraTxBody (MaryEra c) where
  {-# SPECIALIZE instance ShelleyEraTxBody (MaryEra StandardCrypto) #-}

  ttlTxBodyL = notSupportedInThisEraL
  {-# INLINEABLE ttlTxBodyL #-}

  updateTxBodyL =
    lensMaryTxBodyRaw atbrUpdate $ \txBodyRaw update -> txBodyRaw {atbrUpdate = update}
  {-# INLINEABLE updateTxBodyL #-}

instance Crypto c => AllegraEraTxBody (MaryEra c) where
  {-# SPECIALIZE instance AllegraEraTxBody (MaryEra StandardCrypto) #-}

  vldtTxBodyL =
    lensMaryTxBodyRaw atbrValidityInterval $
      \txBodyRaw vldt -> txBodyRaw {atbrValidityInterval = vldt}
  {-# INLINEABLE vldtTxBodyL #-}

instance Crypto c => MaryEraTxBody (MaryEra c) where
  {-# SPECIALIZE instance MaryEraTxBody (MaryEra StandardCrypto) #-}

  mintTxBodyL =
    lensMaryTxBodyRaw atbrMint (\txBodyRaw mint -> txBodyRaw {atbrMint = mint})
  {-# INLINEABLE mintTxBodyL #-}

  mintValueTxBodyF =
    to $ \(TxBodyConstr (Memo (MaryTxBodyRaw txBodyRaw) _)) ->
      MaryValue 0 (atbrMint txBodyRaw)
  {-# INLINEABLE mintValueTxBodyF #-}

  mintedTxBodyF =
    to $ \(TxBodyConstr (Memo (MaryTxBodyRaw txBodyRaw) _)) ->
      Set.map policyID (policies (atbrMint txBodyRaw))
  {-# INLINEABLE mintedTxBodyF #-}
