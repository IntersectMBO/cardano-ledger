{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Mary.TxBody (
  MaryEraTxBody (..),
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
  MaryTxBodyRaw,
)
where

import Cardano.Ledger.Allegra.Core
import Cardano.Ledger.Allegra.TxBody
import Cardano.Ledger.AuxiliaryData (AuxiliaryDataHash)
import Cardano.Ledger.Binary (Annotator, DecCBOR (..), EncCBOR (..), ToCBOR (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Crypto (Crypto, StandardCrypto)
import Cardano.Ledger.Mary.Era (MaryEra)
import Cardano.Ledger.Mary.TxCert ()
import Cardano.Ledger.Mary.TxOut ()
import Cardano.Ledger.Mary.Value
import Cardano.Ledger.MemoBytes (
  EqRaw,
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
import Cardano.Ledger.Shelley.PParams (Update, upgradeUpdate)
import Cardano.Ledger.Shelley.TxBody (getShelleyGenesisKeyHashCountTxBody)
import Cardano.Ledger.TxIn (TxIn (..))
import Control.DeepSeq (NFData (..))
import Data.Sequence.Strict (StrictSeq)
import Data.Set (Set)
import GHC.Generics (Generic)
import Lens.Micro
import NoThunks.Class (NoThunks (..))

class AllegraEraTxBody era => MaryEraTxBody era where
  mintTxBodyL :: Lens' (TxBody era) (MultiAsset (EraCrypto era))

  mintValueTxBodyF :: SimpleGetter (TxBody era) (Value era)

  mintedTxBodyF :: SimpleGetter (TxBody era) (Set (PolicyID (EraCrypto era)))

-- ===========================================================================
-- Wrap it all up in a newtype, hiding the insides with a pattern constructor.

newtype MaryTxBodyRaw era = MaryTxBodyRaw (AllegraTxBodyRaw (MultiAsset (EraCrypto era)) era)

deriving newtype instance
  (Era era, NFData (TxOut era), NFData (TxCert era), NFData (PParamsUpdate era)) =>
  NFData (MaryTxBodyRaw era)

deriving newtype instance
  (Era era, Eq (TxOut era), Eq (TxCert era), Eq (PParamsUpdate era)) =>
  Eq (MaryTxBodyRaw era)

deriving newtype instance
  (Era era, Show (TxOut era), Show (TxCert era), Show (PParamsUpdate era)) =>
  Show (MaryTxBodyRaw era)

deriving instance Generic (MaryTxBodyRaw era)

deriving newtype instance
  (Era era, NoThunks (TxOut era), NoThunks (TxCert era), NoThunks (PParamsUpdate era)) =>
  NoThunks (MaryTxBodyRaw era)

deriving newtype instance AllegraEraTxBody era => DecCBOR (MaryTxBodyRaw era)

newtype MaryTxBody era = TxBodyConstr (MemoBytes MaryTxBodyRaw era)
  deriving newtype (SafeToHash, ToCBOR)

-- | Encodes memoized bytes created upon construction.
instance Era era => EncCBOR (MaryTxBody era)

instance
  (Era era, Eq (PParamsUpdate era), Eq (TxOut era), Eq (TxCert era)) =>
  EqRaw (MaryTxBody era)

instance AllegraEraTxBody era => DecCBOR (Annotator (MaryTxBodyRaw era)) where
  decCBOR = pure <$> decCBOR

deriving newtype instance (EraTxOut era, EraTxCert era) => EncCBOR (MaryTxBodyRaw era)

instance Memoized MaryTxBody where
  type RawType MaryTxBody = MaryTxBodyRaw

deriving newtype instance
  (Era era, Eq (TxOut era), Eq (TxCert era), Eq (PParamsUpdate era)) =>
  Eq (MaryTxBody era)

deriving newtype instance
  (Era era, Show (TxOut era), Show (TxCert era), Show (PParamsUpdate era)) =>
  Show (MaryTxBody era)

deriving instance Generic (MaryTxBody era)

deriving newtype instance
  (Era era, NoThunks (TxOut era), NoThunks (TxCert era), NoThunks (PParamsUpdate era)) =>
  NoThunks (MaryTxBody era)

deriving newtype instance
  (Era era, NFData (TxOut era), NFData (TxCert era), NFData (PParamsUpdate era)) =>
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
  (EraTxOut era, EraTxCert era) =>
  Set (TxIn (EraCrypto era)) ->
  StrictSeq (TxOut era) ->
  StrictSeq (TxCert era) ->
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
  (EraTxOut era, EraTxCert era) =>
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

  spendableInputsTxBodyF = inputsTxBodyL
  {-# INLINE spendableInputsTxBodyF #-}

  allInputsTxBodyF = inputsTxBodyL
  {-# INLINEABLE allInputsTxBodyF #-}

  withdrawalsTxBodyL =
    lensMaryTxBodyRaw atbrWithdrawals $ \txBodyRaw withdrawals -> txBodyRaw {atbrWithdrawals = withdrawals}
  {-# INLINEABLE withdrawalsTxBodyL #-}

  certsTxBodyL =
    lensMaryTxBodyRaw atbrCerts $ \txBodyRaw certs -> txBodyRaw {atbrCerts = certs}
  {-# INLINEABLE certsTxBodyL #-}

  getGenesisKeyHashCountTxBody = getShelleyGenesisKeyHashCountTxBody

  upgradeTxBody atb = do
    certs <- traverse upgradeTxCert (atbCerts atb)
    pure $
      MaryTxBody
        { mtbInputs = atbInputs atb
        , mtbOutputs = upgradeTxOut <$> atbOutputs atb
        , mtbCerts = certs
        , mtbWithdrawals = atbWithdrawals atb
        , mtbTxFee = atbTxFee atb
        , mtbValidityInterval = atbValidityInterval atb
        , mtbUpdate = upgradeUpdate () <$> atbUpdate atb
        , mtbAuxDataHash = atbAuxDataHash atb
        , mtbMint = mempty
        }

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
      MaryValue mempty (atbrMint txBodyRaw)
  {-# INLINEABLE mintValueTxBodyF #-}

  mintedTxBodyF =
    to $ \(TxBodyConstr (Memo (MaryTxBodyRaw txBodyRaw) _)) -> policies (atbrMint txBodyRaw)
  {-# INLINEABLE mintedTxBodyF #-}
