{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
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
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Mary.TxBody (
  MaryEraTxBody (..),
  MaryTxBody (
    MkMaryTxBody,
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
import Cardano.Ledger.Binary (DecCBOR (..), EncCBOR (..), ToCBOR (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Mary.Era (MaryEra)
import Cardano.Ledger.Mary.TxCert ()
import Cardano.Ledger.Mary.TxOut ()
import Cardano.Ledger.Mary.Value
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
  mintTxBodyL :: Lens' (TxBody era) MultiAsset

  mintedTxBodyF :: SimpleGetter (TxBody era) (Set PolicyID)

  mintValueTxBodyF :: SimpleGetter (TxBody era) (Value era)
  default mintValueTxBodyF :: Value era ~ MaryValue => SimpleGetter (TxBody era) (Value era)
  mintValueTxBodyF = mintTxBodyL . to (MaryValue mempty)
  {-# INLINE mintValueTxBodyF #-}

-- ===========================================================================
-- Wrap it all up in a newtype, hiding the insides with a pattern constructor.

type MaryTxBodyRaw era = AllegraTxBodyRaw MultiAsset era

newtype MaryTxBody era = MkMaryTxBody (MemoBytes (MaryTxBodyRaw era))
  deriving newtype (SafeToHash, ToCBOR)

-- | Encodes memoized bytes created upon construction.
instance Era era => EncCBOR (MaryTxBody era)

instance
  (Era era, Eq (PParamsUpdate era), Eq (TxOut era), Eq (TxCert era)) =>
  EqRaw (MaryTxBody era)

instance Memoized (MaryTxBody era) where
  type RawType (MaryTxBody era) = MaryTxBodyRaw era

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

deriving newtype instance MaryEraTxBody era => DecCBOR (MaryTxBody era)

type instance MemoHashIndex (MaryTxBodyRaw era) = EraIndependentTxBody

instance Era era => HashAnnotated (MaryTxBody era) EraIndependentTxBody where
  hashAnnotated = getMemoSafeHash

-- | A pattern to keep the newtype and the MemoBytes hidden
pattern MaryTxBody ::
  forall era.
  (EraTxOut era, EraTxCert era) =>
  Set TxIn ->
  StrictSeq (TxOut era) ->
  StrictSeq (TxCert era) ->
  Withdrawals ->
  Coin ->
  ValidityInterval ->
  StrictMaybe (Update era) ->
  StrictMaybe TxAuxDataHash ->
  MultiAsset ->
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
      AllegraTxBodyRaw
        { atbrInputs = mtbInputs
        , atbrOutputs = mtbOutputs
        , atbrCerts = mtbCerts
        , atbrWithdrawals = mtbWithdrawals
        , atbrFee = mtbTxFee
        , atbrValidityInterval = mtbValidityInterval
        , atbrUpdate = mtbUpdate
        , atbrAuxDataHash = mtbAuxDataHash
        , atbrMint = mtbMint
        }
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
        mkMemoizedEra @era $
          AllegraTxBodyRaw
            { atbrInputs = inputs
            , atbrOutputs = outputs
            , atbrCerts = certs
            , atbrWithdrawals = withdrawals
            , atbrFee = txFee
            , atbrValidityInterval = validityInterval
            , atbrUpdate = update
            , atbrAuxDataHash = auxDataHash
            , atbrMint = mint
            }

{-# COMPLETE MaryTxBody #-}

instance EraTxBody MaryEra where
  type TxBody MaryEra = MaryTxBody MaryEra

  mkBasicTxBody = mkMemoizedEra @MaryEra emptyAllegraTxBodyRaw

  inputsTxBodyL =
    lensMemoRawType @MaryEra atbrInputs $ \txBodyRaw inputs -> txBodyRaw {atbrInputs = inputs}
  {-# INLINEABLE inputsTxBodyL #-}

  outputsTxBodyL =
    lensMemoRawType @MaryEra atbrOutputs $ \txBodyRaw outputs -> txBodyRaw {atbrOutputs = outputs}
  {-# INLINEABLE outputsTxBodyL #-}

  feeTxBodyL =
    lensMemoRawType @MaryEra atbrFee $ \txBodyRaw fee -> txBodyRaw {atbrFee = fee}
  {-# INLINEABLE feeTxBodyL #-}

  auxDataHashTxBodyL =
    lensMemoRawType @MaryEra atbrAuxDataHash $
      \txBodyRaw auxDataHash -> txBodyRaw {atbrAuxDataHash = auxDataHash}
  {-# INLINEABLE auxDataHashTxBodyL #-}

  spendableInputsTxBodyF = inputsTxBodyL
  {-# INLINE spendableInputsTxBodyF #-}

  allInputsTxBodyF = inputsTxBodyL
  {-# INLINEABLE allInputsTxBodyF #-}

  withdrawalsTxBodyL =
    lensMemoRawType @MaryEra atbrWithdrawals $ \txBodyRaw withdrawals -> txBodyRaw {atbrWithdrawals = withdrawals}
  {-# INLINEABLE withdrawalsTxBodyL #-}

  certsTxBodyL =
    lensMemoRawType @MaryEra atbrCerts $ \txBodyRaw certs -> txBodyRaw {atbrCerts = certs}
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

instance ShelleyEraTxBody MaryEra where
  ttlTxBodyL = notSupportedInThisEraL
  {-# INLINEABLE ttlTxBodyL #-}

  updateTxBodyL =
    lensMemoRawType @MaryEra atbrUpdate $ \txBodyRaw update -> txBodyRaw {atbrUpdate = update}
  {-# INLINEABLE updateTxBodyL #-}

instance AllegraEraTxBody MaryEra where
  vldtTxBodyL =
    lensMemoRawType @MaryEra atbrValidityInterval $
      \txBodyRaw vldt -> txBodyRaw {atbrValidityInterval = vldt}
  {-# INLINEABLE vldtTxBodyL #-}

instance MaryEraTxBody MaryEra where
  mintTxBodyL =
    lensMemoRawType @MaryEra atbrMint (\txBodyRaw mint -> txBodyRaw {atbrMint = mint})
  {-# INLINEABLE mintTxBodyL #-}

  mintedTxBodyF = to $ \txBody -> policies (atbrMint (getMemoRawType txBody))
  {-# INLINEABLE mintedTxBodyF #-}
