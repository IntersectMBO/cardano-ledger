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
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_HADDOCK not-home #-}

-- | Provides Mary TxBody internals
--
-- = Warning
--
-- This module is considered __internal__.
--
-- The contents of this module may change __in any way whatsoever__
-- and __without any warning__ between minor versions of this package.
module Cardano.Ledger.Mary.TxBody.Internal (
  MaryEraTxBody (..),
  MaryTxBody (
    ..,
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
  MaryTxBodyRaw (..),
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
  MemoBytes (Memo),
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

  mintValueTxBodyF :: SimpleGetter (TxBody era) (Value era)

  mintedTxBodyF :: SimpleGetter (TxBody era) (Set PolicyID)

-- ===========================================================================
-- Wrap it all up in a newtype, hiding the insides with a pattern constructor.

newtype MaryTxBodyRaw era = MaryTxBodyRaw (AllegraTxBodyRaw MultiAsset era)

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

newtype MaryTxBody era = TxBodyConstr (MemoBytes (MaryTxBodyRaw era))
  deriving newtype (SafeToHash, ToCBOR)

-- | Encodes memoized bytes created upon construction.
instance Era era => EncCBOR (MaryTxBody era)

instance
  (Era era, Eq (PParamsUpdate era), Eq (TxOut era), Eq (TxCert era)) =>
  EqRaw (MaryTxBody era)

deriving newtype instance (EraTxOut era, EraTxCert era) => EncCBOR (MaryTxBodyRaw era)

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
        mkMemoizedEra @era $
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
  forall era a b.
  (EraTxOut era, EraTxCert era) =>
  (AllegraTxBodyRaw MultiAsset era -> a) ->
  (AllegraTxBodyRaw MultiAsset era -> b -> AllegraTxBodyRaw MultiAsset era) ->
  Lens (MaryTxBody era) (MaryTxBody era) a b
lensMaryTxBodyRaw getter setter =
  lensMemoRawType @era (\(MaryTxBodyRaw atbr) -> getter atbr) $
    \(MaryTxBodyRaw atbr) a -> MaryTxBodyRaw (setter atbr a)
{-# INLINEABLE lensMaryTxBodyRaw #-}

instance EraTxBody MaryEra where
  type TxBody MaryEra = MaryTxBody MaryEra

  mkBasicTxBody = mkMemoizedEra @MaryEra $ MaryTxBodyRaw emptyAllegraTxBodyRaw

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

instance ShelleyEraTxBody MaryEra where
  ttlTxBodyL = notSupportedInThisEraL
  {-# INLINEABLE ttlTxBodyL #-}

  updateTxBodyL =
    lensMaryTxBodyRaw atbrUpdate $ \txBodyRaw update -> txBodyRaw {atbrUpdate = update}
  {-# INLINEABLE updateTxBodyL #-}

instance AllegraEraTxBody MaryEra where
  vldtTxBodyL =
    lensMaryTxBodyRaw atbrValidityInterval $
      \txBodyRaw vldt -> txBodyRaw {atbrValidityInterval = vldt}
  {-# INLINEABLE vldtTxBodyL #-}

instance MaryEraTxBody MaryEra where
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
