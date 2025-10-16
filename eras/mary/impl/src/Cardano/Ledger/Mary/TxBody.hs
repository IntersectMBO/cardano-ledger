{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
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

module Cardano.Ledger.Mary.TxBody (
  MaryEraTxBody (..),
  TxBody (
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
) where

import Cardano.Ledger.Allegra.Core
import Cardano.Ledger.Allegra.TxBody
import Cardano.Ledger.Binary (Annotator, DecCBOR (..), EncCBOR (..), ToCBOR (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Mary.Era (MaryEra)
import Cardano.Ledger.Mary.TxCert ()
import Cardano.Ledger.Mary.TxOut ()
import Cardano.Ledger.Mary.Value
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
import Cardano.Ledger.Shelley.PParams (Update)
import Cardano.Ledger.Shelley.TxBody (getShelleyGenesisKeyHashCountTxBody)
import Cardano.Ledger.TxIn (TxIn (..))
import Control.DeepSeq (NFData (..))
import Data.Sequence.Strict (StrictSeq)
import Data.Set (Set)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Lens.Micro
import NoThunks.Class (NoThunks (..))

class AllegraEraTxBody era => MaryEraTxBody era where
  mintTxBodyL :: Lens' (TxBody l era) MultiAsset

  -- TODO: extract away from this type class into a standalone getter
  mintedTxBodyF :: SimpleGetter (TxBody l era) (Set PolicyID)
  mintedTxBodyF = mintTxBodyL . to policies
  {-# INLINE mintedTxBodyF #-}

  mintValueTxBodyF :: SimpleGetter (TxBody l era) (Value era)
  default mintValueTxBodyF :: Value era ~ MaryValue => SimpleGetter (TxBody l era) (Value era)
  mintValueTxBodyF = mintTxBodyL . to (MaryValue mempty)
  {-# INLINE mintValueTxBodyF #-}

-- ===========================================================================
-- Wrap it all up in a newtype, hiding the insides with a pattern constructor.

type MaryTxBodyRaw l = AllegraTxBodyRaw MultiAsset l MaryEra

instance EqRaw (TxBody l MaryEra)

deriving instance Generic (TxBody l MaryEra)

deriving via
  Mem (MaryTxBodyRaw l)
  instance
    Typeable l => DecCBOR (Annotator (TxBody l MaryEra))

type instance MemoHashIndex (MaryTxBodyRaw l) = EraIndependentTxBody

instance HashAnnotated (TxBody l MaryEra) EraIndependentTxBody where
  hashAnnotated = getMemoSafeHash

-- | A pattern to keep the newtype and the MemoBytes hidden
pattern MaryTxBody ::
  (EraTxOut MaryEra, EraTxCert MaryEra) =>
  Set TxIn ->
  StrictSeq (TxOut MaryEra) ->
  StrictSeq (TxCert MaryEra) ->
  Withdrawals ->
  Coin ->
  ValidityInterval ->
  StrictMaybe (Update MaryEra) ->
  StrictMaybe TxAuxDataHash ->
  MultiAsset ->
  TxBody TopTx MaryEra
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
        mkMemoizedEra @MaryEra $
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

instance EraTxLevel MaryEra where type STxLevel l MaryEra = STxTopLevel l MaryEra

instance HasEraTxLevel (AllegraTxBodyRaw ma) MaryEra where
  toSTxLevel AllegraTxBodyRaw {} = STopTxOnly

instance HasEraTxLevel TxBody MaryEra where
  toSTxLevel = toSTxLevel . getMemoRawType

instance Memoized (TxBody l MaryEra) where
  type RawType (TxBody l MaryEra) = AllegraTxBodyRaw MultiAsset l MaryEra

emptyMaryTxBodyRaw :: MaryTxBodyRaw TopTx
emptyMaryTxBodyRaw = emptyAllegraTxBodyRaw

basicMaryTxBody :: Typeable l => TxBody l MaryEra
basicMaryTxBody = mkMemoizedEra @MaryEra $ asSTxTopLevel emptyMaryTxBodyRaw

instance EraTxBody MaryEra where
  newtype TxBody l MaryEra = MkMaryTxBody (MemoBytes (MaryTxBodyRaw l))
    deriving newtype (SafeToHash, ToCBOR, EncCBOR, Eq, Show, NoThunks, NFData)

  mkBasicTxBody = basicMaryTxBody

  inputsTxBodyL =
    lensMemoRawType @MaryEra (\AllegraTxBodyRaw {atbrInputs} -> atbrInputs) $ \txBodyRaw inputs -> txBodyRaw {atbrInputs = inputs}
  {-# INLINEABLE inputsTxBodyL #-}

  outputsTxBodyL =
    lensMemoRawType @MaryEra (\AllegraTxBodyRaw {atbrOutputs} -> atbrOutputs) $ \txBodyRaw outputs -> txBodyRaw {atbrOutputs = outputs}
  {-# INLINEABLE outputsTxBodyL #-}

  feeTxBodyL =
    lensMemoRawType @MaryEra atbrFee $ \txBodyRaw fee -> txBodyRaw {atbrFee = fee}
  {-# INLINEABLE feeTxBodyL #-}

  auxDataHashTxBodyL =
    lensMemoRawType @MaryEra (\AllegraTxBodyRaw {atbrAuxDataHash} -> atbrAuxDataHash) $
      \txBodyRaw auxDataHash -> txBodyRaw {atbrAuxDataHash = auxDataHash}
  {-# INLINEABLE auxDataHashTxBodyL #-}

  spendableInputsTxBodyF = inputsTxBodyL
  {-# INLINE spendableInputsTxBodyF #-}

  allInputsTxBodyF = inputsTxBodyL
  {-# INLINEABLE allInputsTxBodyF #-}

  withdrawalsTxBodyL =
    lensMemoRawType @MaryEra (\AllegraTxBodyRaw {atbrWithdrawals} -> atbrWithdrawals) $ \txBodyRaw withdrawals -> txBodyRaw {atbrWithdrawals = withdrawals}
  {-# INLINEABLE withdrawalsTxBodyL #-}

  certsTxBodyL =
    lensMemoRawType @MaryEra (\AllegraTxBodyRaw {atbrCerts} -> atbrCerts) $ \txBodyRaw certs -> txBodyRaw {atbrCerts = certs}
  {-# INLINEABLE certsTxBodyL #-}

  getGenesisKeyHashCountTxBody = getShelleyGenesisKeyHashCountTxBody

instance ShelleyEraTxBody MaryEra where
  ttlTxBodyL = notSupportedInThisEraL
  {-# INLINEABLE ttlTxBodyL #-}

  updateTxBodyL =
    lensMemoRawType @MaryEra atbrUpdate $ \txBodyRaw update -> txBodyRaw {atbrUpdate = update}
  {-# INLINEABLE updateTxBodyL #-}

instance AllegraEraTxBody MaryEra where
  vldtTxBodyL =
    lensMemoRawType @MaryEra (\AllegraTxBodyRaw {atbrValidityInterval} -> atbrValidityInterval) $
      \txBodyRaw vldt -> txBodyRaw {atbrValidityInterval = vldt}
  {-# INLINEABLE vldtTxBodyL #-}

instance MaryEraTxBody MaryEra where
  mintTxBodyL =
    lensMemoRawType @MaryEra
      (\AllegraTxBodyRaw {atbrMint} -> atbrMint)
      (\txBodyRaw mint -> txBodyRaw {atbrMint = mint})
  {-# INLINEABLE mintTxBodyL #-}
