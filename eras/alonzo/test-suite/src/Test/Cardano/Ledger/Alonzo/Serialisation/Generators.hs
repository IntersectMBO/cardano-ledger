{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Alonzo.Serialisation.Generators where

import Cardano.Ledger.Allegra.Scripts (ValidityInterval (..))
import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.Core
import Cardano.Ledger.Alonzo.Scripts (
  AlonzoScript (..),
 )
import Cardano.Ledger.Alonzo.Scripts.Data (
  BinaryData,
  Data (..),
 )
import Cardano.Ledger.Alonzo.Tx (
  AlonzoTxBody (..),
 )
import Cardano.Ledger.Alonzo.TxAuxData (
  AuxiliaryDataHash,
 )
import Cardano.Ledger.Alonzo.TxBody (AlonzoTxOut (..))
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Binary (EncCBOR (..))
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Crypto
import Cardano.Ledger.Keys (KeyHash)
import Cardano.Ledger.Mary.Value (MultiAsset)
import Cardano.Ledger.Shelley.Delegation (ShelleyDCert)
import Cardano.Ledger.Shelley.PParams (Update)
import Cardano.Ledger.TxIn (TxIn)
import Cardano.Ledger.Val (Val)
import Codec.CBOR.Term (Term (..))
import Data.Maybe (catMaybes)
import Data.Typeable (Typeable)
import Test.Cardano.Ledger.Alonzo.Arbitrary ()
import Test.Cardano.Ledger.Binary.Twiddle
import Test.Cardano.Ledger.Shelley.Serialisation.EraIndepGenerators ()
import Test.Cardano.Ledger.Shelley.Serialisation.Generators ()
import Test.QuickCheck

instance (Era era, Val (Value era)) => Twiddle (AlonzoTxOut era) where
  twiddle v = twiddle v . toTerm v

instance Twiddle SlotNo where
  twiddle v = twiddle v . toTerm v

instance Era era => Twiddle (ShelleyDCert era) where
  twiddle v = twiddle v . toTerm v

instance Crypto c => Twiddle (Withdrawals c) where
  twiddle v = twiddle v . toTerm v

instance Crypto c => Twiddle (AuxiliaryDataHash c) where
  twiddle v = twiddle v . toTerm v

instance Crypto c => Twiddle (Update (AlonzoEra c)) where
  twiddle v = twiddle v . toTerm v

instance Crypto c => Twiddle (MultiAsset c) where
  twiddle v = twiddle v . encodingToTerm v . encCBOR

instance Crypto c => Twiddle (ScriptIntegrityHash c) where
  twiddle v = twiddle v . toTerm v

instance (Crypto c, Typeable t) => Twiddle (KeyHash t c) where
  twiddle v = twiddle v . toTerm v

instance Twiddle Network where
  twiddle v = twiddle v . toTerm v

instance Crypto c => Twiddle (TxIn c) where
  twiddle v = twiddle v . toTerm v

instance Twiddle Coin where
  twiddle v = twiddle v . toTerm v

instance Crypto c => Twiddle (AlonzoTxBody (AlonzoEra c)) where
  twiddle v txBody = do
    inputs' <- twiddle v $ atbInputs txBody
    outputs' <- twiddle v $ atbOutputs txBody
    fee' <- twiddle v $ atbTxFee txBody
    -- Empty collateral can be represented by empty set or the
    -- value can be omitted entirely
    ttl' <- twiddleStrictMaybe v . invalidHereafter $ atbValidityInterval txBody
    cert' <- emptyOrNothing v $ atbCerts txBody
    withdrawals' <- twiddle v $ atbWithdrawals txBody
    update' <- twiddleStrictMaybe v $ atbUpdate txBody
    auxDataHash' <- twiddleStrictMaybe v $ atbAuxDataHash txBody
    validityStart' <- twiddleStrictMaybe v . invalidBefore $ atbValidityInterval txBody
    mint' <- twiddle v $ atbMint txBody
    scriptDataHash' <- twiddleStrictMaybe v $ atbScriptIntegrityHash txBody
    collateral' <- emptyOrNothing v $ atbCollateral txBody
    requiredSigners' <- emptyOrNothing v $ atbReqSignerHashes txBody
    networkId' <- twiddleStrictMaybe v $ atbTxNetworkId txBody
    mp <- elements [TMap, TMapI]
    let fields =
          [ (TInt 0, inputs')
          , (TInt 1, outputs')
          , (TInt 2, fee')
          ]
            <> catMaybes
              [ (TInt 3,) <$> ttl'
              , (TInt 4,) <$> cert'
              , (TInt 5,) <$> Just withdrawals'
              , (TInt 6,) <$> update'
              , (TInt 7,) <$> auxDataHash'
              , (TInt 8,) <$> validityStart'
              , (TInt 9,) <$> Just mint'
              , (TInt 11,) <$> scriptDataHash'
              , (TInt 13,) <$> collateral'
              , (TInt 14,) <$> requiredSigners'
              , (TInt 15,) <$> networkId'
              ]
    fields' <- shuffle fields
    pure $ mp fields'

instance Crypto c => Twiddle (AlonzoScript (AlonzoEra c)) where
  twiddle v = twiddle v . toTerm v

instance Typeable c => Twiddle (Data (AlonzoEra c)) where
  twiddle v = twiddle v . toTerm v

instance Crypto c => Twiddle (BinaryData (AlonzoEra c)) where
  twiddle v = twiddle v . toTerm v
