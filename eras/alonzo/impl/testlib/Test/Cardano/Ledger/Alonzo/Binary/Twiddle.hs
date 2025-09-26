{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Alonzo.Binary.Twiddle (
  module Test.Cardano.Ledger.Mary.Binary.Twiddle,
) where

import Cardano.Ledger.Alonzo (AlonzoEra, AlonzoTxAuxData)
import Cardano.Ledger.Alonzo.Core
import Cardano.Ledger.Alonzo.Scripts (AlonzoScript (..))
import Cardano.Ledger.Alonzo.Tx
import Cardano.Ledger.Alonzo.TxBody (AlonzoTxOut (..), TxBody (..))
import Cardano.Ledger.Alonzo.TxWits (AlonzoTxWits)
import Cardano.Ledger.Mary.Value (MultiAsset)
import Cardano.Ledger.Plutus.Data (BinaryData)
import Cardano.Ledger.Shelley.PParams (Update)
import Cardano.Ledger.Val (Val)
import Codec.CBOR.Term (Term (..))
import Data.Maybe (catMaybes)
import Test.Cardano.Ledger.Alonzo.Arbitrary ()
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Mary.Binary.Twiddle

instance (Era era, Val (Value era)) => Twiddle (AlonzoTxOut era) where
  twiddle = twiddleTerm

instance Twiddle (Update AlonzoEra) where
  twiddle = twiddleTerm

instance Twiddle MultiAsset where
  twiddle = twiddleTerm

instance Twiddle ScriptIntegrityHash where
  twiddle = twiddleTerm

instance Twiddle (TxBody AlonzoEra) where
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

instance AlonzoEraScript era => Twiddle (AlonzoScript era) where
  twiddle = twiddleTerm

instance Twiddle (Data AlonzoEra) where
  twiddle = twiddleTerm

instance Twiddle (BinaryData AlonzoEra) where
  twiddle = twiddleTerm

instance Twiddle (PParams AlonzoEra) where
  twiddle = twiddleTerm

instance Twiddle (PParamsUpdate AlonzoEra) where
  twiddle = twiddleTerm

instance Era era => Twiddle (AlonzoTxAuxData era) where
  twiddle = twiddleTerm

instance Era era => Twiddle (AlonzoTxWits era) where
  twiddle = twiddleTerm

instance Twiddle (Tx AlonzoEra) where
  twiddle = twiddleTerm
