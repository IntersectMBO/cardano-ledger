{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Babbage.Binary.Twiddle () where

import Cardano.Ledger.Babbage (BabbageEra)
import Cardano.Ledger.Babbage.Core
import Cardano.Ledger.Babbage.Tx
import Cardano.Ledger.Babbage.TxBody (BabbageTxOut (..))
import Cardano.Ledger.Binary (Sized, Term (..))
import Cardano.Ledger.Shelley.PParams (Update (..))
import Cardano.Ledger.Val (Val)
import Data.Maybe (catMaybes)
import Test.Cardano.Ledger.Alonzo.Binary.Twiddle ()
import Test.Cardano.Ledger.Binary.Twiddle (Twiddle (..), emptyOrNothing, toTerm, twiddleStrictMaybe)
import Test.Cardano.Ledger.Common

instance EraPParams era => Twiddle (Update era) where
  twiddle v = twiddle v . toTerm v

instance Twiddle a => Twiddle (Sized a)

instance (EraScript era, Val (Value era)) => Twiddle (BabbageTxOut era) where
  twiddle v = twiddle v . toTerm v

instance Twiddle (TxBody BabbageEra) where
  twiddle v txBody = do
    inputs' <- twiddle v $ btbInputs txBody
    outputs' <- twiddle v $ btbOutputs txBody
    fee' <- twiddle v $ btbTxFee txBody
    -- Empty collateral can be represented by empty set or the
    -- value can be omitted entirely
    ttl' <- twiddleStrictMaybe v . invalidHereafter $ btbValidityInterval txBody
    cert' <- emptyOrNothing v $ btbCerts txBody
    withdrawals' <- twiddle v $ btbWithdrawals txBody
    update' <- twiddleStrictMaybe v $ btbUpdate txBody
    auxDataHash' <- twiddleStrictMaybe v $ btbAuxDataHash txBody
    validityStart' <- twiddleStrictMaybe v . invalidBefore $ btbValidityInterval txBody
    mint' <- twiddle v $ btbMint txBody
    scriptDataHash' <- twiddleStrictMaybe v $ btbScriptIntegrityHash txBody
    collateral' <- emptyOrNothing v $ btbCollateral txBody
    requiredSigners' <- emptyOrNothing v $ btbReqSignerHashes txBody
    networkId' <- twiddleStrictMaybe v $ btbTxNetworkId txBody
    collateralReturn <- twiddleStrictMaybe v $ btbCollateralReturn txBody
    totalCollateral <- twiddleStrictMaybe v $ btbTotalCollateral txBody
    referenceInputs <- emptyOrNothing v $ btbReferenceInputs txBody
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
              , (TInt 16,) <$> collateralReturn
              , (TInt 17,) <$> totalCollateral
              , (TInt 18,) <$> referenceInputs
              ]
    fields' <- shuffle fields
    pure $ mp fields'
