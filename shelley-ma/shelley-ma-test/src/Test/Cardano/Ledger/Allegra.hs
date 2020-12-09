{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Cardano.Ledger.Allegra
  ( -- export EraGen instance for AllegraEra and helpers shared with MaryEra
    quantifyTL,
    unQuantifyTL,
    someLeaf,
    genValidityInterval,
  )
where

import Cardano.Binary (serializeEncoding', toCBOR)
import qualified Cardano.Ledger.Crypto as CryptoClass
import Cardano.Ledger.Era (Era (Crypto))
import Cardano.Ledger.ShelleyMA.Timelocks (Timelock (..))
import Cardano.Ledger.ShelleyMA.TxBody
  ( FamsTo,
    TxBody (..),
    ValidityInterval (ValidityInterval),
  )
import Cardano.Slotting.Slot (SlotNo (SlotNo))
import Data.Hashable (hash)
import Data.Sequence.Strict (StrictSeq, fromList)
import qualified Data.Set as Set
import Shelley.Spec.Ledger.API (KeyRole (Witness))
import Shelley.Spec.Ledger.BaseTypes (StrictMaybe (..))
import Shelley.Spec.Ledger.Coin (Coin)
import Shelley.Spec.Ledger.Keys (KeyHash)
import Shelley.Spec.Ledger.Metadata (MetadataHash)
import Shelley.Spec.Ledger.PParams (Update)
import Shelley.Spec.Ledger.TxBody (DCert, TxIn, TxOut, Wdrl)
import Test.Cardano.Ledger.EraBuffet (AllegraEra)
import Test.QuickCheck (Gen, choose, frequency)
import Test.Shelley.Spec.Ledger.Generator.Constants (Constants (..))
import Test.Shelley.Spec.Ledger.Generator.Core (GenEnv (..), genCoin)
import Test.Shelley.Spec.Ledger.Generator.EraGen (EraGen (..))
import Test.Shelley.Spec.Ledger.Generator.ScriptClass
  ( Quantifier (..),
    ScriptClass (..),
  )

{------------------------------------------------------------------------------
 EraGen instance for AllegraEra - This instance makes it possible to run the
 Shelley property tests for (AllegraEra crypto)

 This instance is layered on top of the ShelleyMA instances
 in Cardano.Ledger.ShelleyMA.Scripts:

 `type instance Core.Script (AllegraEra c) = Timelock (AllegraEra c)`
 `type instance ValidateScript (ShelleyMAEra ma c) = ...`
------------------------------------------------------------------------------}

instance (CryptoClass.Crypto c) => ScriptClass (AllegraEra c) where
  isKey _ (RequireSignature hk) = Just hk
  isKey _ _ = Nothing
  basescript _proxy = someLeaf
  quantify _ = quantifyTL
  unQuantify _ = unQuantifyTL

instance CryptoClass.Crypto c => EraGen (AllegraEra c) where
  genGenesisValue (GenEnv _keySpace Constants {minGenesisOutputVal, maxGenesisOutputVal}) =
    genCoin minGenesisOutputVal maxGenesisOutputVal
  genEraTxBody _ge = genTxBody
  genEraMetadata = error "TODO @uroboros - implement genEraMetadata for Allegra"
  updateEraTxBody (TxBody _in _out cert wdrl _txfee vi upd meta forge) fee ins outs =
    TxBody ins outs cert wdrl fee vi upd meta forge

genTxBody ::
  forall era.
  ( FamsTo era,
    EraGen era
  ) =>
  SlotNo ->
  Set.Set (TxIn (Crypto era)) ->
  StrictSeq (TxOut era) ->
  StrictSeq (DCert (Crypto era)) ->
  Wdrl (Crypto era) ->
  Coin ->
  StrictMaybe (Update era) ->
  StrictMaybe (MetadataHash (Crypto era)) ->
  Gen (TxBody era)
genTxBody slot ins outs cert wdrl fee upd meta = do
  validityInterval <- genValidityInterval slot
  let mint = mempty -- the mint field is always empty for an Allegra TxBody
  pure $
    TxBody
      ins
      outs
      cert
      wdrl
      fee
      validityInterval
      upd
      meta
      mint

{------------------------------------------------------------------------------
  ShelleyMA helpers, shared by Allegra and Mary
------------------------------------------------------------------------------}

quantifyTL :: CryptoClass.Crypto crypto => Timelock crypto -> Quantifier (Timelock crypto)
quantifyTL (RequireAllOf xs) = AllOf (foldr (:) [] xs)
quantifyTL (RequireAnyOf xs) = AnyOf (foldr (:) [] xs)
quantifyTL (RequireMOf n xs) = MOf n (foldr (:) [] xs)
quantifyTL t = Leaf t

unQuantifyTL :: CryptoClass.Crypto crypto => Quantifier (Timelock crypto) -> Timelock crypto
unQuantifyTL (AllOf xs) = RequireAllOf (fromList xs)
unQuantifyTL (AnyOf xs) = RequireAnyOf (fromList xs)
unQuantifyTL (MOf n xs) = RequireMOf n (fromList xs)
unQuantifyTL (Leaf t) = t

genValidityInterval :: SlotNo -> Gen ValidityInterval
genValidityInterval (SlotNo currentSlot) = do
  start <- choose (currentSlot, currentSlot + 50)
  end <- choose (start, start + 50)
  validityStart <- frequency [(1, pure SNothing), (4, pure $ SJust (SlotNo start))]
  validityEnd <- frequency [(1, pure SNothing), (4, pure $ SJust (SlotNo end))]
  pure $
    ValidityInterval validityStart validityEnd

-- | Generate some Leaf Timelock (i.e. a Signature or TimeStart or TimeExpire)
someLeaf :: CryptoClass.Crypto crypto => KeyHash 'Witness crypto -> Timelock crypto
someLeaf x =
  let n = mod (hash (serializeEncoding' (toCBOR x))) 200
   in if n <= 50
        then RequireTimeStart (SlotNo (fromIntegral n))
        else
          if n > 150
            then RequireTimeExpire (SlotNo (fromIntegral n))
            else RequireSignature x
