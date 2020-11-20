{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC  -fno-warn-orphans #-}


-- | This module is meant to supply the Era independent testing instances for the MaryEra
--   use it something like this:   import Test.Cardano.Ledger.Mary()
--   The exported functions are used by the AllegraEra, since these two Eras share some stuff.

module Test.Cardano.Ledger.Mary
 ( genMATxBody,
    txBodyZero,
    someLeaf,
    quantifyTL,
    unQuantifyTL,
  ) where

import Data.Sequence.Strict (StrictSeq,fromList)
import Data.Hashable
import Data.String(fromString)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Cardano.Binary (ToCBOR (..),serializeEncoding')
import Cardano.Slotting.Slot (SlotNo (..))
import Cardano.Ledger.Era (Crypto, Era)
import Cardano.Ledger.Mary.Value(Value(..),AssetName(..),PolicyID(..),insert)
import Cardano.Ledger.ShelleyMA.Timelocks(Timelock(..))
import Cardano.Ledger.ShelleyMA.TxBody(TxBody(..),ValidityInterval(..),StrictMaybe(..),FamsTo)
import Cardano.Ledger.Val (Val (zero))
import qualified Cardano.Ledger.Core as Core(Script, Value)
import qualified Cardano.Ledger.Crypto as CryptoClass
import Shelley.Spec.Ledger.Coin(Coin(..))
import Shelley.Spec.Ledger.Keys(KeyHash,KeyRole(..))
import Shelley.Spec.Ledger.MetaData (MetaDataHash)
import Shelley.Spec.Ledger.PParams(Update)
import Shelley.Spec.Ledger.Tx(TxOut,TxIn)
import Shelley.Spec.Ledger.TxBody(DCert,Wdrl(..))
import Test.Cardano.Ledger.EraBuffet(MaryEra)
import Test.Shelley.Spec.Ledger.Generator.Core(EraGen (..))
import Test.Shelley.Spec.Ledger.Generator.EraGen(genUtxo0)
import Test.Shelley.Spec.Ledger.Utils (Split (..))
import Test.Shelley.Spec.Ledger.Generator.Scripts
  ( ScriptClass(..),
    Quantifier(..),
    ValueClass(..),
    TxBodyClass(..),
    exponential,
  )
import Test.QuickCheck(Gen,frequency,elements,vectorOf,choose)

-- =====================================
-- EraGen instances for the MaryEra
-- =====================================

instance (CryptoClass.Crypto c) => ScriptClass (MaryEra c) where
  isKey _ (RequireSignature hk) = Just hk
  isKey _ _ = Nothing
  basescript _proxy = someLeaf
  quantify _ = quantifyTL
  unQuantify _ = unQuantifyTL

type instance Core.Script (MaryEra c) = Timelock (MaryEra c)

instance (CryptoClass.Crypto c) => ValueClass  (MaryEra c) where
   genValue scripthashes = genMaryValue assets (map PolicyID scripthashes)

instance (CryptoClass.Crypto c) => TxBodyClass (MaryEra c) where
  emptyTxBody = txBodyZero
  liftTxBody gen = do { _tx <- gen; pure undefined }

instance CryptoClass.Crypto c => EraGen (MaryEra c) where
  genEraUtxo0 = genUtxo0
  genEraTxBody = genMATxBody
  updateEraTxBody (TxBody _in _out cert wdrl _txfee vi upd meta forge) fee ins outs =
     (TxBody ins outs cert wdrl fee vi upd meta forge)

-- ========================================================
-- Reusable pieces for both Mary and Allegra

quantifyTL:: Era era => Timelock era -> Quantifier (Timelock era)
quantifyTL (RequireAllOf xs) = AllOf (foldr (:) [] xs)
quantifyTL (RequireAnyOf xs) = AnyOf (foldr (:) [] xs)
quantifyTL (RequireMOf n xs) = MOf n (foldr (:) [] xs)
quantifyTL t = Leaf t

unQuantifyTL:: Era era => Quantifier (Timelock era) -> Timelock era
unQuantifyTL (AllOf xs) = (RequireAllOf (fromList xs))
unQuantifyTL (AnyOf xs) = (RequireAnyOf (fromList xs))
unQuantifyTL (MOf n xs) = (RequireMOf n (fromList xs))
unQuantifyTL (Leaf t) = t

-- | Generate some Leaf Timelock (i.e. a Signature or TimeStart or TimeExpire)
someLeaf :: Era era => KeyHash 'Witness (Crypto era) -> Timelock era
someLeaf x =
    let n = mod (hash(serializeEncoding' (toCBOR x))) 200
    in if n <= 50
          then RequireTimeStart (SlotNo (fromIntegral n))
          else if n > 150
                  then RequireTimeExpire (SlotNo (fromIntegral n))
                  else RequireSignature x

assets :: [AssetName]
assets = map (AssetName . fromString) ["Red","Blue","Green","Yellow","Orange","Purple","Black","White"]

genMaryValue :: [AssetName] -> [PolicyID era] -> Integer -> Integer ->  Gen(Value era)
genMaryValue ass policys minCoin maxCoin = do
   coinN <- exponential minCoin maxCoin
   size <- frequency [(6,pure 0),(4,pure 1),(2,pure 2),(1,pure 3)]
   triples <- vectorOf size (do { p <- elements policys; n <- elements ass; i <- elements [1,2,3,4]; pure(p,n,i)})
   pure $ foldr (\ (p,n,i) ans -> insert (+) p n i ans) (Value coinN Map.empty) triples

txBodyZero ::  (Val (Core.Value era), FamsTo era) => TxBody era
txBodyZero = TxBody
    Set.empty
    (fromList [])
    (fromList [])
    (Wdrl Map.empty)
    (Coin 0)
    (ValidityInterval  SNothing  SNothing)
    SNothing
    SNothing
    zero

genMATxBody :: forall era.
  ( FamsTo era,
    ValueClass era
  ) =>
    SlotNo ->
    Set.Set (TxIn era) ->
    StrictSeq (TxOut era) ->
    StrictSeq (DCert era) ->
    Wdrl era ->
    Coin ->
    StrictMaybe (Update era) ->
    StrictMaybe (MetaDataHash era) ->
    Gen (TxBody era)
genMATxBody (ttl@(SlotNo n)) ins outs cert wdrl fee upd meta = do
   m <- choose (5,n)
   timeStart <- frequency [(1,pure SNothing),(4,pure $ SJust (SlotNo m))]
   timeExpire <- frequency [(1,pure SNothing),(4, pure $ SJust ttl)]
   forge <- genValue @era [] 0 0 -- TODO we need a [PolicyID era]  not the []
   pure (TxBody
          ins
          outs
          cert
          wdrl
          fee
          (ValidityInterval timeStart timeExpire)
          upd
          meta
          forge)

instance Split (Value era) where
  vsplit (Value n _) 0 = ([], Coin n)
  vsplit (Value n mp) m
    | m Prelude.<= 0 = error "must split coins into positive parts"
    | otherwise = (take (fromIntegral m) ((Value (n `div` m) mp): (repeat (Value (n `div` m) Map.empty))),
                   Coin (n `rem` m))