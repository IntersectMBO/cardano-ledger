{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Allegra.ImpTest (
  impAllegraSatisfyNativeScript,
  module Test.Cardano.Ledger.Shelley.ImpTest,
  produceScript,
) where

import Cardano.Crypto.DSIGN (DSIGNAlgorithm (..), Ed25519DSIGN)
import Cardano.Crypto.Hash.Class (Hash)
import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.Allegra (AllegraEra)
import Cardano.Ledger.Allegra.Core
import Cardano.Ledger.Allegra.Scripts (Timelock (..))
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Credential (Credential (..), StakeReference (..))
import Cardano.Ledger.Crypto (Crypto (..))
import Cardano.Ledger.Keys (KeyHash, KeyRole (..))
import Cardano.Ledger.TxIn (TxIn)
import Control.Monad.State.Strict (get)
import qualified Data.Map.Strict as Map
import Data.Sequence.Strict (StrictSeq (..))
import qualified Data.Sequence.Strict as SSeq
import qualified Data.Set as Set
import Lens.Micro ((&), (.~), (^.))
import Test.Cardano.Ledger.Allegra.TreeDiff ()
import Test.Cardano.Ledger.Core.KeyPair (KeyPair)
import Test.Cardano.Ledger.Core.Utils (txInAt)
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Shelley.ImpTest

instance
  ( Crypto c
  , NFData (SigDSIGN (DSIGN c))
  , NFData (VerKeyDSIGN (DSIGN c))
  , DSIGN c ~ Ed25519DSIGN
  , Signable (DSIGN c) (Hash (HASH c) EraIndependentTxBody)
  ) =>
  ShelleyEraImp (AllegraEra c)
  where
  initImpTestState = pure ()

  impSatisfyNativeScript = impAllegraSatisfyNativeScript

  fixupTx = shelleyFixupTx

impAllegraSatisfyNativeScript ::
  (ShelleyEraImp era, NativeScript era ~ Timelock era) =>
  Set.Set (KeyHash 'Witness (EraCrypto era)) ->
  NativeScript era ->
  ImpTestM era (Maybe (Map.Map (KeyHash 'Witness (EraCrypto era)) (KeyPair 'Witness (EraCrypto era))))
impAllegraSatisfyNativeScript providedVKeyHashes script = do
  impState <- get
  let
    keyPairs = impState ^. impKeyPairsG
    prevSlotNo = impState ^. impLastTickG
    satisfyMOf m Empty
      | m <= 0 = Just mempty
      | otherwise = Nothing
    satisfyMOf m (x :<| xs) =
      case satisfyScript x of
        Nothing -> satisfyMOf m xs
        Just kps -> do
          kps' <- satisfyMOf (m - 1) xs
          Just $ kps <> kps'
    satisfyScript = \case
      RequireSignature keyHash
        | keyHash `Set.member` providedVKeyHashes -> Just mempty
        | otherwise -> do
            keyPair <- Map.lookup keyHash keyPairs
            Just $ Map.singleton keyHash keyPair
      RequireAllOf ss -> satisfyMOf (length ss) ss
      RequireAnyOf ss -> satisfyMOf 1 ss
      RequireMOf m ss -> satisfyMOf m ss
      RequireTimeExpire slotNo
        | slotNo < prevSlotNo -> Just mempty
        | otherwise -> Nothing
      RequireTimeStart slotNo
        | slotNo > prevSlotNo -> Just mempty
        | otherwise -> Nothing
  pure $ satisfyScript script

produceScript ::
  ShelleyEraImp era =>
  ScriptHash (EraCrypto era) ->
  ImpTestM era (TxIn (EraCrypto era))
produceScript scriptHash = do
  let addr = Addr Testnet (ScriptHashObj scriptHash) StakeRefNull
  let tx =
        mkBasicTx mkBasicTxBody
          & bodyTxL . outputsTxBodyL .~ SSeq.singleton (mkBasicTxOut addr (inject (Coin 10)))
  txInAt (0 :: Int) <$> submitTx tx
