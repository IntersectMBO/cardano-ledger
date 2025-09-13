{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Cardano.Ledger.Allegra.ImpTest (
  impAllegraSatisfyNativeScript,
  module Test.Cardano.Ledger.Shelley.ImpTest,
) where

import Cardano.Ledger.Allegra (AllegraEra)
import Cardano.Ledger.Allegra.Core
import Cardano.Ledger.Allegra.Scripts (
  AllegraEraScript,
  Timelock,
  evalTimelock,
  pattern RequireTimeExpire,
  pattern RequireTimeStart,
 )
import Cardano.Ledger.Shelley.Scripts (
  pattern RequireAllOf,
  pattern RequireAnyOf,
  pattern RequireMOf,
  pattern RequireSignature,
 )
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Lens.Micro ((^.))
import Test.Cardano.Ledger.Allegra.Era ()
import Test.Cardano.Ledger.Allegra.TreeDiff ()
import Test.Cardano.Ledger.Core.KeyPair (KeyPair)
import Test.Cardano.Ledger.Shelley.ImpTest

instance ShelleyEraImp AllegraEra where
  impSatisfyNativeScript = impAllegraSatisfyNativeScript

  fixupTx = shelleyFixupTx
  expectTxSuccess = impShelleyExpectTxSuccess
  modifyImpInitProtVer = shelleyModifyImpInitProtVer

impAllegraSatisfyNativeScript ::
  ( ShelleyEraImp era
  , AllegraEraScript era
  , AllegraEraTxBody era
  , NativeScript era ~ Timelock era
  ) =>
  Set.Set (KeyHash 'Witness) ->
  TxBody era ->
  NativeScript era ->
  ImpTestM era (Maybe (Map.Map (KeyHash 'Witness) (KeyPair 'Witness)))
impAllegraSatisfyNativeScript providedVKeyHashes txBody script = do
  let vi = txBody ^. vldtTxBodyL
  case script of
    RequireSignature keyHash -> impSatisfySignature keyHash providedVKeyHashes
    RequireAllOf ss -> impSatisfyMNativeScripts (length ss) ss providedVKeyHashes txBody
    RequireAnyOf ss -> impSatisfyMNativeScripts 1 ss providedVKeyHashes txBody
    RequireMOf m ss -> impSatisfyMNativeScripts m ss providedVKeyHashes txBody
    lock@(RequireTimeStart _)
      | evalTimelock mempty vi lock -> pure $ Just mempty
      | otherwise -> pure Nothing
    lock@(RequireTimeExpire _)
      | evalTimelock mempty vi lock -> pure $ Just mempty
      | otherwise -> pure Nothing
    _ -> error "Impossible: All NativeScripts should have been accounted for"
