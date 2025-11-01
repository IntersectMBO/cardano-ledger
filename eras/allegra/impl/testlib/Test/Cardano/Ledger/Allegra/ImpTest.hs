{-# LANGUAGE CPP #-}
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
#if __GLASGOW_HASKELL__ >= 914
  data RequireTimeExpire,
  data RequireTimeStart,
#else
  pattern RequireTimeExpire,
  pattern RequireTimeStart,
#endif
 )
import Cardano.Ledger.Shelley.Scripts (
#if __GLASGOW_HASKELL__ >= 914
  data RequireAllOf,
  data RequireAnyOf,
  data RequireMOf,
  data RequireSignature,
#else
  pattern RequireAllOf,
  pattern RequireAnyOf,
  pattern RequireMOf,
  pattern RequireSignature,
#endif
 )
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Lens.Micro ((^.))
import Test.Cardano.Ledger.Allegra.Era ()
import Test.Cardano.Ledger.Allegra.TreeDiff ()
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Shelley.ImpTest

instance ShelleyEraImp AllegraEra where
  impSatisfyNativeScript = impAllegraSatisfyNativeScript

  fixupTx = shelleyFixupTx
  expectTxSuccess = impShelleyExpectTxSuccess
  modifyImpInitProtVer = shelleyModifyImpInitProtVer
  genRegTxCert = shelleyGenRegTxCert
  genUnRegTxCert = shelleyGenUnRegTxCert
  delegStakeTxCert = shelleyDelegStakeTxCert

impAllegraSatisfyNativeScript ::
  ( ShelleyEraImp era
  , AllegraEraScript era
  , AllegraEraTxBody era
  , NativeScript era ~ Timelock era
  ) =>
  Set.Set (KeyHash 'Witness) ->
  TxBody l era ->
  NativeScript era ->
  ImpTestM era (Maybe (Map.Map (KeyHash 'Witness) (KeyPair 'Witness)))
impAllegraSatisfyNativeScript providedVKeyHashes txBody script = do
  let vi = txBody ^. vldtTxBodyL
  case script of
    RequireSignature keyHash -> impSatisfySignature keyHash providedVKeyHashes
    RequireAllOf ss -> impSatisfyMNativeScripts providedVKeyHashes txBody (length ss) ss
    RequireAnyOf ss -> do
      m <- frequency [(9, pure 1), (1, choose (1, length ss))]
      impSatisfyMNativeScripts providedVKeyHashes txBody m ss
    RequireMOf m ss -> impSatisfyMNativeScripts providedVKeyHashes txBody m ss
    lock@(RequireTimeStart _)
      | evalTimelock mempty vi lock -> pure $ Just mempty
      | otherwise -> pure Nothing
    lock@(RequireTimeExpire _)
      | evalTimelock mempty vi lock -> pure $ Just mempty
      | otherwise -> pure Nothing
    _ -> error "Impossible: All NativeScripts should have been accounted for"
