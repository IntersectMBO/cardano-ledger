{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.Allegra.Imp.UtxowSpec (spec) where

import Cardano.Ledger.Allegra.Scripts (Timelock (..))
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Core
import Cardano.Ledger.SafeHash (HashAnnotated (..))
import Cardano.Ledger.Shelley.Rules (ShelleyUtxowPredFailure (..))
import qualified Data.Set as Set
import Lens.Micro
import Test.Cardano.Ledger.Allegra.ImpTest (produceScript)
import Test.Cardano.Ledger.Core.KeyPair (mkWitnessVKey)
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Shelley.ImpTest

spec ::
  forall era.
  ( ShelleyEraImp era
  , NativeScript era ~ Timelock era
  , InjectRuleFailure "LEDGER" ShelleyUtxowPredFailure era
  ) =>
  SpecWith (ImpTestState era)
spec = describe "UTXOW" $ do
  it "MissingScriptWitnessesUTXOW" $ do
    let scriptHash = hashScript @era $ fromNativeScript $ RequireTimeStart (SlotNo 1)
    txIn <- produceScript scriptHash

    tx <- impAnn "Build a script-consuming transaction and fix it up minimally" $ do
      tx <-
        (addRootTxIn >=> fixupFees) $
          mkBasicTx mkBasicTxBody & bodyTxL . inputsTxBodyL .~ Set.singleton txIn
      keyPair <- impRootKh >>= lookupKeyPair
      let wits = witVKey tx keyPair
      pure $ tx & witsTxL . addrTxWitsL .~ Set.singleton wits

    void $
      withNoFixup $
        submitFailingTx
          tx
          [ injectFailure $
              MissingScriptWitnessesUTXOW [scriptHash]
          ]
  where
    witVKey tx keyPair =
      let bodyHash = hashAnnotated $ tx ^. bodyTxL
       in mkWitnessVKey bodyHash keyPair
