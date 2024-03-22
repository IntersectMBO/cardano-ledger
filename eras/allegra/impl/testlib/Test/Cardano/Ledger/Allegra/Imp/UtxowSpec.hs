{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Test.Cardano.Ledger.Allegra.Imp.UtxowSpec (spec) where

import Cardano.Ledger.Allegra.Scripts (Timelock (..))
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Core
import Cardano.Ledger.Keys (coerceKeyRole)
import Cardano.Ledger.SafeHash (HashAnnotated (..))
import Cardano.Ledger.Shelley.Rules (ShelleyUtxowPredFailure (..))
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
    let txToSubmit = mkBasicTx mkBasicTxBody & bodyTxL . inputsTxBodyL .~ [txIn]
    let fixup tx = do
          keyPair <- impRootKh >>= lookupKeyPair
          (addRootTxIn >=> fixupFees) tx <&> withWitVKey keyPair
    void $
      withFixup fixup $
        submitFailingTx
          txToSubmit
          [ injectFailure $
              MissingScriptWitnessesUTXOW [scriptHash]
          ]

  it "ExtraneousScriptWitnessesUTXOW" $ do
    rootKh <- impRootKh
    let script = fromNativeScript $ RequireSignature $ coerceKeyRole rootKh
    let scriptHash = hashScript @era script
    let tx = mkBasicTx mkBasicTxBody & witsTxL . scriptTxWitsL .~ [(scriptHash, script)]
    withFixup shelleyFixupTx $
      submitFailingTx
        tx
        [ injectFailure $
            ExtraneousScriptWitnessesUTXOW [scriptHash]
        ]
  where
    withWitVKey keyPair tx =
      tx & witsTxL . addrTxWitsL .~ [witVKey keyPair tx]
    witVKey keyPair tx =
      let bodyHash = hashAnnotated $ tx ^. bodyTxL
       in mkWitnessVKey bodyHash keyPair
