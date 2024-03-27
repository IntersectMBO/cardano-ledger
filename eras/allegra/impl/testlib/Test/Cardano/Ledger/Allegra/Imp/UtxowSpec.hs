{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Test.Cardano.Ledger.Allegra.Imp.UtxowSpec (
  spec,
  specAllegra,
  genInvalidMetadatum,
) where

import Cardano.Ledger.Allegra.Scripts (Timelock (..))
import Cardano.Ledger.Allegra.TxAuxData
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Core
import Cardano.Ledger.Keys (coerceKeyRole)
import Cardano.Ledger.SafeHash (HashAnnotated (..))
import Cardano.Ledger.Shelley.Rules (ShelleyUtxowPredFailure (..))
import Cardano.Ledger.Shelley.TxAuxData (Metadatum (..))
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Word (Word64)
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

specAllegra ::
  forall era.
  ( ShelleyEraImp era
  , TxAuxData era ~ AllegraTxAuxData era
  , InjectRuleFailure "LEDGER" ShelleyUtxowPredFailure era
  ) =>
  SpecWith (ImpTestState era)
specAllegra = describe "UTXOW - Allegra" $ do
  it "InvalidMetadata" $ do
    invalidMetadatum <- genInvalidMetadatum
    let auxData = AllegraTxAuxData @era invalidMetadatum []
    let auxDataHash = hashTxAuxData auxData
    let tx =
          mkBasicTx mkBasicTxBody
            & bodyTxL . auxDataHashTxBodyL .~ SJust auxDataHash
            & auxDataTxL .~ SJust auxData
    submitFailingTx tx [injectFailure InvalidMetadata]

genInvalidMetadatum :: ImpTestM era (Map.Map Word64 Metadatum)
genInvalidMetadatum = do
  size <- choose (65, 1000)
  let genM =
        oneof
          [ B . BS.pack <$> vectorOf size arbitrary
          , S . T.pack <$> vectorOf size arbitrary
          ]
  Map.fromList <$> listOf1 ((,) <$> arbitrary <*> genM)
