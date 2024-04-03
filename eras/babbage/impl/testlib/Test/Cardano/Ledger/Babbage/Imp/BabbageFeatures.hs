{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Test.Cardano.Ledger.Babbage.Imp.BabbageFeatures
  ( spec
  ) where

import Test.Cardano.Ledger.Imp.Common
import qualified Data.Sequence.Strict as SSeq
import Test.Cardano.Ledger.Babbage.ImpTest
import Lens.Micro
import Cardano.Ledger.Babbage.Core (EraTx (..), EraTxBody (..), EraTxOut (..), txIdTx, BabbageEraTxOut (..), ScriptHash, Era (..), AlonzoEraScript (..), AlonzoEraTxOut (..), BabbageEraTxBody (..), InjectRuleFailure (..), Value)
import qualified Data.Set as Set
import Cardano.Ledger.BaseTypes (Inject(..), maybeToStrictMaybe, StrictMaybe (..), Network (..))
import Test.Cardano.Ledger.Core.KeyPair (mkAddr)
import Cardano.Ledger.Plutus (hashPlutusScript, SLanguage (..), Data (..), hashData, TxOutSource (..))
import Test.Cardano.Ledger.Plutus.Examples (guessTheNumber3)
import Cardano.Ledger.Address (Addr(..))
import Cardano.Ledger.Credential (Credential(..), StakeReference (..))
import qualified PlutusLedgerApi.V1 as P1
import Cardano.Ledger.TxIn (mkTxInPartial)
import Cardano.Ledger.Alonzo.Rules (AlonzoUtxosPredFailure(..))
import Cardano.Ledger.Babbage.TxInfo (BabbageContextError(..))
import Cardano.Ledger.Alonzo.Plutus.Evaluate (CollectError(..))
import Cardano.Ledger.Alonzo.Plutus.Context (EraPlutusContext(..))

mkRefTxOut ::
  ( BabbageEraTxOut era
  , AlonzoEraImp era
  , Arbitrary (Value era)
  ) =>
  ScriptHash (EraCrypto era) ->
  ImpTestM era (TxOut era)
mkRefTxOut sh = do
  kpPayment <- lookupKeyPair =<< freshKeyHash
  kpStaking <- lookupKeyPair =<< freshKeyHash
  let mbyPlutusScript = impLookupPlutusScriptMaybe sh
  val <- arbitrary
  pure $
    mkBasicTxOut (mkAddr (kpPayment, kpStaking)) val
      & referenceScriptTxOutL .~ maybeToStrictMaybe (fromPlutusScript <$> mbyPlutusScript)

scriptLockedTxOut ::
  forall era.
  ( AlonzoEraTxOut era
  , Arbitrary (Value era)
  ) =>
  ScriptHash (EraCrypto era) ->
  P1.Data ->
  ImpTestM era (TxOut era)
scriptLockedTxOut shSpending spendDatum = do
  val <- arbitrary
  pure $ mkBasicTxOut (Addr Testnet (ScriptHashObj shSpending) StakeRefNull) val
    & dataHashTxOutL .~ SJust (hashData @era $ Data spendDatum)

spec :: forall era.
  ( AlonzoEraImp era
  , BabbageEraTxBody era
  , InjectRuleFailure "LEDGER" AlonzoUtxosPredFailure era
  , Inject (BabbageContextError era) (ContextError era)
  , Arbitrary (Value era)
  ) =>
  SpecWith (ImpTestState era)
spec = describe "BabbageFeatures" $ do
  describe "valid transactions" $ do
    it "inline datum" $ do
      let shSpending = hashPlutusScript $ guessTheNumber3 SPlutusV1
      refTxOut <- mkRefTxOut shSpending
      spendDatum <- P1.I <$> arbitrary
      scriptTxOut <- scriptLockedTxOut shSpending spendDatum
      producingTx <-
        fmap txIdTx . submitTxAnn "Producing transaction" $
          mkBasicTx mkBasicTxBody
            & bodyTxL . outputsTxBodyL
              .~ SSeq.fromList
                [ refTxOut
                , scriptTxOut & dataTxOutL .~ SJust (Data spendDatum)
                ]
      let
        lockedTxIn = mkTxInPartial producingTx 1
        consumingTx =
          mkBasicTx mkBasicTxBody
            & bodyTxL . inputsTxBodyL .~ Set.singleton lockedTxIn
            & bodyTxL . referenceInputsTxBodyL .~ Set.singleton (mkTxInPartial producingTx 0)
      impAnn "Consuming transaction" $
        submitFailingTx
          consumingTx
          ( pure . injectFailure $
              CollectErrors
                [BadTranslation . inject . InlineDatumsNotSupported @era $ TxOutFromInput lockedTxIn]
          )
