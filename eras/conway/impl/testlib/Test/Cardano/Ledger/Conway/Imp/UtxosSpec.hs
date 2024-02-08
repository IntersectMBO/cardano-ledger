{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Conway.Imp.UtxosSpec where

import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.Alonzo.Plutus.Context (EraPlutusContext (..))
import Cardano.Ledger.Alonzo.Plutus.Evaluate (CollectError (..))
import Cardano.Ledger.Alonzo.Rules (AlonzoUtxosPredFailure (..))
import Cardano.Ledger.Babbage.Rules (
  BabbageUtxoPredFailure (..),
 )
import Cardano.Ledger.Babbage.TxInfo (BabbageContextError (..))
import Cardano.Ledger.BaseTypes (
  Inject (..),
  Network (..),
  StrictMaybe (..),
  maybeToStrictMaybe,
 )
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Core (
  AlonzoEraScript (..),
  AlonzoEraTxOut (..),
  BabbageEraTxBody (..),
  BabbageEraTxOut (..),
  Era (..),
  EraTx (..),
  EraTxBody (..),
  EraTxOut (..),
  EraTxWits (..),
  InjectRuleFailure (..),
  ScriptHash,
  txIdTx,
 )
import Cardano.Ledger.Credential (Credential (..), StakeReference (..))
import Cardano.Ledger.Plutus (Data (..), TxOutSource (..), hashData)
import Cardano.Ledger.TxIn (TxId (..), mkTxInPartial)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Sequence.Strict as SSeq
import qualified Data.Set as Set
import Lens.Micro ((&), (.~), (^.))
import qualified PlutusLedgerApi.V1 as P1
import Test.Cardano.Ledger.Conway.ImpTest (
  ImpTestM,
  ImpTestState,
  ShelleyEraImp,
  freshKeyHash,
  impAddPlutusScript,
  impAnn,
  impLookupPlutusScript,
  lookupKeyPair,
  submitFailingTx,
  submitTxAnn,
 )
import Test.Cardano.Ledger.Core.KeyPair (mkAddr)
import Test.Cardano.Ledger.Imp.Common
import Test.Cardano.Ledger.Plutus (PlutusArgs (..), ScriptTestContext (..))
import Test.Cardano.Ledger.Plutus.Examples (guessTheNumber3)

spendDatum :: P1.Data
spendDatum = P1.I 3

setupPlutusV1Script :: AlonzoEraScript era => ImpTestM era (ScriptHash (EraCrypto era))
setupPlutusV1Script = do
  impAddPlutusScript
    ScriptTestContext
      { stcScript = guessTheNumber3
      , stcArgs =
          PlutusArgs
            { paSpendDatum = Just spendDatum
            , paData = spendDatum
            }
      }

scriptLockedTxOut ::
  forall era.
  AlonzoEraTxOut era =>
  ScriptHash (EraCrypto era) ->
  TxOut era
scriptLockedTxOut shSpending =
  mkBasicTxOut
    (Addr Testnet (ScriptHashObj shSpending) StakeRefNull)
    (inject $ Coin 1_000_000)
    & dataHashTxOutL .~ SJust (hashData @era $ Data spendDatum)

mkRefTxOut :: BabbageEraTxOut era => ScriptHash (EraCrypto era) -> ImpTestM era (TxOut era)
mkRefTxOut sh = do
  kpPayment <- lookupKeyPair =<< freshKeyHash
  kpStaking <- lookupKeyPair =<< freshKeyHash
  mbyPlutusScript <- impLookupPlutusScript sh
  pure $
    mkBasicTxOut (mkAddr (kpPayment, kpStaking)) (inject $ Coin 100)
      & referenceScriptTxOutL .~ maybeToStrictMaybe (fromPlutusScript <$> mbyPlutusScript)

setupRefTx ::
  forall era.
  ( BabbageEraTxOut era
  , ShelleyEraImp era
  ) =>
  ImpTestM era (TxId (EraCrypto era))
setupRefTx = do
  shSpending <- setupPlutusV1Script
  refTxOut <- mkRefTxOut shSpending
  fmap txIdTx . submitTxAnn "Producing transaction" $
    mkBasicTx mkBasicTxBody
      & bodyTxL . outputsTxBodyL
        .~ SSeq.fromList
          [ refTxOut
          , scriptLockedTxOut shSpending
          , scriptLockedTxOut shSpending
          ]

spec ::
  forall era.
  ( ShelleyEraImp era
  , BabbageEraTxBody era
  , Inject (BabbageContextError era) (ContextError era)
  , InjectRuleFailure "LEDGER" BabbageUtxoPredFailure era
  , InjectRuleFailure "LEDGER" AlonzoUtxosPredFailure era
  ) =>
  SpecWith (ImpTestState era)
spec =
  describe "UTXOS" $ do
    it "can use reference scripts" $ do
      producingTx <- setupRefTx
      referringTx <-
        submitTxAnn "Transaction that refers to the script" $
          mkBasicTx mkBasicTxBody
            & bodyTxL . inputsTxBodyL .~ Set.singleton (mkTxInPartial producingTx 1)
            & bodyTxL . referenceInputsTxBodyL .~ Set.singleton (mkTxInPartial producingTx 0)
      (referringTx ^. witsTxL . scriptTxWitsL) `shouldBe` mempty
    it "can use regular inputs for reference" $ do
      producingTx <- setupRefTx
      referringTx <-
        submitTxAnn "Consuming transaction" $
          mkBasicTx mkBasicTxBody
            & bodyTxL . inputsTxBodyL
              .~ Set.fromList
                [ mkTxInPartial producingTx 0
                , mkTxInPartial producingTx 1
                ]
      (referringTx ^. witsTxL . scriptTxWitsL) `shouldBe` mempty
    it "fails with same txIn in regular inputs and reference inputs" $ do
      producingTx <- setupRefTx
      let
        consumingTx =
          mkBasicTx mkBasicTxBody
            & bodyTxL . inputsTxBodyL
              .~ Set.fromList
                [ mkTxInPartial producingTx 0
                , mkTxInPartial producingTx 1
                ]
            & bodyTxL . referenceInputsTxBodyL .~ Set.singleton (mkTxInPartial producingTx 0)
      _ <-
        submitFailingTx
          consumingTx
          [ injectFailure . BabbageNonDisjointRefInputs $
              mkTxInPartial producingTx 0 :| []
          ]
      pure ()
    it "fails when using inline datums for PlutusV1" $ do
      shSpending <- setupPlutusV1Script
      refTxOut <- mkRefTxOut shSpending
      producingTx <-
        fmap txIdTx . submitTxAnn "Producing transaction" $
          mkBasicTx mkBasicTxBody
            & bodyTxL . outputsTxBodyL
              .~ SSeq.fromList
                [ refTxOut
                , scriptLockedTxOut shSpending & dataTxOutL .~ SJust (Data spendDatum)
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
          [ injectFailure $
              CollectErrors
                [BadTranslation . inject . InlineDatumsNotSupported @era $ TxOutFromInput lockedTxIn]
          ]
