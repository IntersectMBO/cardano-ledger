{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Alonzo.Imp.UtxosSpec (spec) where

import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.Alonzo.Core (AlonzoEraTxOut (..), AlonzoEraTxWits (..))
import Cardano.Ledger.BaseTypes (Inject (..), Network (..), StrictMaybe (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core (EraTx (..), EraTxBody (..), EraTxOut (..))
import Cardano.Ledger.Credential (Credential (..), StakeReference (..))
import Cardano.Ledger.Plutus.Data (Data (..), hashData)
import qualified Data.Sequence.Strict as SSeq
import qualified Data.Set as Set
import Lens.Micro ((&), (.~))
import qualified PlutusLedgerApi.V1 as P
import Test.Cardano.Ledger.Alonzo.ImpTest (
  ImpTestState,
  PlutusArgs (..),
  ShelleyEraImp,
  impAddPlutusScript,
  submitTxAnn,
  submitTxAnn_,
 )
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.Utils (txInAt)
import Test.Cardano.Ledger.Plutus (ScriptTestContext (..))
import Test.Cardano.Ledger.Plutus.Examples (guessTheNumber3)

spec ::
  forall era.
  ( ShelleyEraImp era
  , AlonzoEraTxWits era
  , AlonzoEraTxOut era
  ) =>
  SpecWith (ImpTestState era)
spec = describe "UTXOS" $ do
  it "Plutus script transactions are fixed up" $ do
    let spendDatum = P.I 3
    shSpending <-
      impAddPlutusScript
        ScriptTestContext
          { stcScript = guessTheNumber3
          , stcArgs =
              PlutusArgs
                { paSpendDatum = Just spendDatum
                , paData = P.I 3
                }
          }
    tx0 <-
      submitTxAnn "Sumbit a transaction with a script output" $
        mkBasicTx mkBasicTxBody
          & bodyTxL . outputsTxBodyL
            .~ SSeq.singleton
              ( mkBasicTxOut
                  (Addr Testnet (ScriptHashObj shSpending) StakeRefNull)
                  (inject (Coin 100))
                  & dataHashTxOutL .~ SJust (hashData @era $ Data spendDatum)
              )
    submitTxAnn_ "Submit a transaction that consumes the script output" $
      mkBasicTx mkBasicTxBody
        & bodyTxL . inputsTxBodyL
          .~ Set.singleton (txInAt (0 :: Int) tx0)
