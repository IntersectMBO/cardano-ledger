{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.Ledger.Alonzo.Imp.UtxosSpec (spec) where

import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.Alonzo.Core (
  AlonzoEraScript (..),
  AlonzoEraTx (..),
  AlonzoEraTxWits (..),
  AsIx (AsIx),
  Era (..),
  ppMaxTxExUnitsL,
 )
import Cardano.Ledger.Alonzo.Tx (IsValid (..))
import Cardano.Ledger.Alonzo.TxWits (Redeemers (..))
import Cardano.Ledger.BaseTypes (Inject (..), Network (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core (EraTx (..), EraTxBody (..), EraTxOut (..))
import Cardano.Ledger.Credential (Credential (..), StakeReference (..))
import Cardano.Ledger.Plutus.Data (Data (..))
import Cardano.Ledger.Plutus.Language (SLanguage (..), hashPlutusScript)
import Cardano.Ledger.Shelley.LedgerState (curPParamsEpochStateL, nesEsL)
import Cardano.Ledger.TxIn (TxIn)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as SSeq
import qualified Data.Set as Set
import Lens.Micro ((&), (.~))
import qualified PlutusLedgerApi.Common as P
import Test.Cardano.Ledger.Alonzo.ImpTest (
  ImpTestM,
  ImpTestState,
  ShelleyEraImp,
  getsNES,
  impAnn,
  submitTxAnn,
  submitTxAnn_,
  submitTx_,
 )
import Test.Cardano.Ledger.Common
import Test.Cardano.Ledger.Core.Utils (txInAt)
import Test.Cardano.Ledger.Plutus.Examples (guessTheNumber3)

submitProducingTx :: forall era. ShelleyEraImp era => ImpTestM era (TxIn (EraCrypto era))
submitProducingTx =
  fmap (txInAt (0 :: Int)) . submitTxAnn "Sumbit a transaction with a script output" $
    mkBasicTx mkBasicTxBody
      & bodyTxL . outputsTxBodyL
        .~ SSeq.singleton
          ( mkBasicTxOut
              (Addr Testnet (ScriptHashObj $ hashPlutusScript (guessTheNumber3 SPlutusV1)) StakeRefNull)
              (inject (Coin 100))
          )

spec ::
  forall era.
  ( ShelleyEraImp era
  , AlonzoEraTx era
  ) =>
  SpecWith (ImpTestState era)
spec = describe "UTXOS" $ do
  it "Plutus script transactions are fixed up" $ do
    txIn0 <- submitProducingTx
    submitTxAnn_ "Submit a transaction that consumes the script output" $
      mkBasicTx mkBasicTxBody
        & bodyTxL . inputsTxBodyL
          .~ Set.singleton txIn0
  it "Invalid plutus script fails in phase 2" $ do
    txIn0 <- submitProducingTx
    exUnits <- getsNES $ nesEsL . curPParamsEpochStateL . ppMaxTxExUnitsL
    impAnn "Submitting consuming transaction" $
      submitTx_
        ( mkBasicTx mkBasicTxBody
            & bodyTxL . inputsTxBodyL .~ Set.singleton txIn0
            & isValidTxL .~ IsValid False
            & witsTxL . rdmrsTxWitsL
              .~ Redeemers
                ( Map.singleton
                    (mkSpendingPurpose $ AsIx 0)
                    (Data $ P.I 32, exUnits)
                )
        )
