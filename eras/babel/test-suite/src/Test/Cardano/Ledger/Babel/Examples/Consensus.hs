{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.Babel.Examples.Consensus where

import Cardano.Ledger.Allegra.Scripts (Timelock (..))
import Cardano.Ledger.Alonzo.Scripts (AlonzoScript (..), ExUnits (..))
import Cardano.Ledger.Alonzo.Tx (IsValid (..))
import Cardano.Ledger.Alonzo.TxAuxData (
  AuxiliaryDataHash (..),
  mkAlonzoTxAuxData,
 )
import Cardano.Ledger.Alonzo.TxWits (Redeemers (..), TxDats (..))
import Cardano.Ledger.Babbage.TxBody (BabbageTxOut (..))
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Binary (mkSized)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Babel (Babel)
import Cardano.Ledger.Babel.Core
import Cardano.Ledger.Babel.Genesis (BabelGenesis (..))
import Cardano.Ledger.Babel.Governance (VotingProcedures (..))
import Cardano.Ledger.Babel.Rules (BabelCERTS, BabelCertsPredFailure (..), BabelLEDGER)
import Cardano.Ledger.Babel.Scripts (BabelPlutusPurpose (..))
import Cardano.Ledger.Babel.Translation ()
import Cardano.Ledger.Babel.Tx (AlonzoTx (..))
import Cardano.Ledger.Babel.TxBody (BabelTxBody (..))
import Cardano.Ledger.Babel.TxCert
import Cardano.Ledger.Babel.TxWits (AlonzoTxWits (..))
import Cardano.Ledger.Credential (Credential (KeyHashObj, ScriptHashObj))
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Keys (asWitness)
import Cardano.Ledger.Mary.Value (MaryValue (..))
import Cardano.Ledger.Plutus.Data (
  Data (..),
  Datum (..),
  dataToBinaryData,
  hashData,
 )
import Cardano.Ledger.Plutus.Language (Language (..))
import Cardano.Ledger.SafeHash (hashAnnotated)
import Cardano.Ledger.Shelley.API (
  ApplyTxError (..),
  NewEpochState (..),
  ProposedPPUpdates (..),
  RewardAccount (..),
  TxId (..),
 )
import Cardano.Ledger.Shelley.Tx (ShelleyTx (..))
import Cardano.Ledger.TxIn (mkTxInPartial)
import Control.State.Transition.Extended (Embed (..))
import Data.Default.Class (Default (def))
import qualified Data.Map.Strict as Map
import qualified Data.OSet.Strict as OSet
import Data.Proxy (Proxy (..))
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Lens.Micro
import qualified PlutusLedgerApi.Common as P
import Test.Cardano.Ledger.Alonzo.Arbitrary (alwaysFails, alwaysSucceeds)
import Test.Cardano.Ledger.Babel.Genesis (expectedBabelGenesis)
import Test.Cardano.Ledger.Core.KeyPair (mkAddr, mkWitnessesVKey)
import Test.Cardano.Ledger.Core.Utils (mkDummySafeHash)
import qualified Test.Cardano.Ledger.Mary.Examples.Consensus as MarySLE
import Test.Cardano.Ledger.Shelley.Examples.Consensus (examplePoolParams)
import qualified Test.Cardano.Ledger.Shelley.Examples.Consensus as SLE

-- ==============================================================

-- | ShelleyLedgerExamples for Babel era
ledgerExamplesBabel ::
  SLE.ShelleyLedgerExamples Babel
ledgerExamplesBabel =
  SLE.ShelleyLedgerExamples
    { SLE.sleBlock = SLE.exampleShelleyLedgerBlock exampleTransactionInBlock
    , SLE.sleHashHeader = SLE.exampleHashHeader (Proxy @Babel)
    , SLE.sleTx = exampleTransactionInBlock
    , SLE.sleApplyTxError =
        ApplyTxError $
          pure $
            wrapFailed @(BabelCERTS Babel) @(BabelLEDGER Babel) $
              DelegateeNotRegisteredDELEG @Babel (SLE.mkKeyHash 1)
    , SLE.sleRewardsCredentials =
        Set.fromList
          [ Left (Coin 100)
          , Right (ScriptHashObj (SLE.mkScriptHash 1))
          , Right (KeyHashObj (SLE.mkKeyHash 2))
          ]
    , SLE.sleResultExamples = resultExamples
    , SLE.sleNewEpochState = exampleBabelNewEpochState
    , SLE.sleChainDepState = SLE.exampleLedgerChainDepState 1
    , SLE.sleTranslationContext = exampleBabelGenesis
    }
  where
    resultExamples =
      SLE.ShelleyResultExamples
        { SLE.srePParams = def
        , SLE.sreProposedPPUpdates = examplePPPU
        , SLE.srePoolDistr = SLE.examplePoolDistr
        , SLE.sreNonMyopicRewards = SLE.exampleNonMyopicRewards
        , SLE.sreShelleyGenesis = SLE.testShelleyGenesis
        }
    examplePPPU =
      ProposedPPUpdates $
        Map.singleton
          (SLE.mkKeyHash 0)
          (emptyPParamsUpdate & ppuCollateralPercentageL .~ SJust 150)

collateralOutput :: BabbageTxOut Babel
collateralOutput =
  BabbageTxOut
    (mkAddr (SLE.examplePayKey, SLE.exampleStakeKey))
    (MaryValue (Coin 8675309) mempty)
    NoDatum
    SNothing

exampleBabelCerts :: Era era => OSet.OSet (BabelTxCert era)
exampleBabelCerts =
  OSet.fromList -- TODO should I add the new certs here?
    [ BabelTxCertPool (RegPool examplePoolParams)
    ]

exampleTxBodyBabel :: TxBody Babel
exampleTxBodyBabel =
  BabelTxBody
    (Set.fromList [mkTxInPartial (TxId (mkDummySafeHash Proxy 1)) 0]) -- spending inputs
    (Set.fromList [mkTxInPartial (TxId (mkDummySafeHash Proxy 2)) 1]) -- collateral inputs
    (Set.fromList [mkTxInPartial (TxId (mkDummySafeHash Proxy 1)) 3]) -- reference inputs
    ( StrictSeq.fromList
        [ mkSized (eraProtVerHigh @Babel) $
            BabbageTxOut
              (mkAddr (SLE.examplePayKey, SLE.exampleStakeKey))
              (MarySLE.exampleMultiAssetValue 2)
              (Datum $ dataToBinaryData datumExample) -- inline datum
              (SJust $ alwaysSucceeds @'PlutusV2 3) -- reference script
        ]
    )
    (SJust $ mkSized (eraProtVerHigh @Babel) collateralOutput) -- collateral return
    (SJust $ Coin 8675309) -- collateral tot
    exampleBabelCerts -- txcerts
    ( Withdrawals $
        Map.singleton
          (RewardAccount Testnet (SLE.keyToCredential SLE.exampleStakeKey))
          (Coin 100) -- txwdrls
    )
    (Coin 999) -- txfee
    (ValidityInterval (SJust (SlotNo 2)) (SJust (SlotNo 4))) -- txvldt
    (Set.singleton $ SLE.mkKeyHash 212) -- reqSignerHashes
    exampleMultiAsset -- mint
    (SJust $ mkDummySafeHash (Proxy @StandardCrypto) 42) -- scriptIntegrityHash
    (SJust . AuxiliaryDataHash $ mkDummySafeHash (Proxy @StandardCrypto) 42) -- adHash
    (SJust Mainnet) -- txnetworkid
    (VotingProcedures mempty)
    mempty
    (SJust $ Coin 867530900000) -- current treasury value
    mempty
    mempty
    mempty
    mempty
  where
    MaryValue _ exampleMultiAsset = MarySLE.exampleMultiAssetValue 3

datumExample :: Data Babel
datumExample = Data (P.I 191)

redeemerExample :: Data Babel
redeemerExample = Data (P.I 919)

exampleTx :: ShelleyTx Babel
exampleTx =
  ShelleyTx
    exampleTxBodyBabel
    ( AlonzoTxWits
        (mkWitnessesVKey (hashAnnotated exampleTxBodyBabel) [asWitness SLE.examplePayKey]) -- vkey
        mempty -- bootstrap
        ( Map.singleton
            (hashScript @Babel $ alwaysSucceeds @'PlutusV1 3)
            (alwaysSucceeds @'PlutusV1 3) -- txscripts
        )
        (TxDats $ Map.singleton (hashData datumExample) datumExample)
        ( Redeemers $
            Map.singleton (BabelSpending $ AsIx 0) (redeemerExample, ExUnits 5000 5000)
        ) -- redeemers
    )
    ( SJust $
        mkAlonzoTxAuxData
          SLE.exampleAuxDataMap -- metadata
          [alwaysFails @'PlutusV1 2, TimelockScript $ RequireAllOf mempty] -- Scripts
    )

exampleTransactionInBlock :: AlonzoTx Babel
exampleTransactionInBlock = AlonzoTx b w (IsValid True) a -- mempty
  where
    ShelleyTx b w a = exampleTx

exampleBabelNewEpochState :: NewEpochState Babel
exampleBabelNewEpochState =
  SLE.exampleNewEpochState
    (MarySLE.exampleMultiAssetValue 1)
    emptyPParams
    (emptyPParams & ppCoinsPerUTxOByteL .~ CoinPerByte (Coin 1))

exampleBabelGenesis :: BabelGenesis StandardCrypto
exampleBabelGenesis = expectedBabelGenesis
