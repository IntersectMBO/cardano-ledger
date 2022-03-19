{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Babbage.Examples.Consensus where

import Cardano.Ledger.Alonzo.Data (AuxiliaryData (..), AuxiliaryDataHash (..), Data (..), dataToBinaryData, hashData)
import Cardano.Ledger.Alonzo.Language (Language (..))
import Cardano.Ledger.Alonzo.Scripts (ExUnits (..), Script (..))
import qualified Cardano.Ledger.Alonzo.Scripts as Tag (Tag (..))
import Cardano.Ledger.Alonzo.Translation ()
import Cardano.Ledger.Alonzo.Tx (IsValid (..), ValidatedTx (..))
import Cardano.Ledger.Alonzo.TxWitness (RdmrPtr (..), Redeemers (..), TxDats (..), TxWitness (..))
import Cardano.Ledger.Babbage (BabbageEra)
import Cardano.Ledger.Babbage.PParams (PParams' (..), emptyPParams, emptyPParamsUpdate)
import Cardano.Ledger.Babbage.Translation ()
import Cardano.Ledger.Babbage.TxBody (Datum (..), TxBody (..), TxOut (..))
import Cardano.Ledger.BaseTypes (StrictMaybe (..))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core (TxBody)
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Era (ValidateScript (hashScript))
import Cardano.Ledger.Keys (asWitness)
import qualified Cardano.Ledger.Mary.Value as Mary
import Cardano.Ledger.SafeHash (hashAnnotated)
import Cardano.Ledger.Shelley.API
  ( ApplyTxError (..),
    Credential (..),
    Network (..),
    NewEpochState (..),
    ProposedPPUpdates (..),
    RewardAcnt (..),
    TxId (..),
    Update (..),
    Wdrl (..),
  )
import Cardano.Ledger.Shelley.Rules.Delegs (DelegsPredicateFailure (..))
import Cardano.Ledger.Shelley.Rules.Ledger (LedgerPredicateFailure (..))
import Cardano.Ledger.Shelley.Tx (Tx (..))
import Cardano.Ledger.Shelley.UTxO (makeWitnessesVKey)
import Cardano.Ledger.ShelleyMA.Timelocks (Timelock (..), ValidityInterval (..))
import Cardano.Ledger.TxIn (mkTxInPartial)
import Cardano.Slotting.Slot (EpochNo (..), SlotNo (..))
import Data.Default.Class (def)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import qualified PlutusTx as Plutus
import Test.Cardano.Ledger.Alonzo.Scripts (alwaysFails, alwaysSucceeds)
import qualified Test.Cardano.Ledger.Mary.Examples.Consensus as SLE
import qualified Test.Cardano.Ledger.Shelley.Examples.Consensus as SLE
import Test.Cardano.Ledger.Shelley.Orphans ()
import Test.Cardano.Ledger.Shelley.Utils (mkAddr)

type StandardBabbage = BabbageEra StandardCrypto

-- | ShelleyLedgerExamples for Babbage era
ledgerExamplesBabbage :: SLE.ShelleyLedgerExamples StandardBabbage
ledgerExamplesBabbage =
  SLE.ShelleyLedgerExamples
    { SLE.sleBlock = SLE.exampleShelleyLedgerBlock exampleTransactionInBlock,
      SLE.sleHashHeader = SLE.exampleHashHeader (Proxy @StandardBabbage),
      SLE.sleTx = exampleTransactionInBlock,
      SLE.sleApplyTxError =
        ApplyTxError $
          pure $
            DelegsFailure $
              DelegateeNotRegisteredDELEG @StandardBabbage (SLE.mkKeyHash 1),
      SLE.sleRewardsCredentials =
        Set.fromList
          [ Left (Coin 100),
            Right (ScriptHashObj (SLE.mkScriptHash 1)),
            Right (KeyHashObj (SLE.mkKeyHash 2))
          ],
      SLE.sleResultExamples = resultExamples,
      SLE.sleNewEpochState = exampleBabbageNewEpochState,
      SLE.sleChainDepState = SLE.exampleLedgerChainDepState 1,
      SLE.sleTranslationContext = ()
    }
  where
    resultExamples =
      SLE.ShelleyResultExamples
        { SLE.srePParams = def,
          SLE.sreProposedPPUpdates = examplePPPU,
          SLE.srePoolDistr = SLE.examplePoolDistr,
          SLE.sreNonMyopicRewards = SLE.exampleNonMyopicRewards,
          SLE.sreShelleyGenesis = SLE.testShelleyGenesis
        }
    examplePPPU =
      ProposedPPUpdates $
        Map.singleton
          (SLE.mkKeyHash 0)
          (emptyPParamsUpdate {_collateralPercentage = SJust 150})

collateralOutput :: TxOut StandardBabbage
collateralOutput =
  TxOut
    (mkAddr (SLE.examplePayKey, SLE.exampleStakeKey))
    (Mary.Value 8675309 mempty)
    NoDatum
    SNothing

exampleTxBodyBabbage :: Cardano.Ledger.Core.TxBody StandardBabbage
exampleTxBodyBabbage =
  TxBody
    (Set.fromList [mkTxInPartial (TxId (SLE.mkDummySafeHash Proxy 1)) 0]) -- spending inputs
    (Set.fromList [mkTxInPartial (TxId (SLE.mkDummySafeHash Proxy 2)) 1]) -- collateral inputs
    (Set.fromList [mkTxInPartial (TxId (SLE.mkDummySafeHash Proxy 1)) 3]) -- reference inputs
    ( StrictSeq.fromList
        [ TxOut
            (mkAddr (SLE.examplePayKey, SLE.exampleStakeKey))
            (SLE.exampleMultiAssetValue 2)
            (Datum $ dataToBinaryData datumExample) -- inline datum
            (SJust $ alwaysSucceeds PlutusV2 3) -- reference script
        ]
    )
    (SJust collateralOutput) -- collateral return
    (Coin 8675309) -- collateral tot
    SLE.exampleCerts -- txcerts
    ( Wdrl $
        Map.singleton
          (RewardAcnt Testnet (SLE.keyToCredential SLE.exampleStakeKey))
          (Coin 100) -- txwdrls
    )
    (Coin 999) -- txfee
    (ValidityInterval (SJust (SlotNo 2)) (SJust (SlotNo 4))) -- txvldt
    ( SJust $
        Update
          ( ProposedPPUpdates $
              Map.singleton
                (SLE.mkKeyHash 1)
                (emptyPParamsUpdate {_maxBHSize = SJust 4000})
          )
          (EpochNo 0)
    ) -- txUpdates
    (Set.singleton $ SLE.mkKeyHash 212) -- reqSignerHashes
    (SLE.exampleMultiAssetValue 3) -- mint
    (SJust $ SLE.mkDummySafeHash (Proxy @StandardCrypto) 42) -- scriptIntegrityHash
    (SJust . AuxiliaryDataHash $ SLE.mkDummySafeHash (Proxy @StandardCrypto) 42) -- adHash
    (SJust Mainnet) -- txnetworkid

datumExample :: Data StandardBabbage
datumExample = Data (Plutus.I 191)

redeemerExample :: Data StandardBabbage
redeemerExample = Data (Plutus.I 919)

exampleTx :: Tx StandardBabbage
exampleTx =
  Tx
    exampleTxBodyBabbage
    ( TxWitness
        (makeWitnessesVKey (hashAnnotated exampleTxBodyBabbage) [asWitness SLE.examplePayKey]) -- vkey
        mempty -- bootstrap
        ( Map.singleton
            (hashScript @StandardBabbage $ alwaysSucceeds PlutusV1 3)
            (alwaysSucceeds PlutusV1 3) -- txscripts
        )
        (TxDats $ Map.singleton (hashData datumExample) datumExample)
        ( Redeemers $
            Map.singleton (RdmrPtr Tag.Spend 0) (redeemerExample, ExUnits 5000 5000)
        ) -- redeemers
    )
    ( SJust $
        AuxiliaryData
          SLE.exampleMetadataMap -- metadata
          ( StrictSeq.fromList
              [alwaysFails PlutusV1 2, TimelockScript $ RequireAllOf mempty] -- Scripts
          )
    )

exampleTransactionInBlock :: ValidatedTx StandardBabbage
exampleTransactionInBlock = ValidatedTx b w (IsValid True) a
  where
    (Tx b w a) = exampleTx

exampleBabbageNewEpochState :: NewEpochState StandardBabbage
exampleBabbageNewEpochState =
  SLE.exampleNewEpochState
    (SLE.exampleMultiAssetValue 1)
    emptyPParams
    (emptyPParams {_coinsPerUTxOWord = Coin 1})
