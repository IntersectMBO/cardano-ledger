{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Cardano.Ledger.Examples.TwoPhaseValidation where

import qualified Cardano.Crypto.Hash as CH
import Cardano.Crypto.Hash.Class (sizeHash)
import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.Alonzo.Data (Data (..), hashData)
import Cardano.Ledger.Alonzo.Language (Language (..))
import Cardano.Ledger.Alonzo.PParams (AlonzoPParamsHKD (..))
import Cardano.Ledger.Alonzo.PlutusScriptApi (CollectError (..), collectTwoPhaseScriptInputs)
import Cardano.Ledger.Alonzo.Rules
  ( AlonzoBBODY,
    AlonzoBbodyPredFailure (..),
    AlonzoUtxoPredFailure (..),
    AlonzoUtxosPredFailure (..),
    AlonzoUtxowPredFailure (..),
    FailureDescription (..),
    TagMismatchDescription (..),
  )
import Cardano.Ledger.Alonzo.Scripts
  ( AlonzoScript (..),
    CostModel,
    CostModels (..),
    ExUnits (..),
    mkCostModel,
  )
import qualified Cardano.Ledger.Alonzo.Scripts as Tag (Tag (..))
import Cardano.Ledger.Alonzo.Tx
  ( AlonzoTx (..),
    IsValid (..),
    ScriptPurpose (..),
    minfee,
  )
import Cardano.Ledger.Alonzo.TxInfo (TranslationError, VersionedTxInfo, txInfo, valContext)
import Cardano.Ledger.Alonzo.TxWitness (RdmrPtr (..), Redeemers (..), TxDats (..), unRedeemers)
import Cardano.Ledger.BHeaderView (BHeaderView (..))
import qualified Cardano.Ledger.Babbage.PParams as Babbage (BabbagePParamsHKD (..))
import Cardano.Ledger.Babbage.Rules (BabbageUtxoPredFailure (..))
import Cardano.Ledger.Babbage.Rules as Babbage (BabbageUtxowPredFailure (..))
import Cardano.Ledger.BaseTypes
  ( BlocksMade (..),
    Network (..),
    StrictMaybe (..),
    TxIx,
    mkTxIxPartial,
    textToUrl,
  )
import Cardano.Ledger.Block (Block (..), txid)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core hiding (TranslationError)
import Cardano.Ledger.Credential
  ( Credential (..),
    StakeCredential,
    StakeReference (..),
  )
import qualified Cardano.Ledger.Crypto as CC
import Cardano.Ledger.Keys
  ( GenDelegs (..),
    KeyHash,
    KeyPair (..),
    KeyRole (..),
    asWitness,
    coerceKeyRole,
    hashKey,
    hashVerKeyVRF,
  )
import Cardano.Ledger.Pretty
import Cardano.Ledger.Pretty.Babbage ()
import Cardano.Ledger.SafeHash (hashAnnotated)
import Cardano.Ledger.Shelley.API
  ( CLI (..),
    DPState (..),
    DState (..),
    LedgerState (..),
    PoolParams (..),
    ProtVer (..),
    UTxO (..),
  )
import Cardano.Ledger.Shelley.BlockChain (bBodySize)
import Cardano.Ledger.Shelley.LedgerState
  ( UTxOState (..),
    smartUTxOState,
  )
import Cardano.Ledger.Shelley.PParams (ShelleyPParamsHKD (..))
import Cardano.Ledger.Shelley.Rules.Bbody (BbodyEnv (..), ShelleyBbodyPredFailure (..), ShelleyBbodyState (..))
import Cardano.Ledger.Shelley.Rules.Delegs (ShelleyDelegsPredFailure (..))
import Cardano.Ledger.Shelley.Rules.Delpl (ShelleyDelplPredFailure (..))
import Cardano.Ledger.Shelley.Rules.Ledger (ShelleyLedgerPredFailure (..))
import Cardano.Ledger.Shelley.Rules.Ledgers (ShelleyLedgersPredFailure (..))
import Cardano.Ledger.Shelley.Rules.Pool (ShelleyPoolPredFailure (..))
import Cardano.Ledger.Shelley.Rules.Utxo (UtxoEnv (..))
import Cardano.Ledger.Shelley.Rules.Utxow as Shelley (ShelleyUtxowPredFailure (..))
import Cardano.Ledger.Shelley.TxBody
  ( DCert (..),
    DelegCert (..),
    PoolCert (..),
    PoolMetadata (..),
    RewardAcnt (..),
    Wdrl (..),
  )
import Cardano.Ledger.Shelley.UTxO (makeWitnessVKey)
import Cardano.Ledger.ShelleyMA.Timelocks (ValidityInterval (..))
import Cardano.Ledger.TxIn (TxIn (..))
import Cardano.Ledger.Val (inject, (<+>))
import Cardano.Slotting.EpochInfo (EpochInfo, fixedEpochInfo)
import Cardano.Slotting.Slot (EpochSize (..), SlotNo (..))
import Cardano.Slotting.Time (SystemStart (..), mkSlotLength)
import Control.State.Transition.Extended hiding (Assertion)
import qualified Data.ByteString as BS (replicate)
import Data.ByteString.Short (ShortByteString)
import Data.Default.Class (Default (..))
import Data.Either (fromRight)
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.UMap (View (Rewards))
import qualified Data.UMap as UM
import GHC.Stack
import Numeric.Natural (Natural)
import qualified PlutusLedgerApi.V1 as Plutus
import PlutusLedgerApi.Test.EvaluationContext (costModelParamsForTesting)
import Test.Cardano.Ledger.Generic.Fields
  ( PParamsField (..),
    TxBodyField (..),
    TxField (..),
    TxOutField (..),
    WitnessesField (..),
  )
import Test.Cardano.Ledger.Generic.Indexed (theKeyPair)
import Test.Cardano.Ledger.Generic.PrettyCore ()
import Test.Cardano.Ledger.Generic.Proof
import Test.Cardano.Ledger.Generic.Scriptic (HasTokens (..), PostShelley, Scriptic (..), after, matchkey)
import Test.Cardano.Ledger.Generic.Updaters
import Test.Cardano.Ledger.Shelley.Generator.EraGen (genesisId)
import Test.Cardano.Ledger.Shelley.Utils
  ( RawSeed (..),
    mkKeyPair,
    mkVRFKeyPair,
  )
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))

-- =======================
-- Setup the initial state
-- =======================

testEpochInfo :: EpochInfo (Either Text)
testEpochInfo = fixedEpochInfo (EpochSize 100) (mkSlotLength 1)

testSystemStart :: SystemStart
testSystemStart = SystemStart $ posixSecondsToUTCTime 0

-- | A cost model that sets everything as being free
freeCostModelV1 :: CostModel
freeCostModelV1 =
  fromRight (error "corrupt freeCostModelV1") $
    mkCostModel PlutusV1 (0 <$ costModelParamsForTesting)

-- | A cost model that sets everything as being free
freeCostModelV2 :: CostModel
freeCostModelV2 =
  fromRight (error "corrupt freeCostModelV1") $
    mkCostModel PlutusV1 (0 <$ costModelParamsForTesting) -- TODO use PV2 when it exists

defaultPPs :: [PParamsField era]
defaultPPs =
  [ Costmdls . CostModels $ Map.singleton PlutusV1 freeCostModelV1,
    MaxValSize 1000000000,
    MaxTxExUnits $ ExUnits 1000000 1000000,
    MaxBlockExUnits $ ExUnits 1000000 1000000,
    ProtocolVersion $ ProtVer 5 0,
    CollateralPercentage 100
  ]

utxoEnv :: PParams era -> UtxoEnv era
utxoEnv pparams =
  UtxoEnv
    (SlotNo 0)
    pparams
    mempty
    (GenDelegs mempty)

-- | Create an address with a given payment script.
-- The proof here is used only as a Proxy.
scriptAddr :: forall era. (Scriptic era) => Script era -> Proof era -> Addr (Crypto era)
scriptAddr s _pf = Addr Testnet pCred sCred
  where
    pCred = ScriptHashObj . hashScript @era $ s
    (_ssk, svk) = mkKeyPair @(Crypto era) (RawSeed 0 0 0 0 0)
    sCred = StakeRefBase . KeyHashObj . hashKey $ svk

someKeys :: forall era. Era era => Proof era -> KeyPair 'Payment (Crypto era)
someKeys _pf = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair @(Crypto era) (RawSeed 1 1 1 1 1)

someAddr :: forall era. Era era => Proof era -> Addr (Crypto era)
someAddr pf = Addr Testnet pCred sCred
  where
    (_ssk, svk) = mkKeyPair @(Crypto era) (RawSeed 0 0 0 0 2)
    pCred = KeyHashObj . hashKey . vKey $ someKeys pf
    sCred = StakeRefBase . KeyHashObj . hashKey $ svk

someOutput :: EraTxOut era => Proof era -> TxOut era
someOutput pf =
  newTxOut pf [Address $ someAddr pf, Amount (inject $ Coin 1000)]

nonScriptOutWithDatum :: forall era. EraTxOut era => Proof era -> TxOut era
nonScriptOutWithDatum pf =
  newTxOut
    pf
    [ Address (someAddr pf),
      Amount (inject $ Coin 1221),
      DHash' [hashData $ datumExample1 @era]
    ]

mkGenesisTxIn :: (CH.HashAlgorithm (CC.HASH crypto), HasCallStack) => Integer -> TxIn crypto
mkGenesisTxIn = TxIn genesisId . mkTxIxPartial

collateralOutput :: EraTxOut era => Proof era -> TxOut era
collateralOutput pf =
  newTxOut pf [Address $ someAddr pf, Amount (inject $ Coin 5)]

alwaysSucceedsHash ::
  forall era.
  Scriptic era =>
  Natural ->
  Proof era ->
  ScriptHash (Crypto era)
alwaysSucceedsHash n pf = hashScript @era $ always n pf

alwaysFailsHash :: forall era. Scriptic era => Natural -> Proof era -> ScriptHash (Crypto era)
alwaysFailsHash n pf = hashScript @era $ never n pf

timelockScript :: PostShelley era => Int -> Proof era -> Script era
timelockScript s = allOf [matchkey 1, after (100 + s)]

timelockHash ::
  forall era.
  PostShelley era =>
  Int ->
  Proof era ->
  ScriptHash (Crypto era)
timelockHash n pf = hashScript @era $ timelockScript n pf

timelockAddr :: forall era. PostShelley era => Proof era -> Addr (Crypto era)
timelockAddr pf = Addr Testnet pCred sCred
  where
    (_ssk, svk) = mkKeyPair @(Crypto era) (RawSeed 0 0 0 0 2)
    pCred = ScriptHashObj (timelockHash 0 pf)
    sCred = StakeRefBase . KeyHashObj . hashKey $ svk

timelockOut :: (EraTxOut era, PostShelley era) => Proof era -> TxOut era
timelockOut pf =
  newTxOut pf [Address $ timelockAddr pf, Amount (inject $ Coin 1)]

-- | This output is unspendable since it is locked by a plutus script,
--  but has no datum hash.
unspendableOut :: forall era. (EraTxOut era, Scriptic era) => Proof era -> TxOut era
unspendableOut pf =
  newTxOut
    pf
    [ Address (scriptAddr (always 3 pf) pf),
      Amount (inject $ Coin 5000)
    ]

initUTxO :: (EraTxOut era, PostShelley era) => Proof era -> UTxO era
initUTxO pf =
  UTxO $
    Map.fromList $
      [ (mkGenesisTxIn 1, alwaysSucceedsOutput pf),
        (mkGenesisTxIn 2, alwaysFailsOutput pf)
      ]
        ++ map (\i -> (mkGenesisTxIn i, someOutput pf)) [3 .. 8]
        ++ map (\i -> (mkGenesisTxIn i, collateralOutput pf)) [11 .. 18]
        ++ [ (mkGenesisTxIn 100, timelockOut pf),
             (mkGenesisTxIn 101, unspendableOut pf),
             (mkGenesisTxIn 102, alwaysSucceedsOutputV2 pf),
             (mkGenesisTxIn 103, nonScriptOutWithDatum pf)
           ]

initialUtxoSt ::
  (Default (State (EraRule "PPUP" era)), EraTxOut era) =>
  UTxO era ->
  UTxOState era
initialUtxoSt utxo = smartUTxOState utxo (Coin 0) (Coin 0) def

-- | This is a helper type for the expectedUTxO function.
--  ExpectSuccess indicates that we created a valid transaction
--  where the IsValid flag is true.
data Expect era
  = ExpectSuccess (TxBody era) (TxOut era)
  | ExpectSuccessInvalid
  | ExpectFailure

-- | In each of our main eight examples, the UTxO map obtained
-- by applying the transaction is straightforward. This function
-- captures the logic.
--
-- Each example transaction (given a number i) will use
-- (TxIn genesisId (10+i), someOutput) for its' single input,
-- and (TxIn genesisId i, collateralOutput) for its' single collateral output.
--
-- If we expect the transaction script to validate, then
-- the UTxO for (TxIn genesisId i) will be consumed and a UTxO will be created.
-- Otherwise, the UTxO for (TxIn genesisId (10+i)) will be consumed.
expectedUTxO ::
  forall era.
  (HasCallStack, EraTxBody era) =>
  UTxO era ->
  Expect era ->
  Integer ->
  UTxO era
expectedUTxO initUtxo ex idx = UTxO utxo
  where
    utxo = case ex of
      ExpectSuccess txb newOut ->
        Map.insert (TxIn (txid txb) minBound) newOut (filteredUTxO (mkTxIxPartial idx))
      ExpectSuccessInvalid -> filteredUTxO (mkTxIxPartial idx)
      ExpectFailure -> filteredUTxO (mkTxIxPartial (10 + idx))
    filteredUTxO :: TxIx -> Map.Map (TxIn (Crypto era)) (TxOut era)
    filteredUTxO x = Map.filterWithKey (\(TxIn _ i) _ -> i /= x) $ unUTxO initUtxo

expectedUTxO' ::
  (HasCallStack, EraTxBody era, PostShelley era) =>
  Proof era ->
  Expect era ->
  Integer ->
  UTxO era
expectedUTxO' pf ex idx = expectedUTxO (initUTxO pf) ex idx

keyBy :: Ord k => (a -> k) -> [a] -> Map k a
keyBy f xs = Map.fromList $ (\x -> (f x, x)) <$> xs

pp :: Proof era -> PParams era
pp pf = newPParams pf defaultPPs

-- =========================================================================
--  Example 1: Process a SPEND transaction with a succeeding Plutus script.
-- =========================================================================

datumExample1 :: Data era
datumExample1 = Data (Plutus.I 123)

redeemerExample1 :: Data era
redeemerExample1 = Data (Plutus.I 42)

txDatsExample1 :: Era era => TxDats era
txDatsExample1 = TxDats $ keyBy hashData [datumExample1]

alwaysSucceedsOutput :: forall era. (EraTxOut era, Scriptic era) => Proof era -> TxOut era
alwaysSucceedsOutput pf =
  newTxOut
    pf
    [ Address (scriptAddr (always 3 pf) pf),
      Amount (inject $ Coin 5000),
      DHash' [hashData $ datumExample1 @era]
    ]

alwaysSucceedsOutputV2 :: forall era. (EraTxOut era, Scriptic era) => Proof era -> TxOut era
alwaysSucceedsOutputV2 pf =
  newTxOut
    pf
    [ Address (scriptAddr (alwaysAlt 3 pf) pf),
      Amount (inject $ Coin 5000),
      DHash' [hashData $ datumExample1 @era]
    ]

validatingRedeemersEx1 :: Era era => Redeemers era
validatingRedeemersEx1 =
  Redeemers $
    Map.singleton (RdmrPtr Tag.Spend 0) (redeemerExample1, ExUnits 5000 5000)

extraRedeemersEx :: Era era => Redeemers era
extraRedeemersEx =
  Redeemers $
    Map.insert (RdmrPtr Tag.Spend 7) (redeemerExample1, ExUnits 432 444) (unRedeemers validatingRedeemersEx1)

extraRedeemersBody :: EraTxBody era => Proof era -> TxBody era
extraRedeemersBody pf =
  newTxBody
    pf
    [ Inputs' [mkGenesisTxIn 1],
      Collateral' [mkGenesisTxIn 11],
      Outputs' [outEx1 pf],
      Txfee (Coin 5),
      WppHash (newScriptIntegrityHash pf (pp pf) [PlutusV1] extraRedeemersEx txDatsExample1)
    ]

extraRedeemersTx ::
  forall era.
  ( Scriptic era,
    EraTx era,
    GoodCrypto (Crypto era)
  ) =>
  Proof era ->
  Tx era
extraRedeemersTx pf =
  newTx
    pf
    [ Body (extraRedeemersBody pf),
      WitnessesI
        [ AddrWits' [makeWitnessVKey (hashAnnotated (extraRedeemersBody pf)) (someKeys pf)],
          ScriptWits' [always 3 pf],
          DataWits' [datumExample1],
          RdmrWits extraRedeemersEx
        ]
    ]

outEx1 :: EraTxOut era => Proof era -> TxOut era
outEx1 pf = newTxOut pf [Address (someAddr pf), Amount (inject $ Coin 4995)]

validatingBody :: (Scriptic era, EraTxBody era) => Proof era -> TxBody era
validatingBody pf =
  newTxBody
    pf
    [ Inputs' [mkGenesisTxIn 1],
      Collateral' [mkGenesisTxIn 11],
      Outputs' [outEx1 pf],
      Txfee (Coin 5),
      WppHash (newScriptIntegrityHash pf (pp pf) [PlutusV1] validatingRedeemersEx1 txDatsExample1)
    ]

validatingTx ::
  forall era.
  ( Scriptic era,
    EraTx era,
    GoodCrypto (Crypto era)
  ) =>
  Proof era ->
  Tx era
validatingTx pf =
  newTx
    pf
    [ Body (validatingBody pf),
      WitnessesI
        [ AddrWits' [makeWitnessVKey (hashAnnotated (validatingBody pf)) (someKeys pf)],
          ScriptWits' [always 3 pf],
          DataWits' [datumExample1],
          RdmrWits validatingRedeemersEx1
        ]
    ]

utxoEx1 :: forall era. (PostShelley era, EraTxBody era) => Proof era -> UTxO era
utxoEx1 pf = expectedUTxO' pf (ExpectSuccess (validatingBody pf) (outEx1 pf)) 1

utxoStEx1 ::
  forall era.
  (Default (State (EraRule "PPUP" era)), EraTxBody era, PostShelley era) =>
  Proof era ->
  UTxOState era
utxoStEx1 pf = smartUTxOState (utxoEx1 pf) (Coin 0) (Coin 5) def

-- ======================================================================
--  Example 2: Process a SPEND transaction with a failing Plutus script.
-- ======================================================================

datumExample2 :: Data era
datumExample2 = Data (Plutus.I 0)

redeemerExample2 :: Data era
redeemerExample2 = Data (Plutus.I 1)

txDatsExample2 :: Era era => TxDats era
txDatsExample2 = TxDats $ keyBy hashData $ [datumExample2]

notValidatingRedeemers :: Era era => Redeemers era
notValidatingRedeemers =
  Redeemers
    ( Map.fromList
        [ ( RdmrPtr Tag.Spend 0,
            (redeemerExample2, ExUnits 5000 5000)
          )
        ]
    )

alwaysFailsOutput :: forall era. (Scriptic era, EraTxOut era) => Proof era -> TxOut era
alwaysFailsOutput pf =
  newTxOut
    pf
    [ Address (scriptAddr (never 0 pf) pf),
      Amount (inject $ Coin 3000),
      DHash' [hashData $ datumExample2 @era]
    ]

outEx2 :: (Scriptic era, EraTxOut era) => Proof era -> TxOut era
outEx2 pf = newTxOut pf [Address (someAddr pf), Amount (inject $ Coin 2995)]

notValidatingBody :: (Scriptic era, EraTxBody era) => Proof era -> TxBody era
notValidatingBody pf =
  newTxBody
    pf
    [ Inputs' [mkGenesisTxIn 2],
      Collateral' [mkGenesisTxIn 12],
      Outputs' [outEx2 pf],
      Txfee (Coin 5),
      WppHash (newScriptIntegrityHash pf (pp pf) [PlutusV1] notValidatingRedeemers txDatsExample2)
    ]

notValidatingTx ::
  ( Scriptic era,
    EraTx era,
    GoodCrypto (Crypto era)
  ) =>
  Proof era ->
  Tx era
notValidatingTx pf =
  newTx
    pf
    [ Body (notValidatingBody pf),
      WitnessesI
        [ AddrWits' [makeWitnessVKey (hashAnnotated (notValidatingBody pf)) (someKeys pf)],
          ScriptWits' [never 0 pf],
          DataWits' [datumExample2],
          RdmrWits notValidatingRedeemers
        ]
    ]

utxoEx2 :: (EraTxBody era, PostShelley era) => Proof era -> UTxO era
utxoEx2 pf = expectedUTxO' pf ExpectFailure 2

utxoStEx2 ::
  (Default (State (EraRule "PPUP" era)), EraTxBody era, PostShelley era) =>
  Proof era ->
  UTxOState era
utxoStEx2 pf = smartUTxOState (utxoEx2 pf) (Coin 0) (Coin 5) def

-- =========================================================================
--  Example 3: Process a CERT transaction with a succeeding Plutus script.
-- =========================================================================

outEx3 :: EraTxOut era => Proof era -> TxOut era
outEx3 pf = newTxOut pf [Address (someAddr pf), Amount (inject $ Coin 995)]

redeemerExample3 :: Data era
redeemerExample3 = Data (Plutus.I 42)

validatingRedeemersEx3 :: Era era => Redeemers era
validatingRedeemersEx3 =
  Redeemers $
    Map.singleton (RdmrPtr Tag.Cert 0) (redeemerExample3, ExUnits 5000 5000)

scriptStakeCredSuceed :: Scriptic era => Proof era -> StakeCredential (Crypto era)
scriptStakeCredSuceed pf = ScriptHashObj (alwaysSucceedsHash 2 pf)

validatingBodyWithCert :: (Scriptic era, EraTxBody era) => Proof era -> TxBody era
validatingBodyWithCert pf =
  newTxBody
    pf
    [ Inputs' [mkGenesisTxIn 3],
      Collateral' [mkGenesisTxIn 13],
      Outputs' [outEx3 pf],
      Certs' [DCertDeleg (DeRegKey $ scriptStakeCredSuceed pf)],
      Txfee (Coin 5),
      WppHash (newScriptIntegrityHash pf (pp pf) [PlutusV1] validatingRedeemersEx3 mempty)
    ]

validatingTxWithCert ::
  forall era.
  ( Scriptic era,
    EraTx era,
    GoodCrypto (Crypto era)
  ) =>
  Proof era ->
  Tx era
validatingTxWithCert pf =
  newTx
    pf
    [ Body (validatingBodyWithCert pf),
      WitnessesI
        [ AddrWits' [makeWitnessVKey (hashAnnotated (validatingBodyWithCert pf)) (someKeys pf)],
          ScriptWits' [always 2 pf],
          RdmrWits validatingRedeemersEx3
        ]
    ]

utxoEx3 :: (PostShelley era, EraTxBody era) => Proof era -> UTxO era
utxoEx3 pf = expectedUTxO' pf (ExpectSuccess (validatingBodyWithCert pf) (outEx3 pf)) 3

utxoStEx3 ::
  (Default (State (EraRule "PPUP" era)), EraTxBody era, PostShelley era) =>
  Proof era ->
  UTxOState era
utxoStEx3 pf = smartUTxOState (utxoEx3 pf) (Coin 0) (Coin 5) def

-- =====================================================================
--  Example 4: Process a CERT transaction with a failing Plutus script.
-- =====================================================================

outEx4 :: (Scriptic era, EraTxOut era) => Proof era -> TxOut era
outEx4 pf = newTxOut pf [Address (someAddr pf), Amount (inject $ Coin 995)]

redeemerExample4 :: Data era
redeemerExample4 = Data (Plutus.I 0)

notValidatingRedeemersEx4 :: Era era => Redeemers era
notValidatingRedeemersEx4 =
  Redeemers $
    Map.singleton (RdmrPtr Tag.Cert 0) (redeemerExample4, ExUnits 5000 5000)

scriptStakeCredFail :: Scriptic era => Proof era -> StakeCredential (Crypto era)
scriptStakeCredFail pf = ScriptHashObj (alwaysFailsHash 1 pf)

notValidatingBodyWithCert :: (Scriptic era, EraTxBody era) => Proof era -> TxBody era
notValidatingBodyWithCert pf =
  newTxBody
    pf
    [ Inputs' [mkGenesisTxIn 4],
      Collateral' [mkGenesisTxIn 14],
      Outputs' [outEx4 pf],
      Certs' [DCertDeleg (DeRegKey $ scriptStakeCredFail pf)],
      Txfee (Coin 5),
      WppHash (newScriptIntegrityHash pf (pp pf) [PlutusV1] notValidatingRedeemersEx4 mempty)
    ]

notValidatingTxWithCert ::
  forall era.
  ( Scriptic era,
    EraTx era,
    GoodCrypto (Crypto era)
  ) =>
  Proof era ->
  Tx era
notValidatingTxWithCert pf =
  newTx
    pf
    [ Body (notValidatingBodyWithCert pf),
      WitnessesI
        [ AddrWits' [makeWitnessVKey (hashAnnotated (notValidatingBodyWithCert pf)) (someKeys pf)],
          ScriptWits' [never 1 pf],
          RdmrWits notValidatingRedeemersEx4
        ]
    ]

utxoEx4 :: (EraTxBody era, PostShelley era) => Proof era -> UTxO era
utxoEx4 pf = expectedUTxO' pf ExpectFailure 4

utxoStEx4 ::
  (Default (State (EraRule "PPUP" era)), EraTxBody era, PostShelley era) =>
  Proof era ->
  UTxOState era
utxoStEx4 pf = smartUTxOState (utxoEx4 pf) (Coin 0) (Coin 5) def

-- ==============================================================================
--  Example 5: Process a WITHDRAWAL transaction with a succeeding Plutus script.
-- ==============================================================================

outEx5 :: (Scriptic era, EraTxOut era) => Proof era -> TxOut era
outEx5 pf = newTxOut pf [Address (someAddr pf), Amount (inject $ Coin 1995)]

redeemerExample5 :: Data era
redeemerExample5 = Data (Plutus.I 42)

validatingRedeemersEx5 :: Era era => Redeemers era
validatingRedeemersEx5 =
  Redeemers $
    Map.singleton (RdmrPtr Tag.Rewrd 0) (redeemerExample5, ExUnits 5000 5000)

validatingBodyWithWithdrawal :: (EraTxBody era, Scriptic era) => Proof era -> TxBody era
validatingBodyWithWithdrawal pf =
  newTxBody
    pf
    [ Inputs' [mkGenesisTxIn 5],
      Collateral' [mkGenesisTxIn 15],
      Outputs' [outEx5 pf],
      Txfee (Coin 5),
      Wdrls
        ( Wdrl $
            Map.singleton
              (RewardAcnt Testnet (scriptStakeCredSuceed pf))
              (Coin 1000)
        ),
      WppHash (newScriptIntegrityHash pf (pp pf) [PlutusV1] validatingRedeemersEx5 mempty)
    ]

validatingTxWithWithdrawal ::
  forall era.
  ( Scriptic era,
    EraTx era,
    GoodCrypto (Crypto era)
  ) =>
  Proof era ->
  Tx era
validatingTxWithWithdrawal pf =
  newTx
    pf
    [ Body (validatingBodyWithWithdrawal pf),
      WitnessesI
        [ AddrWits' [makeWitnessVKey (hashAnnotated (validatingBodyWithWithdrawal pf)) (someKeys pf)],
          ScriptWits' [always 2 pf],
          RdmrWits validatingRedeemersEx5
        ]
    ]

utxoEx5 :: (PostShelley era, EraTxBody era) => Proof era -> UTxO era
utxoEx5 pf = expectedUTxO' pf (ExpectSuccess (validatingBodyWithWithdrawal pf) (outEx5 pf)) 5

utxoStEx5 ::
  (Default (State (EraRule "PPUP" era)), EraTxBody era, PostShelley era) =>
  Proof era ->
  UTxOState era
utxoStEx5 pf = smartUTxOState (utxoEx5 pf) (Coin 0) (Coin 5) def

-- ===========================================================================
--  Example 6: Process a WITHDRAWAL transaction with a failing Plutus script.
-- ===========================================================================

outEx6 :: (Scriptic era, EraTxOut era) => Proof era -> TxOut era
outEx6 pf = newTxOut pf [Address (someAddr pf), Amount (inject $ Coin 1995)]

redeemerExample6 :: Data era
redeemerExample6 = Data (Plutus.I 0)

notValidatingRedeemersEx6 :: Era era => Redeemers era
notValidatingRedeemersEx6 =
  Redeemers $
    Map.singleton (RdmrPtr Tag.Rewrd 0) (redeemerExample6, ExUnits 5000 5000)

notValidatingBodyWithWithdrawal :: (Scriptic era, EraTxBody era) => Proof era -> TxBody era
notValidatingBodyWithWithdrawal pf =
  newTxBody
    pf
    [ Inputs' [mkGenesisTxIn 6],
      Collateral' [mkGenesisTxIn 16],
      Outputs' [outEx6 pf],
      Txfee (Coin 5),
      Wdrls
        ( Wdrl $
            Map.singleton
              (RewardAcnt Testnet (scriptStakeCredFail pf))
              (Coin 1000)
        ),
      WppHash (newScriptIntegrityHash pf (pp pf) [PlutusV1] notValidatingRedeemersEx6 mempty)
    ]

notValidatingTxWithWithdrawal ::
  forall era.
  ( Scriptic era,
    EraTx era,
    GoodCrypto (Crypto era)
  ) =>
  Proof era ->
  Tx era
notValidatingTxWithWithdrawal pf =
  newTx
    pf
    [ Body (notValidatingBodyWithWithdrawal pf),
      WitnessesI
        [ AddrWits' [makeWitnessVKey (hashAnnotated (notValidatingBodyWithWithdrawal pf)) (someKeys pf)],
          ScriptWits' [never 1 pf],
          RdmrWits notValidatingRedeemersEx6
        ]
    ]

utxoEx6 :: (EraTxBody era, PostShelley era) => Proof era -> UTxO era
utxoEx6 pf = expectedUTxO' pf ExpectFailure 6

utxoStEx6 ::
  (Default (State (EraRule "PPUP" era)), EraTxBody era, PostShelley era) =>
  Proof era ->
  UTxOState era
utxoStEx6 pf = smartUTxOState (utxoEx6 pf) (Coin 0) (Coin 5) def

-- =============================================================================
--  Example 7: Process a MINT transaction with a succeeding Plutus script.
-- =============================================================================

mintEx7 :: forall era. (Scriptic era, HasTokens era) => Proof era -> Value era
mintEx7 pf = forge @era 1 (always 2 pf)

outEx7 :: (HasTokens era, EraTxOut era, Scriptic era) => Proof era -> TxOut era
outEx7 pf = newTxOut pf [Address (someAddr pf), Amount (mintEx7 pf <+> inject (Coin 995))]

redeemerExample7 :: Data era
redeemerExample7 = Data (Plutus.I 42)

validatingRedeemersEx7 :: Era era => Redeemers era
validatingRedeemersEx7 =
  Redeemers $
    Map.singleton (RdmrPtr Tag.Mint 0) (redeemerExample7, ExUnits 5000 5000)

validatingBodyWithMint ::
  (HasTokens era, EraTxBody era, Scriptic era) =>
  Proof era ->
  TxBody era
validatingBodyWithMint pf =
  newTxBody
    pf
    [ Inputs' [mkGenesisTxIn 7],
      Collateral' [mkGenesisTxIn 17],
      Outputs' [outEx7 pf],
      Txfee (Coin 5),
      Mint (mintEx7 pf),
      WppHash (newScriptIntegrityHash pf (pp pf) [PlutusV1] validatingRedeemersEx7 mempty)
    ]

validatingTxWithMint ::
  forall era.
  ( Scriptic era,
    HasTokens era,
    EraTx era,
    GoodCrypto (Crypto era)
  ) =>
  Proof era ->
  Tx era
validatingTxWithMint pf =
  newTx
    pf
    [ Body (validatingBodyWithMint pf),
      WitnessesI
        [ AddrWits' [makeWitnessVKey (hashAnnotated (validatingBodyWithMint pf)) (someKeys pf)],
          ScriptWits' [always 2 pf],
          RdmrWits validatingRedeemersEx7
        ]
    ]

utxoEx7 :: forall era. (HasTokens era, EraTxBody era, PostShelley era) => Proof era -> UTxO era
utxoEx7 pf = expectedUTxO' pf (ExpectSuccess (validatingBodyWithMint pf) (outEx7 pf)) 7

utxoStEx7 ::
  forall era.
  (Default (State (EraRule "PPUP" era)), PostShelley era, EraTxBody era, HasTokens era) =>
  Proof era ->
  UTxOState era
utxoStEx7 pf = smartUTxOState (utxoEx7 pf) (Coin 0) (Coin 5) def

-- ==============================================================================
--  Example 8: Process a MINT transaction with a failing Plutus script.
-- ==============================================================================

mintEx8 :: forall era. (Scriptic era, HasTokens era) => Proof era -> Value era
mintEx8 pf = forge @era 1 (never 1 pf)

outEx8 :: (HasTokens era, EraTxOut era, Scriptic era) => Proof era -> TxOut era
outEx8 pf = newTxOut pf [Address (someAddr pf), Amount (mintEx8 pf <+> inject (Coin 995))]

redeemerExample8 :: Data era
redeemerExample8 = Data (Plutus.I 0)

notValidatingRedeemersEx8 :: Era era => Redeemers era
notValidatingRedeemersEx8 =
  Redeemers $
    Map.singleton (RdmrPtr Tag.Mint 0) (redeemerExample8, ExUnits 5000 5000)

notValidatingBodyWithMint ::
  (HasTokens era, EraTxBody era, Scriptic era) =>
  Proof era ->
  TxBody era
notValidatingBodyWithMint pf =
  newTxBody
    pf
    [ Inputs' [mkGenesisTxIn 8],
      Collateral' [mkGenesisTxIn 18],
      Outputs' [outEx8 pf],
      Txfee (Coin 5),
      Mint (mintEx8 pf),
      WppHash (newScriptIntegrityHash pf (pp pf) [PlutusV1] notValidatingRedeemersEx8 mempty)
    ]

notValidatingTxWithMint ::
  forall era.
  ( Scriptic era,
    HasTokens era,
    EraTx era,
    GoodCrypto (Crypto era)
  ) =>
  Proof era ->
  Tx era
notValidatingTxWithMint pf =
  newTx
    pf
    [ Body (notValidatingBodyWithMint pf),
      WitnessesI
        [ AddrWits' [makeWitnessVKey (hashAnnotated (notValidatingBodyWithMint pf)) (someKeys pf)],
          ScriptWits' [never 1 pf],
          RdmrWits notValidatingRedeemersEx8
        ]
    ]

utxoEx8 :: (PostShelley era, EraTxBody era) => Proof era -> UTxO era
utxoEx8 pf = expectedUTxO' pf ExpectFailure 8

utxoStEx8 ::
  (Default (State (EraRule "PPUP" era)), EraTxBody era, PostShelley era) =>
  Proof era ->
  UTxOState era
utxoStEx8 pf = smartUTxOState (utxoEx8 pf) (Coin 0) (Coin 5) def

-- ====================================================================================
--  Example 9: Process a transaction with a succeeding script in every place possible,
--  and also with succeeding timelock scripts.
-- ====================================================================================

validatingRedeemersEx9 :: Era era => Redeemers era
validatingRedeemersEx9 =
  Redeemers . Map.fromList $
    [ (RdmrPtr Tag.Spend 0, (Data (Plutus.I 101), ExUnits 5000 5000)),
      (RdmrPtr Tag.Cert 1, (Data (Plutus.I 102), ExUnits 5000 5000)),
      (RdmrPtr Tag.Rewrd 0, (Data (Plutus.I 103), ExUnits 5000 5000)),
      (RdmrPtr Tag.Mint 1, (Data (Plutus.I 104), ExUnits 5000 5000))
    ]

mintEx9 :: forall era. (PostShelley era, EraTxOut era, HasTokens era) => Proof era -> Value era
mintEx9 pf = forge @era 1 (always 2 pf) <+> forge @era 1 (timelockScript 1 pf)

outEx9 :: (HasTokens era, EraTxOut era, PostShelley era) => Proof era -> TxOut era
outEx9 pf =
  newTxOut
    pf
    [ Address (someAddr pf),
      Amount (mintEx9 pf <+> inject (Coin 4996))
    ]

timelockStakeCred :: PostShelley era => Proof era -> StakeCredential (Crypto era)
timelockStakeCred pf = ScriptHashObj (timelockHash 2 pf)

validatingBodyManyScripts ::
  (HasTokens era, EraTxBody era, PostShelley era) =>
  Proof era ->
  TxBody era
validatingBodyManyScripts pf =
  newTxBody
    pf
    [ Inputs' [mkGenesisTxIn 1, mkGenesisTxIn 100],
      Collateral' [mkGenesisTxIn 11],
      Outputs' [outEx9 pf],
      Txfee (Coin 5),
      Certs'
        [ DCertDeleg (DeRegKey $ timelockStakeCred pf),
          DCertDeleg (DeRegKey $ scriptStakeCredSuceed pf)
        ],
      Wdrls
        ( Wdrl $
            Map.fromList
              [ (RewardAcnt Testnet (scriptStakeCredSuceed pf), Coin 0),
                (RewardAcnt Testnet (timelockStakeCred pf), Coin 0)
              ]
        ),
      Mint (mintEx9 pf),
      WppHash (newScriptIntegrityHash pf (pp pf) [PlutusV1] validatingRedeemersEx9 txDatsExample1),
      Vldt (ValidityInterval SNothing (SJust $ SlotNo 1))
    ]

validatingTxManyScripts ::
  forall era.
  ( PostShelley era,
    HasTokens era,
    EraTxBody era,
    GoodCrypto (Crypto era)
  ) =>
  Proof era ->
  Tx era
validatingTxManyScripts pf =
  newTx
    pf
    [ Body (validatingBodyManyScripts pf),
      WitnessesI
        [ AddrWits' $
            map
              (makeWitnessVKey . hashAnnotated . validatingBodyManyScripts $ pf)
              [someKeys pf, theKeyPair 1],
          ScriptWits'
            [ always 2 pf,
              always 3 pf,
              timelockScript 0 pf,
              timelockScript 1 pf,
              timelockScript 2 pf
            ],
          DataWits' [datumExample1],
          RdmrWits validatingRedeemersEx9
        ]
    ]

utxoEx9 :: forall era. (EraTxBody era, PostShelley era, HasTokens era) => Proof era -> UTxO era
utxoEx9 pf = UTxO utxo
  where
    utxo =
      Map.insert (TxIn (txid (validatingBodyManyScripts pf)) minBound) (outEx9 pf) $
        Map.filterWithKey
          (\k _ -> k /= mkGenesisTxIn 1 && k /= mkGenesisTxIn 100)
          (unUTxO $ initUTxO pf)

utxoStEx9 ::
  forall era.
  (EraTxBody era, Default (State (EraRule "PPUP" era)), PostShelley era, HasTokens era) =>
  Proof era ->
  UTxOState era
utxoStEx9 pf = smartUTxOState (utxoEx9 pf) (Coin 0) (Coin 5) def

-- ====================================================================================
--  Example 10: A transaction with an acceptable supplimentary datum
-- ====================================================================================

outEx10 :: forall era. (EraTxBody era, Scriptic era) => Proof era -> TxOut era
outEx10 pf =
  newTxOut
    pf
    [ Address (scriptAddr (always 3 pf) pf),
      Amount (inject $ Coin 995),
      DHash' [hashData $ datumExample1 @era]
    ]

okSupplimentaryDatumTxBody :: (EraTxBody era, Scriptic era) => Proof era -> TxBody era
okSupplimentaryDatumTxBody pf =
  newTxBody
    pf
    [ Inputs' [mkGenesisTxIn 3],
      Outputs' [outEx10 pf],
      Txfee (Coin 5),
      WppHash (newScriptIntegrityHash pf (pp pf) [] (Redeemers mempty) txDatsExample1)
    ]

okSupplimentaryDatumTx ::
  forall era.
  ( Scriptic era,
    EraTx era,
    GoodCrypto (Crypto era)
  ) =>
  Proof era ->
  Tx era
okSupplimentaryDatumTx pf =
  newTx
    pf
    [ Body (okSupplimentaryDatumTxBody pf),
      WitnessesI
        [ AddrWits' [makeWitnessVKey (hashAnnotated (okSupplimentaryDatumTxBody pf)) (someKeys pf)],
          DataWits' [datumExample1]
        ]
    ]

utxoEx10 :: forall era. (EraTxBody era, PostShelley era) => Proof era -> UTxO era
utxoEx10 pf = expectedUTxO' pf (ExpectSuccess (okSupplimentaryDatumTxBody pf) (outEx10 pf)) 3

utxoStEx10 ::
  forall era.
  (EraTxBody era, Default (State (EraRule "PPUP" era)), PostShelley era) =>
  Proof era ->
  UTxOState era
utxoStEx10 pf = smartUTxOState (utxoEx10 pf) (Coin 0) (Coin 5) def

-- ====================================================================================
--  Example 11: A transaction with multiple identical certificates
-- ====================================================================================

multipleEqualCertsRedeemers :: Era era => Redeemers era
multipleEqualCertsRedeemers =
  Redeemers $
    Map.fromList
      [ (RdmrPtr Tag.Cert 0, (redeemerExample3, ExUnits 5000 5000))
      ]

multipleEqualCertsBody :: (EraTxBody era, Scriptic era) => Proof era -> TxBody era
multipleEqualCertsBody pf =
  newTxBody
    pf
    [ Inputs' [mkGenesisTxIn 3],
      Collateral' [mkGenesisTxIn 13],
      Outputs' [outEx3 pf],
      Certs'
        [ DCertDeleg (DeRegKey $ scriptStakeCredSuceed pf),
          DCertDeleg (DeRegKey $ scriptStakeCredSuceed pf) -- not allowed by DELEG, but here is fine
        ],
      Txfee (Coin 5),
      WppHash (newScriptIntegrityHash pf (pp pf) [PlutusV1] multipleEqualCertsRedeemers mempty)
    ]

multipleEqualCertsTx ::
  forall era.
  ( Scriptic era,
    EraTx era,
    GoodCrypto (Crypto era)
  ) =>
  Proof era ->
  Tx era
multipleEqualCertsTx pf =
  newTx
    pf
    [ Body (multipleEqualCertsBody pf),
      WitnessesI
        [ AddrWits' [makeWitnessVKey (hashAnnotated (multipleEqualCertsBody pf)) (someKeys pf)],
          ScriptWits' [always 2 pf],
          RdmrWits multipleEqualCertsRedeemers
        ]
    ]

utxoEx11 :: (EraTxBody era, PostShelley era) => Proof era -> UTxO era
utxoEx11 pf = expectedUTxO' pf (ExpectSuccess (multipleEqualCertsBody pf) (outEx3 pf)) 3

utxoStEx11 ::
  (EraTxBody era, Default (State (EraRule "PPUP" era)), PostShelley era) =>
  Proof era ->
  UTxOState era
utxoStEx11 pf = smartUTxOState (utxoEx11 pf) (Coin 0) (Coin 5) def

-- ====================================================================================
--  Example 12: Attaching a datum (hash) to a non-script output.
--
--  Note that a when spending a non-script output with a datum hash, the datum cannot
--  be supplied, because it is considered extraneous,
--  as in the 'notOkSupplimentaryDatumTx' example.
-- ====================================================================================

outEx12 :: EraTxOut era => Proof era -> TxOut era
outEx12 pf = newTxOut pf [Address (someAddr pf), Amount (inject $ Coin 1216)]

nonScriptOutWithDatumTxBody :: EraTxBody era => Proof era -> TxBody era
nonScriptOutWithDatumTxBody pf =
  newTxBody
    pf
    [ Inputs' [mkGenesisTxIn 103],
      Outputs' [outEx12 pf],
      Txfee (Coin 5)
    ]

nonScriptOutWithDatumTx ::
  forall era.
  ( Scriptic era,
    EraTx era,
    GoodCrypto (Crypto era)
  ) =>
  Proof era ->
  Tx era
nonScriptOutWithDatumTx pf =
  newTx
    pf
    [ Body (nonScriptOutWithDatumTxBody pf),
      WitnessesI
        [ AddrWits' [makeWitnessVKey (hashAnnotated (nonScriptOutWithDatumTxBody pf)) (someKeys pf)]
        ]
    ]

utxoEx12 :: (EraTxBody era, PostShelley era) => Proof era -> UTxO era
utxoEx12 pf = expectedUTxO' pf (ExpectSuccess (nonScriptOutWithDatumTxBody pf) (outEx12 pf)) 103

utxoStEx12 ::
  ( Default (State (EraRule "PPUP" era)),
    PostShelley era,
    EraTxBody era
  ) =>
  Proof era ->
  UTxOState era
utxoStEx12 pf =
  smartUTxOState
    (utxoEx12 pf)
    (Coin 0)
    (Coin 5)
    def

-- =======================
-- Invalid Transactions
-- =======================

incorrectNetworkIDTxBody :: EraTxBody era => Proof era -> TxBody era
incorrectNetworkIDTxBody pf =
  newTxBody
    pf
    [ Inputs' [mkGenesisTxIn 3],
      Outputs' [outEx3 pf],
      Txfee (Coin 5),
      Txnetworkid (SJust Mainnet)
    ]

incorrectNetworkIDTx :: (EraTx era, GoodCrypto (Crypto era)) => Proof era -> Tx era
incorrectNetworkIDTx pf =
  newTx
    pf
    [ Body (incorrectNetworkIDTxBody pf),
      WitnessesI
        [ AddrWits' [makeWitnessVKey (hashAnnotated (incorrectNetworkIDTxBody pf)) (someKeys pf)]
        ]
    ]

extraneousKeyHash :: CC.Crypto c => KeyHash 'Witness c
extraneousKeyHash = hashKey . snd . mkKeyPair $ RawSeed 0 0 0 0 99

missingRequiredWitnessTxBody :: EraTxBody era => Proof era -> TxBody era
missingRequiredWitnessTxBody pf =
  newTxBody
    pf
    [ Inputs' [mkGenesisTxIn 3],
      Outputs' [outEx3 pf],
      Txfee (Coin 5),
      ReqSignerHashes' [extraneousKeyHash]
    ]

missingRequiredWitnessTx :: (EraTx era, GoodCrypto (Crypto era)) => Proof era -> Tx era
missingRequiredWitnessTx pf =
  newTx
    pf
    [ Body (missingRequiredWitnessTxBody pf),
      WitnessesI
        [ AddrWits' [makeWitnessVKey (hashAnnotated (missingRequiredWitnessTxBody pf)) (someKeys pf)]
        ]
    ]

missingRedeemerTxBody :: EraTxBody era => Proof era -> TxBody era
missingRedeemerTxBody pf =
  newTxBody
    pf
    [ Inputs' [mkGenesisTxIn 1],
      Collateral' [mkGenesisTxIn 11],
      Outputs' [outEx1 pf],
      Txfee (Coin 5),
      WppHash (newScriptIntegrityHash pf (pp pf) [PlutusV1] (Redeemers mempty) txDatsExample1)
    ]

missingRedeemerTx ::
  (Scriptic era, EraTx era, GoodCrypto (Crypto era)) =>
  Proof era ->
  Tx era
missingRedeemerTx pf =
  newTx
    pf
    [ Body (missingRedeemerTxBody pf),
      WitnessesI
        [ AddrWits' [makeWitnessVKey (hashAnnotated (missingRedeemerTxBody pf)) (someKeys pf)],
          ScriptWits' [always 3 pf],
          DataWits' [datumExample1]
        ]
    ]

wrongWppHashTx ::
  (Scriptic era, EraTx era, GoodCrypto (Crypto era)) =>
  Proof era ->
  Tx era
wrongWppHashTx pf =
  newTx
    pf
    [ Body (missingRedeemerTxBody pf),
      WitnessesI
        [ AddrWits' [makeWitnessVKey (hashAnnotated (missingRedeemerTxBody pf)) (someKeys pf)],
          ScriptWits' [always 3 pf],
          DataWits' [datumExample1],
          RdmrWits validatingRedeemersEx1
        ]
    ]

missing1phaseScriptWitnessTx ::
  forall era.
  ( PostShelley era,
    HasTokens era,
    EraTxBody era,
    GoodCrypto (Crypto era)
  ) =>
  Proof era ->
  Tx era
missing1phaseScriptWitnessTx pf =
  newTx
    pf
    [ Body (validatingBodyManyScripts pf),
      WitnessesI
        [ AddrWits' $
            map
              (makeWitnessVKey . hashAnnotated . validatingBodyManyScripts $ pf)
              [someKeys pf, theKeyPair 1],
          ScriptWits'
            [ always 2 pf,
              always 3 pf,
              -- intentionally missing -> timelockScript 0 pf,
              timelockScript 1 pf,
              timelockScript 2 pf
            ],
          DataWits' [datumExample1],
          RdmrWits validatingRedeemersEx9
        ]
    ]

missing2phaseScriptWitnessTx ::
  forall era.
  ( PostShelley era,
    HasTokens era,
    EraTx era,
    GoodCrypto (Crypto era)
  ) =>
  Proof era ->
  Tx era
missing2phaseScriptWitnessTx pf =
  newTx
    pf
    [ Body (validatingBodyManyScripts pf),
      WitnessesI
        [ AddrWits' $
            map
              (makeWitnessVKey . hashAnnotated . validatingBodyManyScripts $ pf)
              [someKeys pf, theKeyPair 1],
          ScriptWits'
            [ -- intentionally missing -> always 2 pf,
              always 3 pf,
              timelockScript 0 pf,
              timelockScript 1 pf,
              timelockScript 2 pf
            ],
          DataWits' [datumExample1],
          RdmrWits validatingRedeemersEx9
        ]
    ]

misPurposedRedeemer :: Era era => Redeemers era
misPurposedRedeemer =
  Redeemers $
    -- The label *should* be Spend, not Mint
    Map.singleton (RdmrPtr Tag.Mint 0) (redeemerExample1, ExUnits 5000 5000)

wrongRedeemerLabelTxBody :: EraTxBody era => Proof era -> TxBody era
wrongRedeemerLabelTxBody pf =
  newTxBody
    pf
    [ Inputs' [mkGenesisTxIn 1],
      Collateral' [mkGenesisTxIn 11],
      Outputs' [outEx1 pf],
      Txfee (Coin 5),
      WppHash (newScriptIntegrityHash pf (pp pf) [PlutusV1] misPurposedRedeemer txDatsExample1)
    ]

wrongRedeemerLabelTx ::
  forall era.
  ( Scriptic era,
    EraTx era,
    GoodCrypto (Crypto era)
  ) =>
  Proof era ->
  Tx era
wrongRedeemerLabelTx pf =
  newTx
    pf
    [ Body (wrongRedeemerLabelTxBody pf),
      WitnessesI
        [ AddrWits' [makeWitnessVKey (hashAnnotated (wrongRedeemerLabelTxBody pf)) (someKeys pf)],
          ScriptWits' [always 3 pf],
          DataWits' [datumExample1],
          RdmrWits misPurposedRedeemer
        ]
    ]

missingDatumTxBody :: EraTxBody era => Proof era -> TxBody era
missingDatumTxBody pf =
  newTxBody
    pf
    [ Inputs' [mkGenesisTxIn 1],
      Collateral' [mkGenesisTxIn 11],
      Outputs' [outEx1 pf],
      Txfee (Coin 5),
      WppHash (newScriptIntegrityHash pf (pp pf) [PlutusV1] validatingRedeemersEx1 mempty)
    ]

missingDatumTx ::
  forall era.
  ( Scriptic era,
    EraTx era,
    GoodCrypto (Crypto era)
  ) =>
  Proof era ->
  Tx era
missingDatumTx pf =
  newTx
    pf
    [ Body (missingDatumTxBody pf),
      WitnessesI
        [ AddrWits' [makeWitnessVKey (hashAnnotated (missingDatumTxBody pf)) (someKeys pf)],
          ScriptWits' [always 3 pf],
          RdmrWits validatingRedeemersEx1
        ]
    ]

phase1FailureTx ::
  forall era.
  ( PostShelley era,
    HasTokens era,
    EraTx era,
    GoodCrypto (Crypto era)
  ) =>
  Proof era ->
  Tx era
phase1FailureTx pf =
  newTx
    pf
    [ Body (validatingBodyManyScripts pf),
      WitnessesI
        [ AddrWits'
            [ makeWitnessVKey
                (hashAnnotated $ validatingBodyManyScripts pf)
                (someKeys pf)
            ],
          ScriptWits'
            [ always 2 pf,
              always 3 pf,
              timelockScript 0 pf,
              timelockScript 1 pf,
              timelockScript 2 pf
            ],
          DataWits' [datumExample1],
          RdmrWits validatingRedeemersEx9
        ]
    ]

validatingRedeemersTooManyExUnits :: Era era => Redeemers era
validatingRedeemersTooManyExUnits =
  Redeemers $
    Map.singleton (RdmrPtr Tag.Spend 0) (redeemerExample1, ExUnits 1000001 5000)

tooManyExUnitsTxBody :: EraTxBody era => Proof era -> TxBody era
tooManyExUnitsTxBody pf =
  newTxBody
    pf
    [ Inputs' [mkGenesisTxIn 1],
      Collateral' [mkGenesisTxIn 11],
      Outputs' [outEx1 pf],
      Txfee (Coin 5),
      WppHash (newScriptIntegrityHash pf (pp pf) [PlutusV1] validatingRedeemersTooManyExUnits txDatsExample1)
    ]

tooManyExUnitsTx ::
  forall era.
  ( Scriptic era,
    EraTx era,
    GoodCrypto (Crypto era)
  ) =>
  Proof era ->
  Tx era
tooManyExUnitsTx pf =
  newTx
    pf
    [ Body (tooManyExUnitsTxBody pf),
      WitnessesI
        [ AddrWits' [makeWitnessVKey (hashAnnotated (tooManyExUnitsTxBody pf)) (someKeys pf)],
          ScriptWits' [always 3 pf],
          DataWits' [datumExample1],
          RdmrWits validatingRedeemersTooManyExUnits
        ]
    ]

missingCollateralSig ::
  forall era.
  (Scriptic era, EraTx era) =>
  Proof era ->
  Tx era
missingCollateralSig pf =
  newTx
    pf
    [ Body (validatingBody pf),
      WitnessesI
        [ ScriptWits' [always 3 pf],
          DataWits' [datumExample1],
          RdmrWits validatingRedeemersEx1
        ]
    ]

plutusOutputWithNoDataTxBody :: EraTxBody era => Proof era -> TxBody era
plutusOutputWithNoDataTxBody pf =
  newTxBody
    pf
    [ Inputs' [mkGenesisTxIn 101],
      Collateral' [mkGenesisTxIn 11],
      Outputs' [outEx1 pf],
      Txfee (Coin 5),
      WppHash (newScriptIntegrityHash pf (pp pf) [PlutusV1] validatingRedeemersEx1 mempty)
    ]

plutusOutputWithNoDataTx ::
  forall era.
  ( Scriptic era,
    EraTx era,
    GoodCrypto (Crypto era)
  ) =>
  Proof era ->
  Tx era
plutusOutputWithNoDataTx pf =
  newTx
    pf
    [ Body (plutusOutputWithNoDataTxBody pf),
      WitnessesI
        [ AddrWits' [makeWitnessVKey (hashAnnotated (plutusOutputWithNoDataTxBody pf)) (someKeys pf)],
          ScriptWits' [always 3 pf],
          RdmrWits validatingRedeemersEx1
        ]
    ]

totallyIrrelevantDatum :: Data era
totallyIrrelevantDatum = Data (Plutus.I 1729)

outputWithNoDatum :: forall era. EraTxOut era => Proof era -> TxOut era
outputWithNoDatum pf = newTxOut pf [Address $ someAddr pf, Amount (inject $ Coin 995)]

notOkSupplimentaryDatumTxBody :: EraTxBody era => Proof era -> TxBody era
notOkSupplimentaryDatumTxBody pf =
  newTxBody
    pf
    [ Inputs' [mkGenesisTxIn 3],
      Outputs' [outputWithNoDatum pf],
      Txfee (Coin 5),
      WppHash (newScriptIntegrityHash pf (pp pf) [] (Redeemers mempty) totallyIrrelevantTxDats)
    ]
  where
    totallyIrrelevantTxDats = TxDats $ keyBy hashData [totallyIrrelevantDatum]

notOkSupplimentaryDatumTx ::
  forall era.
  ( Scriptic era,
    EraTx era,
    GoodCrypto (Crypto era)
  ) =>
  Proof era ->
  Tx era
notOkSupplimentaryDatumTx pf =
  newTx
    pf
    [ Body (notOkSupplimentaryDatumTxBody pf),
      WitnessesI
        [ AddrWits' [makeWitnessVKey (hashAnnotated (notOkSupplimentaryDatumTxBody pf)) (someKeys pf)],
          DataWits' [totallyIrrelevantDatum]
        ]
    ]

hashsize :: forall c. CC.Crypto c => Int
hashsize = fromIntegral $ sizeHash ([] @(CC.HASH c))

poolMDHTooBigTxBody :: forall era. (EraTxBody era, Scriptic era) => Proof era -> TxBody era
poolMDHTooBigTxBody pf =
  newTxBody
    pf
    [ Inputs' [mkGenesisTxIn 3],
      Outputs' [newTxOut pf [Address $ someAddr pf, Amount (inject $ Coin 995)]],
      Certs' [DCertPool (RegPool poolParams)],
      Txfee (Coin 5)
    ]
  where
    tooManyBytes = BS.replicate (hashsize @(Crypto era) + 1) 0
    poolParams =
      PoolParams
        { _poolId = coerceKeyRole . hashKey . vKey $ someKeys pf,
          _poolVrf = hashVerKeyVRF . snd . mkVRFKeyPair $ RawSeed 0 0 0 0 0,
          _poolPledge = Coin 0,
          _poolCost = Coin 0,
          _poolMargin = minBound,
          _poolRAcnt = RewardAcnt Testnet (scriptStakeCredSuceed pf),
          _poolOwners = mempty,
          _poolRelays = mempty,
          _poolMD = SJust $ PoolMetadata (fromJust $ textToUrl "") tooManyBytes
        }

poolMDHTooBigTx ::
  forall era.
  ( Scriptic era,
    EraTxBody era,
    GoodCrypto (Crypto era)
  ) =>
  Proof era ->
  Tx era
poolMDHTooBigTx pf =
  -- Note that the UTXOW rule will no trigger the expected predicate failure,
  -- since it is checked in the POOL rule. BBODY will trigger it, however.
  newTx
    pf
    [ Body (poolMDHTooBigTxBody pf),
      WitnessesI
        [ AddrWits' [makeWitnessVKey (hashAnnotated (poolMDHTooBigTxBody pf)) (someKeys pf)]
        ]
    ]

multipleEqualCertsRedeemersInvalid :: Era era => Redeemers era
multipleEqualCertsRedeemersInvalid =
  Redeemers $
    Map.fromList
      [ (RdmrPtr Tag.Cert 0, (redeemerExample3, ExUnits 5000 5000)),
        (RdmrPtr Tag.Cert 1, (redeemerExample3, ExUnits 5000 5000))
      ]

multipleEqualCertsBodyInvalid :: EraTxBody era => Scriptic era => Proof era -> TxBody era
multipleEqualCertsBodyInvalid pf =
  newTxBody
    pf
    [ Inputs' [mkGenesisTxIn 3],
      Collateral' [mkGenesisTxIn 13],
      Outputs' [outEx3 pf],
      Certs'
        [ DCertDeleg (DeRegKey $ scriptStakeCredSuceed pf),
          DCertDeleg (DeRegKey $ scriptStakeCredSuceed pf) -- not allowed by DELEG, but here is fine
        ],
      Txfee (Coin 5),
      WppHash (newScriptIntegrityHash pf (pp pf) [PlutusV1] multipleEqualCertsRedeemersInvalid mempty)
    ]

multipleEqualCertsTxInvalid ::
  forall era.
  ( Scriptic era,
    EraTx era,
    GoodCrypto (Crypto era)
  ) =>
  Proof era ->
  Tx era
multipleEqualCertsTxInvalid pf =
  newTx
    pf
    [ Body (multipleEqualCertsBodyInvalid pf),
      WitnessesI
        [ AddrWits' [makeWitnessVKey (hashAnnotated (multipleEqualCertsBodyInvalid pf)) (someKeys pf)],
          ScriptWits' [always 2 pf],
          RdmrWits multipleEqualCertsRedeemersInvalid
        ]
    ]

noCostModelBody :: EraTxBody era => Proof era -> TxBody era
noCostModelBody pf =
  newTxBody
    pf
    [ Inputs' [mkGenesisTxIn 102],
      Collateral' [mkGenesisTxIn 11],
      Outputs' [outEx1 pf],
      Txfee (Coin 5),
      WppHash (newScriptIntegrityHash pf (pp pf) [PlutusV2] validatingRedeemersEx1 txDatsExample1)
    ]

noCostModelTx ::
  forall era.
  ( Scriptic era,
    GoodCrypto (Crypto era),
    EraTx era
  ) =>
  Proof era ->
  Tx era
noCostModelTx pf =
  newTx
    pf
    [ Body (noCostModelBody pf),
      WitnessesI
        [ AddrWits' [makeWitnessVKey (hashAnnotated (noCostModelBody pf)) (someKeys pf)],
          ScriptWits' [alwaysAlt 3 pf],
          DataWits' [datumExample1],
          RdmrWits validatingRedeemersEx1
        ]
    ]

-- =======================
-- Alonzo UTXOW Tests
-- =======================

type A = AlonzoEra C_Crypto

type UtxowPF = PredicateFailure (EraRule "UTXOW" A)

quietPlutusFailure :: FailureDescription
quietPlutusFailure = PlutusFailure "human" "debug"

-- =========================
-- We have some tests that use plutus scripts, so they can only be run in
-- Babbage and Alonzo. How do we do that? We identify functions that are
-- only well typed in those Eras, and we make versions which are parameterized
-- by a proof. But which raise an error in other Eras.

collectInputs ::
  forall era.
  Proof era ->
  EpochInfo (Either Text) ->
  SystemStart ->
  PParams era ->
  Tx era ->
  UTxO era ->
  Either
    [CollectError (Crypto era)]
    [(ShortByteString, Language, [Data era], ExUnits, CostModel)]
collectInputs (Alonzo _) = collectTwoPhaseScriptInputs @era
collectInputs (Babbage _) = collectTwoPhaseScriptInputs @era
collectInputs (Conway _) = collectTwoPhaseScriptInputs @era
collectInputs x = error ("collectInputs Not defined in era " ++ show x)

getTxInfo ::
  Proof era ->
  PParams era ->
  Language ->
  EpochInfo (Either Text) ->
  SystemStart ->
  UTxO era ->
  Tx era ->
  Either (TranslationError (Crypto era)) VersionedTxInfo
getTxInfo (Alonzo _) = txInfo
getTxInfo (Babbage _) = txInfo
getTxInfo (Conway _) = txInfo
getTxInfo era = error ("getTxInfo Not defined in era " ++ show era)

-- Test for Plutus Data Ordering, using this strategy

-- | Never apply this to any Era but Alonzo or Babbage
collectTwoPhaseScriptInputsOutputOrdering ::
  Assertion
collectTwoPhaseScriptInputsOutputOrdering =
  collectInputs apf testEpochInfo testSystemStart (pp apf) (validatingTx apf) (initUTxO apf)
    @?= Right
      [ ( sbs,
          lang,
          [datumExample1, redeemerExample1, context],
          ExUnits 5000 5000,
          freeCostModelV1
        )
      ]
  where
    apf = Alonzo Mock
    (lang, sbs) = case always 3 apf of
      TimelockScript _ -> error "always was not a Plutus script"
      PlutusScript l s -> (l, s)
    context =
      valContext
        ( fromRight (error "translation error") $
            getTxInfo
              apf
              (pp apf)
              PlutusV1
              testEpochInfo
              testSystemStart
              (initUTxO apf)
              (validatingTx apf)
        )
        (Spending $ mkGenesisTxIn 1)

collectOrderingAlonzo :: TestTree
collectOrderingAlonzo =
  testCase
    "collectTwoPhaseScriptInputs output order"
    collectTwoPhaseScriptInputsOutputOrdering

-- =======================
-- Alonzo BBODY Tests
-- =======================

bbodyEnv :: Proof era -> BbodyEnv era
bbodyEnv pf = BbodyEnv (pp pf) def

dpstate :: Scriptic era => Proof era -> DPState (Crypto era)
dpstate pf =
  def
    { dpsDState =
        def {_unified = UM.insert (scriptStakeCredSuceed pf) (Coin 1000) (Rewards UM.empty)}
    }

initialBBodyState ::
  ( Default (State (EraRule "PPUP" era)),
    EraTxOut era,
    PostShelley era
  ) =>
  Proof era ->
  UTxO era ->
  ShelleyBbodyState era
initialBBodyState pf utxo =
  BbodyState (LedgerState (initialUtxoSt utxo) (dpstate pf)) (BlocksMade mempty)

coldKeys :: CC.Crypto c => KeyPair 'BlockIssuer c
coldKeys = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair (RawSeed 1 2 3 2 1)

makeNaiveBlock ::
  forall era. EraSegWits era => [Tx era] -> Block (BHeaderView (Crypto era)) era
makeNaiveBlock txs = UnsafeUnserialisedBlock bhView txs'
  where
    bhView =
      BHeaderView
        { bhviewID = hashKey (vKey coldKeys),
          bhviewBSize = fromIntegral $ bBodySize txs',
          bhviewHSize = 0,
          bhviewBHash = hashTxSeq @era txs',
          bhviewSlot = SlotNo 0
        }
    txs' = (toTxSeq @era) . StrictSeq.fromList $ txs

trustMeP :: Proof era -> Bool -> Tx era -> Tx era
trustMeP (Alonzo _) iv' (AlonzoTx b w _ m) = AlonzoTx b w (IsValid iv') m
trustMeP (Babbage _) iv' (AlonzoTx b w _ m) = AlonzoTx b w (IsValid iv') m
trustMeP (Conway _) iv' (AlonzoTx b w _ m) = AlonzoTx b w (IsValid iv') m
trustMeP _ _ tx = tx

testAlonzoBlock ::
  ( GoodCrypto (Crypto era),
    HasTokens era,
    Scriptic era,
    EraSegWits era
  ) =>
  Proof era ->
  Block (BHeaderView (Crypto era)) era
testAlonzoBlock pf =
  makeNaiveBlock
    [ trustMeP pf True $ validatingTx pf,
      trustMeP pf False $ notValidatingTx pf,
      trustMeP pf True $ validatingTxWithWithdrawal pf,
      trustMeP pf False $ notValidatingTxWithWithdrawal pf,
      trustMeP pf True $ validatingTxWithCert pf,
      trustMeP pf False $ notValidatingTxWithCert pf,
      trustMeP pf True $ validatingTxWithMint pf,
      trustMeP pf False $ notValidatingTxWithMint pf
    ]

testAlonzoBadPMDHBlock :: GoodCrypto (Crypto era) => Proof era -> Block (BHeaderView (Crypto era)) era
testAlonzoBadPMDHBlock pf@(Alonzo _) = makeNaiveBlock [trustMeP pf True $ poolMDHTooBigTx pf]
testAlonzoBadPMDHBlock pf@(Babbage _) = makeNaiveBlock [trustMeP pf True $ poolMDHTooBigTx pf]
testAlonzoBadPMDHBlock pf@(Conway _) = makeNaiveBlock [trustMeP pf True $ poolMDHTooBigTx pf]
testAlonzoBadPMDHBlock other = error ("testAlonzoBadPMDHBlock does not work in era " ++ show other)

example1UTxO ::
  ( GoodCrypto (Crypto era),
    HasTokens era,
    PostShelley era,
    EraTxBody era
  ) =>
  Proof era ->
  UTxO era
example1UTxO pf =
  UTxO $
    Map.fromList
      [ (TxIn (txid (validatingBody pf)) minBound, outEx1 pf),
        (TxIn (txid (validatingBodyWithCert pf)) minBound, outEx3 pf),
        (TxIn (txid (validatingBodyWithWithdrawal pf)) minBound, outEx5 pf),
        (TxIn (txid (validatingBodyWithMint pf)) minBound, outEx7 pf),
        (mkGenesisTxIn 11, collateralOutput pf),
        (mkGenesisTxIn 2, alwaysFailsOutput pf),
        (mkGenesisTxIn 13, collateralOutput pf),
        (mkGenesisTxIn 4, someOutput pf),
        (mkGenesisTxIn 15, collateralOutput pf),
        (mkGenesisTxIn 6, someOutput pf),
        (mkGenesisTxIn 17, collateralOutput pf),
        (mkGenesisTxIn 8, someOutput pf),
        (mkGenesisTxIn 100, timelockOut pf),
        (mkGenesisTxIn 101, unspendableOut pf),
        (mkGenesisTxIn 102, alwaysSucceedsOutputV2 pf),
        (mkGenesisTxIn 103, nonScriptOutWithDatum pf)
      ]

example1UtxoSt ::
  ( EraTxBody era,
    GoodCrypto (Crypto era),
    HasTokens era,
    PostShelley era,
    Default (State (EraRule "PPUP" era))
  ) =>
  Proof era ->
  UTxOState era
example1UtxoSt proof = smartUTxOState (example1UTxO proof) (Coin 0) (Coin 40) def

example1BBodyState ::
  ( GoodCrypto (Crypto era),
    HasTokens era,
    PostShelley era,
    Default (State (EraRule "PPUP" era)),
    EraTxBody era
  ) =>
  Proof era ->
  ShelleyBbodyState era
example1BBodyState proof =
  BbodyState (LedgerState (example1UtxoSt proof) def) (BlocksMade $ Map.singleton poolID 1)
  where
    poolID = hashKey . vKey . coerceKeyRole $ coldKeys

-- =============================================================

testEvaluateTransactionFee :: Assertion
testEvaluateTransactionFee =
  evaluateTransactionFee @A
    pparams
    validatingTxNoWits
    1
    @?= minfee pparams (validatingTx pf)
  where
    pf = Alonzo Mock
    pparams = newPParams pf $ defaultPPs ++ [MinfeeA 1]
    validatingTxNoWits =
      newTx
        pf
        [ Body (validatingBody pf),
          WitnessesI
            [ ScriptWits' [always 3 pf],
              DataWits' [datumExample1],
              RdmrWits validatingRedeemersEx1
            ]
        ]

alonzoAPITests :: TestTree
alonzoAPITests =
  testGroup "Alonzo API" [testCase "evaluateTransactionFee" testEvaluateTransactionFee]

-- =====================================================================================
-- Proof parameterized TestTrees

-- | This type is what you get when you use runSTS in the UTXOW rule. It is also
--   the type one uses for expected answers, to compare the 'computed' against 'expected'
type Result era = Either [PredicateFailure (EraRule "UTXOW" era)] (State (EraRule "UTXOW" era))

testUTXOWwith ::
  forall era.
  ( GoodCrypto (Crypto era),
    Default (State (EraRule "PPUP" era)),
    EraTx era
  ) =>
  WitRule "UTXOW" era ->
  (Result era -> Result era -> Assertion) ->
  UTxO era ->
  PParams era ->
  Tx era ->
  Result era ->
  Assertion
testUTXOWwith wit@(UTXOW proof) cont utxo pparams tx expected =
  let env = utxoEnv pparams
      state = initialUtxoSt utxo
   in case proof of
        Conway _ -> runSTS wit (TRC (env, state, tx)) (cont expected)
        Babbage _ -> runSTS wit (TRC (env, state, tx)) (cont expected)
        Alonzo _ -> runSTS wit (TRC (env, state, tx)) (cont expected)
        Mary _ -> runSTS wit (TRC (env, state, tx)) (cont expected)
        Allegra _ -> runSTS wit (TRC (env, state, tx)) (cont expected)
        Shelley _ -> runSTS wit (TRC (env, state, tx)) (cont expected)

-- | A small example of what a continuation for 'runSTS' might look like
genericCont ::
  ( Eq x,
    PrettyA x,
    Eq y,
    PrettyA y,
    HasCallStack
  ) =>
  String ->
  Either [x] y ->
  Either [x] y ->
  Assertion
genericCont cause expected computed =
  case (computed, expected) of
    (Left c, Left e)
      | c /= e -> assertFailure $ causedBy ++ expectedToFail e ++ failedWith c
    (Right c, Right e)
      | c /= e -> assertFailure $ causedBy ++ expectedToPass e ++ passedWith c
    (Left x, Right y) ->
      assertFailure $ causedBy ++ expectedToPass y ++ failedWith x
    (Right x, Left y) ->
      assertFailure $ causedBy ++ expectedToFail y ++ passedWith x
    _ -> pure ()
  where
    causedBy
      | null cause = ""
      | otherwise = "Caused by:\n" ++ cause ++ "\n"
    expectedToPass y = "Expected to pass with:\n" ++ show (prettyA y) ++ "\n"
    expectedToFail y = "Expected to fail with:\n" ++ show (ppList prettyA y) ++ "\n"
    failedWith x = "But failed with:\n" ++ show (ppList prettyA x)
    passedWith x = "But passed with:\n" ++ show (prettyA x)

isSubset :: Eq t => [t] -> [t] -> Bool
isSubset small big = List.all (`List.elem` big) small

subsetCont ::
  ( Eq x,
    Show x,
    PrettyA x,
    Eq y,
    Show y,
    PrettyA y
  ) =>
  Either [x] y ->
  Either [x] y ->
  Assertion
subsetCont expected computed =
  case (computed, expected) of
    (Left c, Left e) ->
      -- It is OK if the expected is a subset of what's computed
      if isSubset e c then e @?= e else c @?= e
    (Right c, Right e) -> c @?= e
    (Left x, Right y) ->
      error $
        "expected to pass with "
          ++ show (prettyA y)
          ++ "\n\nBut failed with\n\n"
          ++ show (ppList prettyA x)
    (Right x, Left y) ->
      error $
        "expected to fail with "
          ++ show (ppList prettyA y)
          ++ "\n\nBut passed with\n\n"
          ++ show (prettyA x)

-- ==============================================================================
-- Three slighty different ways to make an Assertion, but all with the same type

testUTXOWsubset,
  specialCase,
  testUTXOW ::
    forall era.
    ( GoodCrypto (Crypto era),
      Default (State (EraRule "PPUP" era)),
      PostShelley era,
      EraTx era,
      HasCallStack
    ) =>
    WitRule "UTXOW" era ->
    UTxO era ->
    PParams era ->
    Tx era ->
    Either [PredicateFailure (EraRule "UTXOW" era)] (State (EraRule "UTXOW" era)) ->
    Assertion

-- | Use an equality test on the expected and computed [PredicateFailure]
testUTXOW wit@(UTXOW (Alonzo _)) utxo p tx = testUTXOWwith wit (genericCont (show tx)) utxo p tx
testUTXOW wit@(UTXOW (Babbage _)) utxo p tx = testUTXOWwith wit (genericCont (show tx)) utxo p tx
testUTXOW wit@(UTXOW (Conway _)) utxo p tx = testUTXOWwith wit (genericCont (show tx)) utxo p tx
testUTXOW (UTXOW other) _ _ _ = error ("Cannot use testUTXOW in era " ++ show other)

-- | Use a subset test on the expected and computed [PredicateFailure]
testUTXOWsubset wit@(UTXOW (Alonzo _)) utxo = testUTXOWwith wit subsetCont utxo
testUTXOWsubset wit@(UTXOW (Babbage _)) utxo = testUTXOWwith wit subsetCont utxo
testUTXOWsubset wit@(UTXOW (Conway _)) utxo = testUTXOWwith wit subsetCont utxo
testUTXOWsubset (UTXOW other) _ = error ("Cannot use testUTXOW in era " ++ show other)

testU ::
  forall era.
  ( GoodCrypto (Crypto era),
    Default (State (EraRule "PPUP" era)),
    PostShelley era,
    EraTx era,
    HasCallStack
  ) =>
  Proof era ->
  Tx era ->
  Either [PredicateFailure (EraRule "UTXOW" era)] (State (EraRule "UTXOW" era)) ->
  Assertion
testU pf tx expect = testUTXOW (UTXOW pf) (initUTxO pf) (pp pf) tx expect

-- | Use a test where any two (ValidationTagMismatch x y) failures match regardless of 'x' and 'y'
specialCase wit@(UTXOW proof) utxo pparam tx expected =
  let env = utxoEnv pparam
      state = initialUtxoSt utxo
   in case proof of
        Alonzo _ -> runSTS wit (TRC (env, state, tx)) (specialCont proof expected)
        Babbage _ -> runSTS wit (TRC (env, state, tx)) (specialCont proof expected)
        Conway _ -> runSTS wit (TRC (env, state, tx)) (specialCont proof expected)
        other -> error ("Cannot use specialCase in era " ++ show other)

-- ========================================
-- This implements a special rule to test that for ValidationTagMismatch. Rather than comparing the insides of
-- ValidationTagMismatch (which are complicated and depend on Plutus) we just note that both the computed
-- and expected are ValidationTagMismatch. Of course the 'path' to ValidationTagMismatch differs by Era.
-- so we need to case over the Era proof, to get the path correctly.

findMismatch ::
  Proof era ->
  PredicateFailure (EraRule "UTXOW" era) ->
  Maybe (AlonzoUtxosPredFailure era)
findMismatch (Alonzo _) (ShelleyInAlonzoUtxowPredFailure (Shelley.UtxoFailure (UtxosFailure x@(ValidationTagMismatch _ _)))) = Just x
findMismatch (Babbage _) (Babbage.UtxoFailure (AlonzoInBabbageUtxoPredFailure (UtxosFailure x@(ValidationTagMismatch _ _)))) = Just x
findMismatch (Conway _) (Babbage.UtxoFailure (AlonzoInBabbageUtxoPredFailure (UtxosFailure x@(ValidationTagMismatch _ _)))) = Just x
findMismatch _ _ = Nothing

specialCont ::
  ( Eq (PredicateFailure (EraRule "UTXOW" era)),
    Eq a,
    Show (PredicateFailure (EraRule "UTXOW" era)),
    Show a,
    HasCallStack
  ) =>
  Proof era ->
  Either [PredicateFailure (EraRule "UTXOW" era)] a ->
  Either [PredicateFailure (EraRule "UTXOW" era)] a ->
  Assertion
specialCont proof expected computed =
  case (computed, expected) of
    (Left [x], Left [y]) ->
      case (findMismatch proof x, findMismatch proof y) of
        (Just _, Just _) -> y @?= y
        (_, _) -> error "Not both ValidationTagMismatch case 1"
    (Left _, Left _) -> error "Not both ValidationTagMismatch case 2"
    (Right x, Right y) -> x @?= y
    (Left _, Right _) -> error "expected to pass, but failed."
    (Right _, Left _) -> error "expected to fail, but passed."

-- ================================================================================================

alonzoUTXOWexamplesB ::
  forall era.
  ( AlonzoBased era (PredicateFailure (EraRule "UTXOW" era)),
    State (EraRule "UTXOW" era) ~ UTxOState era,
    GoodCrypto (Crypto era),
    HasTokens era,
    Scriptic era,
    Default (State (EraRule "PPUP" era)),
    EraTx era,
    PostShelley era -- MAYBE WE CAN REPLACE THIS BY GoodCrypto
  ) =>
  Proof era ->
  TestTree
alonzoUTXOWexamplesB pf =
  testGroup
    (show pf ++ " UTXOW examples")
    [ testGroup
        "valid transactions"
        [ testCase "validating SPEND script" $
            testU
              pf
              (trustMeP pf True $ validatingTx pf)
              (Right . utxoStEx1 $ pf),
          testCase "not validating SPEND script" $
            testU
              pf
              (trustMeP pf False $ notValidatingTx pf)
              (Right . utxoStEx2 $ pf),
          testCase "validating CERT script" $
            testU
              pf
              (trustMeP pf True $ validatingTxWithCert pf)
              (Right . utxoStEx3 $ pf),
          testCase "not validating CERT script" $
            testU
              pf
              (trustMeP pf False $ notValidatingTxWithCert pf)
              (Right . utxoStEx4 $ pf),
          testCase "validating WITHDRAWAL script" $
            testU
              pf
              (trustMeP pf True $ validatingTxWithWithdrawal pf)
              (Right . utxoStEx5 $ pf),
          testCase "not validating WITHDRAWAL script" $
            testU
              pf
              (trustMeP pf False $ notValidatingTxWithWithdrawal pf)
              (Right . utxoStEx6 $ pf),
          testCase "validating MINT script" $
            testU
              pf
              (trustMeP pf True $ validatingTxWithMint pf)
              (Right . utxoStEx7 $ pf),
          testCase "not validating MINT script" $
            testU
              pf
              (trustMeP pf False $ notValidatingTxWithMint pf)
              (Right . utxoStEx8 $ pf),
          testCase "validating scripts everywhere" $
            testU
              pf
              (trustMeP pf True $ validatingTxManyScripts pf)
              (Right . utxoStEx9 $ pf),
          testCase "acceptable supplimentary datum" $
            testU
              pf
              (trustMeP pf True $ okSupplimentaryDatumTx pf)
              (Right . utxoStEx10 $ pf),
          testCase "multiple identical certificates" $
            testU
              pf
              (trustMeP pf True $ multipleEqualCertsTx pf)
              (Right . utxoStEx11 $ pf),
          testCase "non-script output with datum" $
            testU
              pf
              (trustMeP pf True $ nonScriptOutWithDatumTx pf)
              (Right . utxoStEx12 $ pf)
        ],
      testGroup
        "invalid transactions"
        [ testCase "wrong network ID" $
            testU
              pf
              (trustMeP pf True $ incorrectNetworkIDTx pf)
              ( Left
                  [ fromUtxo @era (WrongNetworkInTxBody Testnet Mainnet)
                  ]
              ),
          testCase "missing required key witness" $
            testU
              pf
              (trustMeP pf True $ missingRequiredWitnessTx pf)
              ( Left [(fromPredFail @era . MissingRequiredSigners . Set.singleton) extraneousKeyHash]
              ),
          testCase "missing redeemer" $
            testU
              pf
              (trustMeP pf True $ missingRedeemerTx pf)
              ( Left
                  [ fromUtxos @era . CollectErrors $
                      [NoRedeemer (Spending (mkGenesisTxIn 1))],
                    fromPredFail $
                      MissingRedeemers @era
                        [(Spending (mkGenesisTxIn 1), alwaysSucceedsHash 3 pf)]
                  ]
              ),
          testCase "wrong wpp hash" $
            testU
              pf
              (trustMeP pf True $ wrongWppHashTx pf)
              ( Left
                  [ fromPredFail @era $
                      PPViewHashesDontMatch
                        ( newScriptIntegrityHash
                            pf
                            (pp pf)
                            [PlutusV1]
                            (Redeemers mempty)
                            txDatsExample1
                        )
                        ( newScriptIntegrityHash
                            pf
                            (pp pf)
                            [PlutusV1]
                            validatingRedeemersEx1
                            txDatsExample1
                        )
                  ]
              ),
          testCase "missing 1-phase script witness" $
            testU
              pf
              (trustMeP pf True $ missing1phaseScriptWitnessTx pf)
              ( Left
                  [ fromUtxow @era . MissingScriptWitnessesUTXOW . Set.singleton $
                      timelockHash 0 pf
                  ]
              ),
          testCase "missing 2-phase script witness" $
            testU
              pf
              (trustMeP pf True $ missing2phaseScriptWitnessTx pf)
              ( Left
                  [ -- these redeemers are associated with phase-1 scripts
                    fromPredFail @era . ExtraRedeemers $
                      [ RdmrPtr Tag.Mint 1,
                        RdmrPtr Tag.Cert 1,
                        RdmrPtr Tag.Rewrd 0
                      ],
                    fromUtxow @era . MissingScriptWitnessesUTXOW . Set.singleton $
                      alwaysSucceedsHash 2 pf
                  ]
              ),
          testCase "redeemer with incorrect label" $
            testU
              pf
              (trustMeP pf True $ wrongRedeemerLabelTx pf)
              ( Left
                  [ fromUtxos @era (CollectErrors [NoRedeemer (Spending (mkGenesisTxIn 1))]),
                    -- now "wrong redeemer label" means there are both unredeemable scripts and extra redeemers
                    fromPredFail @era . MissingRedeemers $
                      [ ( Spending (mkGenesisTxIn 1),
                          alwaysSucceedsHash 3 pf
                        )
                      ],
                    fromPredFail @era . ExtraRedeemers $ [RdmrPtr Tag.Mint 0]
                  ]
              ),
          testCase "missing datum" $
            testU
              pf
              (trustMeP pf True $ missingDatumTx pf)
              ( Left
                  [ fromPredFail @era $
                      MissingRequiredDatums
                        (Set.singleton $ hashData @era datumExample1)
                        mempty
                  ]
              ),
          testCase "phase 1 script failure" $
            testU
              pf
              (trustMeP pf True $ phase1FailureTx pf)
              ( Left
                  [ fromUtxow @era $
                      ScriptWitnessNotValidatingUTXOW $
                        Set.fromList
                          [ timelockHash 0 pf,
                            timelockHash 1 pf,
                            timelockHash 2 pf
                          ]
                  ]
              ),
          testCase "valid transaction marked as invalid" $
            testU
              pf
              (trustMeP pf False $ validatingTx pf)
              ( Left [fromUtxos @era (ValidationTagMismatch (IsValid False) PassedUnexpectedly)]
              ),
          testCase "invalid transaction marked as valid" $
            specialCase
              (UTXOW pf)
              (initUTxO pf)
              (pp pf)
              (trustMeP pf True $ notValidatingTx pf)
              ( Left
                  [ fromUtxos @era
                      ( ValidationTagMismatch
                          (IsValid True)
                          (FailedUnexpectedly (quietPlutusFailure :| []))
                      )
                  ]
              ),
          testCase "too many execution units for tx" $
            testU
              pf
              (trustMeP pf True $ tooManyExUnitsTx pf)
              ( Left
                  [ fromUtxo @era $
                      ExUnitsTooBigUTxO
                        (ExUnits {exUnitsMem = 1000000, exUnitsSteps = 1000000})
                        (ExUnits {exUnitsMem = 1000001, exUnitsSteps = 5000})
                  ]
              ),
          testCase "missing signature for collateral input" $
            testU
              pf
              (trustMeP pf True $ missingCollateralSig pf)
              ( Left
                  [ fromUtxow @era
                      ( MissingVKeyWitnessesUTXOW
                          ( Set.fromList
                              [ asWitness $
                                  hashKey (vKey $ someKeys pf)
                              ]
                          )
                      )
                  ]
              ),
          testCase "insufficient collateral" $
            testUTXOW
              (UTXOW pf)
              (initUTxO pf)
              (newPParams pf $ defaultPPs ++ [CollateralPercentage 150])
              (trustMeP pf True $ validatingTx pf)
              ( Left [fromUtxo @era (InsufficientCollateral (Coin 5) (Coin 8))]
              ),
          testCase "two-phase UTxO with no datum hash" $
            testU
              pf
              (trustMeP pf True $ plutusOutputWithNoDataTx pf)
              ( Left
                  [ fromPredFail @era $ UnspendableUTxONoDatumHash . Set.singleton $ mkGenesisTxIn 101
                  ]
              ),
          testCase "unacceptable supplimentary datum" $
            testUTXOWsubset
              (UTXOW pf) -- Special rules apply here, use (expected `isSubset` computed)
              (initUTxO pf)
              (pp pf)
              (trustMeP pf True $ notOkSupplimentaryDatumTx pf)
              ( Left
                  [ fromPredFail @era $
                      NonOutputSupplimentaryDatums
                        (Set.singleton $ hashData @era totallyIrrelevantDatum)
                        mempty
                  ]
              ),
          testCase "unacceptable extra redeemer" $
            testU
              pf
              (trustMeP pf True $ extraRedeemersTx pf)
              ( Left
                  [ fromPredFail @era $
                      ExtraRedeemers
                        [RdmrPtr Tag.Spend 7]
                  ]
              ),
          testCase "multiple equal plutus-locked certs" $
            testU
              pf
              (trustMeP pf True $ multipleEqualCertsTxInvalid pf)
              ( Left
                  [ fromPredFail @era $ ExtraRedeemers [RdmrPtr Tag.Cert 1]
                  ]
              ),
          testCase "no cost model" $
            testU
              pf
              (trustMeP pf True $ noCostModelTx pf)
              ( Left [fromUtxos @era (CollectErrors [NoCostModel PlutusV2])]
              )
        ]
    ]

-- =====================================

-- | We use this to write set of tests that raise AlonzoBased PredicateFailures.
--   The type we use is for '(fail ~ PredicateFailure (EraRule "UTXOW" era))' .
--   There are 4 types of AlonzoBased PredicateFailures: 'UtxowPredFail',
--   'UtxosPredicateFailure',  'UtxoPredicateFailure', and  'UtxowPredicateFailure' .
--   The idea is to make tests that only raise these failures, in Alonzo and future Eras.
class AlonzoBased era failure where
  fromUtxos :: AlonzoUtxosPredFailure era -> failure
  fromUtxo :: AlonzoUtxoPredFailure era -> failure
  fromUtxow :: ShelleyUtxowPredFailure era -> failure
  fromPredFail :: AlonzoUtxowPredFailure era -> failure

instance AlonzoBased (AlonzoEra c) (AlonzoUtxowPredFailure (AlonzoEra c)) where
  fromUtxos = ShelleyInAlonzoUtxowPredFailure . Shelley.UtxoFailure . UtxosFailure
  fromUtxo = ShelleyInAlonzoUtxowPredFailure . Shelley.UtxoFailure
  fromUtxow = ShelleyInAlonzoUtxowPredFailure
  fromPredFail = id

instance AlonzoBased (BabbageEra c) (BabbageUtxowPredFailure (BabbageEra c)) where
  fromUtxos = Babbage.UtxoFailure . AlonzoInBabbageUtxoPredFailure . UtxosFailure
  fromUtxo = Babbage.UtxoFailure . AlonzoInBabbageUtxoPredFailure
  fromUtxow = AlonzoInBabbageUtxowPredFailure . ShelleyInAlonzoUtxowPredFailure
  fromPredFail = AlonzoInBabbageUtxowPredFailure

instance AlonzoBased (ConwayEra c) (BabbageUtxowPredFailure (ConwayEra c)) where
  fromUtxos = Babbage.UtxoFailure . AlonzoInBabbageUtxoPredFailure . UtxosFailure
  fromUtxo = Babbage.UtxoFailure . AlonzoInBabbageUtxoPredFailure
  fromUtxow = AlonzoInBabbageUtxowPredFailure . ShelleyInAlonzoUtxowPredFailure
  fromPredFail = AlonzoInBabbageUtxowPredFailure

-- ===================================================================

testBBODY ::
  (GoodCrypto (Crypto era), HasCallStack) =>
  WitRule "BBODY" era ->
  ShelleyBbodyState era ->
  Block (BHeaderView (Crypto era)) era ->
  Either [PredicateFailure (AlonzoBBODY era)] (ShelleyBbodyState era) ->
  Assertion
testBBODY wit@(BBODY proof) initialSt block expected =
  let env = bbodyEnv proof
   in case proof of
        Alonzo _ -> runSTS wit (TRC (env, initialSt, block)) (genericCont "" expected)
        Babbage _ -> runSTS wit (TRC (env, initialSt, block)) (genericCont "" expected)
        Conway _ -> runSTS wit (TRC (env, initialSt, block)) (genericCont "" expected)
        other -> error ("We cannot testBBODY in era " ++ show other)

alonzoBBODYexamplesP ::
  forall era.
  ( GoodCrypto (Crypto era),
    HasTokens era,
    Default (State (EraRule "PPUP" era)),
    PostShelley era,
    EraSegWits era
  ) =>
  Proof era ->
  TestTree
alonzoBBODYexamplesP proof =
  testGroup
    (show proof ++ " BBODY examples")
    [ testCase "eight plutus scripts cases" $
        testBBODY
          (BBODY proof)
          (initialBBodyState proof (initUTxO proof))
          (testAlonzoBlock proof)
          (Right (example1BBodyState proof)),
      testCase "block with bad pool md hash in tx" $
        testBBODY
          (BBODY proof)
          (initialBBodyState proof (initUTxO proof))
          (testAlonzoBadPMDHBlock proof)
          (Left [makeTooBig proof])
    ]

makeTooBig :: Proof era -> AlonzoBbodyPredFailure era
makeTooBig proof@(Alonzo _) =
  ShelleyInAlonzoBbodyPredFailure . LedgersFailure . LedgerFailure . DelegsFailure . DelplFailure . PoolFailure $
    PoolMedataHashTooBig (coerceKeyRole . hashKey . vKey $ someKeys proof) (hashsize @Mock + 1)
makeTooBig proof@(Babbage _) =
  ShelleyInAlonzoBbodyPredFailure . LedgersFailure . LedgerFailure . DelegsFailure . DelplFailure . PoolFailure $
    PoolMedataHashTooBig (coerceKeyRole . hashKey . vKey $ someKeys proof) (hashsize @Mock + 1)
makeTooBig proof@(Conway _) =
  ShelleyInAlonzoBbodyPredFailure . LedgersFailure . LedgerFailure . DelegsFailure . DelplFailure . PoolFailure $
    PoolMedataHashTooBig (coerceKeyRole . hashKey . vKey $ someKeys proof) (hashsize @Mock + 1)
makeTooBig proof = error ("makeTooBig does not work in era " ++ show proof)

-- ==============================================================================

allTrees :: TestTree
allTrees =
  testGroup
    "Generic Tests, testing Alonzo PredicateFailures, in postAlonzo eras."
    [ alonzoUTXOWexamplesB (Alonzo Mock),
      alonzoUTXOWexamplesB (Babbage Mock),
      alonzoUTXOWexamplesB (Conway Mock),
      alonzoBBODYexamplesP (Alonzo Mock),
      alonzoBBODYexamplesP (Babbage Mock),
      alonzoBBODYexamplesP (Conway Mock)
    ]

main :: IO ()
main = defaultMain allTrees
