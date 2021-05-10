{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Alonzo.Examples.Utxow where

import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.Data (Data (..), hashData)
import Cardano.Ledger.Alonzo.Language (Language (..))
import Cardano.Ledger.Alonzo.PParams (PParams, PParams' (..))
import Cardano.Ledger.Alonzo.PlutusScriptApi
  ( CollectError (..),
    collectTwoPhaseScriptInputs,
  )
import Cardano.Ledger.Alonzo.Rules.Utxo (UtxoPredicateFailure (..))
import Cardano.Ledger.Alonzo.Rules.Utxos (UtxosPredicateFailure (..))
import Cardano.Ledger.Alonzo.Rules.Utxow (AlonzoPredFail (..), AlonzoUTXOW)
import Cardano.Ledger.Alonzo.Scripts
  ( CostModel (..),
    ExUnits (..),
    Script (..),
    Tag (..),
    alwaysFails,
    alwaysSucceeds,
  )
import Cardano.Ledger.Alonzo.Tx
  ( IsValidating (..),
    ScriptPurpose (..),
    ValidatedTx (..),
    hashWitnessPPData,
  )
import Cardano.Ledger.Alonzo.TxBody
  ( TxBody (..),
    TxOut (..),
    WitnessPPDataHash,
  )
import Cardano.Ledger.Alonzo.TxInfo (txInfo, valContext)
import Cardano.Ledger.Alonzo.TxWitness
  ( RdmrPtr (..),
    Redeemers (..),
    TxWitness (..),
  )
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Era (ValidateScript (..))
import Cardano.Ledger.Hashes (ScriptHash)
import Cardano.Ledger.Mary.Value
  ( AssetName (..),
    PolicyID (..),
    Value (..),
  )
import Cardano.Ledger.SafeHash (hashAnnotated)
import Cardano.Ledger.ShelleyMA.Timelocks (Timelock (..), ValidityInterval (..))
import Cardano.Ledger.Val ((<+>))
import qualified Cardano.Ledger.Val as Val
import Cardano.Slotting.EpochInfo (fixedEpochInfo)
import Cardano.Slotting.Time (SystemStart (..), mkSlotLength)
import Control.State.Transition.Extended hiding (Assertion)
import Control.State.Transition.Trace (checkTrace, (.-), (.->))
import qualified Data.ByteString.Char8 as BS
import Data.Default.Class (def)
import Data.Functor.Identity (Identity)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Word (Word64)
import Numeric.Natural (Natural)
import qualified Plutus.V1.Ledger.Api as P
  ( EvaluationError (..),
    ExBudget (..),
    VerboseMode (..),
    evaluateScriptRestricting,
  )
import Plutus.V1.Ledger.Examples
  ( alwaysFailingNAryFunction,
    alwaysSucceedingNAryFunction,
  )
import qualified PlutusCore.Evaluation.Machine.ExBudgetingDefaults as P
  ( defaultCostModelParams,
  )
import qualified PlutusCore.Evaluation.Machine.ExMemory as P
  ( ExCPU (..),
    ExMemory (..),
  )
import qualified PlutusTx as Plutus
import Shelley.Spec.Ledger.Address (Addr (..))
import Shelley.Spec.Ledger.BaseTypes (Network (..), StrictMaybe (..))
import Shelley.Spec.Ledger.Credential
  ( Credential (..),
    StakeCredential,
    StakeReference (..),
  )
import Shelley.Spec.Ledger.Keys
  ( GenDelegs (..),
    KeyHash,
    KeyPair (..),
    KeyRole (..),
    asWitness,
    hashKey,
  )
import Shelley.Spec.Ledger.LedgerState (UTxOState (..))
import Shelley.Spec.Ledger.STS.Utxo (UtxoEnv (..))
import Shelley.Spec.Ledger.STS.Utxow (UtxowPredicateFailure (..))
import Shelley.Spec.Ledger.Slot (EpochInfo, EpochSize (..), SlotNo (..))
import Shelley.Spec.Ledger.TxBody
  ( DCert (..),
    DelegCert (..),
    RewardAcnt (..),
    TxIn (..),
    Wdrl (..),
  )
import Shelley.Spec.Ledger.UTxO (UTxO (..), makeWitnessVKey, txid)
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes (C_Crypto)
import Test.Shelley.Spec.Ledger.Generator.EraGen (genesisId)
import Test.Shelley.Spec.Ledger.Utils (applySTSTest, mkKeyPair, runShelleyBase)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase, (@?=))

type A = AlonzoEra C_Crypto

-- =======================
-- Setup the initial state
-- =======================

testEpochInfo :: EpochInfo Identity
testEpochInfo = fixedEpochInfo (EpochSize 100) (mkSlotLength 1)

testSystemStart :: SystemStart
testSystemStart = SystemStart $ posixSecondsToUTCTime 0

pp :: PParams A
pp =
  def
    { _costmdls = Map.singleton PlutusV1 (CostModel mempty),
      _maxValSize = 1000000000,
      _maxTxExUnits = ExUnits 1000000 1000000,
      _maxBlockExUnits = ExUnits 1000000 1000000
    }

utxoEnv :: UtxoEnv A
utxoEnv =
  UtxoEnv
    (SlotNo 0)
    pp
    mempty
    (GenDelegs mempty)

-- | Create an address with a given payment script.
scriptAddr :: Script A -> Addr C_Crypto
scriptAddr s = Addr Testnet pCred sCred
  where
    pCred = ScriptHashObj . hashScript @A $ s
    (_ssk, svk) = mkKeyPair @C_Crypto (0, 0, 0, 0, 0)
    sCred = StakeRefBase . KeyHashObj . hashKey $ svk

someKeys :: KeyPair 'Payment C_Crypto
someKeys = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair @C_Crypto (0, 0, 0, 0, 1)

someAddr :: Addr C_Crypto
someAddr = Addr Testnet pCred sCred
  where
    (_ssk, svk) = mkKeyPair @C_Crypto (0, 0, 0, 0, 2)
    pCred = KeyHashObj . hashKey . vKey $ someKeys
    sCred = StakeRefBase . KeyHashObj . hashKey $ svk

collateralOutput :: TxOut A
collateralOutput =
  TxOut
    someAddr
    (Val.inject $ Coin 1000)
    SNothing

someOutput :: TxOut A
someOutput =
  TxOut
    someAddr
    (Val.inject $ Coin 1000)
    SNothing

alwaysSucceedsHash1 :: ScriptHash C_Crypto
alwaysSucceedsHash1 = hashScript @A $ alwaysSucceeds 1

alwaysSucceedsHash2 :: ScriptHash C_Crypto
alwaysSucceedsHash2 = hashScript @A $ alwaysSucceeds 2

alwaysSucceedsHash3 :: ScriptHash C_Crypto
alwaysSucceedsHash3 = hashScript @A $ alwaysSucceeds 3

alwaysFailsHash0 :: ScriptHash C_Crypto
alwaysFailsHash0 = hashScript @A $ alwaysFails 0

alwaysFailsHash1 :: ScriptHash C_Crypto
alwaysFailsHash1 = hashScript @A $ alwaysFails 1

timelockKeys :: KeyPair 'Payment C_Crypto
timelockKeys = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair @C_Crypto (1, 2, 3, 4, 5)

timelockScript :: Word64 -> Script A
timelockScript s =
  TimelockScript $
    RequireAllOf $
      StrictSeq.fromList
        [ RequireSignature . asWitness . hashKey . vKey $ timelockKeys,
          RequireTimeExpire (SlotNo $ 100 + s)
        ]

timelockHash0 :: ScriptHash C_Crypto
timelockHash0 = hashScript @A $ timelockScript 0

timelockHash1 :: ScriptHash C_Crypto
timelockHash1 = hashScript @A $ timelockScript 1

timelockHash2 :: ScriptHash C_Crypto
timelockHash2 = hashScript @A $ timelockScript 2

timelockAddr :: Addr C_Crypto
timelockAddr = Addr Testnet pCred sCred
  where
    (_ssk, svk) = mkKeyPair @C_Crypto (0, 0, 0, 0, 3)
    pCred = ScriptHashObj timelockHash0
    sCred = StakeRefBase . KeyHashObj . hashKey $ svk

timelockOut :: TxOut A
timelockOut =
  TxOut
    timelockAddr
    (Val.inject $ Coin 1)
    SNothing

initUTxO :: UTxO A
initUTxO =
  UTxO $
    Map.fromList $
      [ (TxIn genesisId 1, alwaysSucceedsOutput),
        (TxIn genesisId 2, alwaysFailsOutput)
      ]
        ++ map (\i -> (TxIn genesisId i, someOutput)) [3 .. 8]
        ++ map (\i -> (TxIn genesisId i, collateralOutput)) [11 .. 18]
        ++ [(TxIn genesisId 100, timelockOut)]

initialUtxoSt :: UTxOState A
initialUtxoSt = UTxOState initUTxO (Coin 0) (Coin 0) def

-- | This is a helper type for the expectedUTxO function.
--  ExpectSuccess indicates that we created a valid transaction
--  where the IsValidating flag is true.
data Expect = ExpectSuccess (TxBody A) (TxOut A) | ExpectFailure

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
expectedUTxO :: Expect -> Natural -> UTxO A
expectedUTxO ex idx = UTxO utxo
  where
    utxo = case ex of
      ExpectSuccess txb newOut ->
        Map.insert (TxIn (txid @A txb) 0) newOut (filteredUTxO idx)
      ExpectFailure -> filteredUTxO (10 + idx)
    filteredUTxO x = Map.filterWithKey (\(TxIn _ i) _ -> i /= x) (unUTxO initUTxO)

-- =========================================================================
--  Example 1: Process a SPEND transaction with a succeeding Plutus script.
-- =========================================================================

datumExample1 :: Data A
datumExample1 = Data (Plutus.I 123)

redeemerExample1 :: Data A
redeemerExample1 = Data (Plutus.I 42)

alwaysSucceedsOutput :: TxOut A
alwaysSucceedsOutput =
  TxOut
    (scriptAddr $ alwaysSucceeds 3)
    (Val.inject $ Coin 5000)
    (SJust . hashData $ datumExample1)

validatingRedeemersEx1 :: Redeemers A
validatingRedeemersEx1 =
  Redeemers $
    Map.singleton (RdmrPtr Spend 0) (redeemerExample1, ExUnits 5000 5000)

outEx1 :: TxOut A
outEx1 = TxOut someAddr (Val.inject $ Coin 4995) SNothing

hashWPPD :: Redeemers A -> StrictMaybe (WitnessPPDataHash C_Crypto)
hashWPPD rs = hashWitnessPPData pp (Set.singleton PlutusV1) rs

validatingBody :: TxBody A
validatingBody =
  TxBody
    (Set.singleton $ TxIn genesisId 1) --inputs
    (Set.singleton $ TxIn genesisId 11) --collateral
    (StrictSeq.singleton outEx1) --outputs
    StrictSeq.empty --txcerts
    (Wdrl mempty) --txwdrls
    (Coin 5) --txfee
    (ValidityInterval SNothing SNothing) --txvldt
    SNothing --txUpdates
    (Set.singleton . asWitness . hashKey . vKey $ someKeys) -- reqSignerHashes
    mempty --mint
    (hashWPPD validatingRedeemersEx1) --wppHash
    SNothing --adHash
    SNothing --network id

validatingTx :: ValidatedTx A
validatingTx =
  ValidatedTx
    validatingBody
    TxWitness
      { txwitsVKey = Set.singleton $ makeWitnessVKey (hashAnnotated validatingBody) someKeys,
        txwitsBoot = mempty,
        txscripts =
          Map.singleton alwaysSucceedsHash3 (alwaysSucceeds 3),
        txdats = Map.singleton (hashData datumExample1) datumExample1,
        txrdmrs = validatingRedeemersEx1
      }
    (IsValidating True)
    SNothing

utxoEx1 :: UTxO A
utxoEx1 = expectedUTxO (ExpectSuccess validatingBody outEx1) 1

utxoStEx1 :: UTxOState A
utxoStEx1 = UTxOState utxoEx1 (Coin 0) (Coin 5) def

-- ======================================================================
--  Example 2: Process a SPEND transaction with a failing Plutus script.
-- ======================================================================

datumExample2 :: Data A
datumExample2 = Data (Plutus.I 0)

redeemerExample2 :: Data A
redeemerExample2 = Data (Plutus.I 1)

notValidatingRedeemers :: Redeemers A
notValidatingRedeemers =
  Redeemers $
    Map.singleton (RdmrPtr Spend 0) (redeemerExample2, ExUnits 5000 5000)

alwaysFailsOutput :: TxOut A
alwaysFailsOutput =
  TxOut
    (scriptAddr $ alwaysFails 0)
    (Val.inject $ Coin 3000)
    (SJust . hashData $ datumExample2)

outEx2 :: TxOut A
outEx2 = TxOut someAddr (Val.inject $ Coin 2995) SNothing

notValidatingBody :: TxBody A
notValidatingBody =
  TxBody
    (Set.singleton $ TxIn genesisId 2) --inputs
    (Set.singleton $ TxIn genesisId 12) --collateral
    (StrictSeq.singleton outEx2) --outputs
    StrictSeq.empty --txcerts
    (Wdrl mempty) --txwdrls
    (Coin 5) --txfee
    (ValidityInterval SNothing SNothing) --txvldt
    SNothing --txUpdates
    mempty -- reqSignerHashes
    mempty --mint
    (hashWPPD notValidatingRedeemers) --wppHash
    SNothing --adHash
    SNothing --network id

notValidatingTx :: ValidatedTx A
notValidatingTx =
  ValidatedTx
    notValidatingBody
    TxWitness
      { txwitsVKey = Set.singleton $ makeWitnessVKey (hashAnnotated notValidatingBody) someKeys,
        txwitsBoot = mempty,
        txscripts =
          Map.singleton alwaysFailsHash0 (alwaysFails 0),
        txdats = Map.singleton (hashData datumExample2) datumExample2,
        txrdmrs = notValidatingRedeemers
      }
    (IsValidating False)
    SNothing

utxoEx2 :: UTxO A
utxoEx2 = expectedUTxO ExpectFailure 2

utxoStEx2 :: UTxOState A
utxoStEx2 = UTxOState utxoEx2 (Coin 0) (Coin 1000) def

-- =========================================================================
--  Example 3: Process a CERT transaction with a succeeding Plutus script.
-- =========================================================================

outEx3 :: TxOut A
outEx3 = TxOut someAddr (Val.inject $ Coin 995) SNothing

redeemerExample3 :: Data A
redeemerExample3 = Data (Plutus.I 42)

validatingRedeemersEx3 :: Redeemers A
validatingRedeemersEx3 =
  Redeemers $
    Map.singleton (RdmrPtr Cert 0) (redeemerExample3, ExUnits 5000 5000)

scriptStakeCredSuceed :: StakeCredential C_Crypto
scriptStakeCredSuceed = ScriptHashObj alwaysSucceedsHash2

validatingBodyWithCert :: TxBody A
validatingBodyWithCert =
  TxBody
    (Set.singleton $ TxIn genesisId 3) --inputs
    (Set.singleton $ TxIn genesisId 13) --collateral
    (StrictSeq.singleton outEx3) --outputs
    (StrictSeq.fromList [DCertDeleg (DeRegKey scriptStakeCredSuceed)]) --txcerts
    (Wdrl mempty) --txwdrls
    (Coin 5) --txfee
    (ValidityInterval SNothing SNothing) --txvldt
    SNothing --txUpdates
    mempty -- reqSignerHashes
    mempty --mint
    (hashWPPD validatingRedeemersEx3) --wppHash
    SNothing --adHash
    SNothing --network id

validatingTxWithCert :: ValidatedTx A
validatingTxWithCert =
  ValidatedTx
    validatingBodyWithCert
    TxWitness
      { txwitsVKey = Set.singleton $ makeWitnessVKey (hashAnnotated validatingBodyWithCert) someKeys,
        txwitsBoot = mempty,
        txscripts =
          Map.singleton alwaysSucceedsHash2 (alwaysSucceeds 2),
        txdats = mempty,
        txrdmrs = validatingRedeemersEx3
      }
    (IsValidating True)
    SNothing

utxoEx3 :: UTxO A
utxoEx3 = expectedUTxO (ExpectSuccess validatingBodyWithCert outEx3) 3

utxoStEx3 :: UTxOState A
utxoStEx3 = UTxOState utxoEx3 (Coin 0) (Coin 5) def

-- =====================================================================
--  Example 4: Process a CERT transaction with a failing Plutus script.
-- =====================================================================

outEx4 :: TxOut A
outEx4 = TxOut someAddr (Val.inject $ Coin 995) SNothing

redeemerExample4 :: Data A
redeemerExample4 = Data (Plutus.I 0)

notValidatingRedeemersEx4 :: Redeemers A
notValidatingRedeemersEx4 =
  Redeemers $
    Map.singleton (RdmrPtr Cert 0) (redeemerExample4, ExUnits 5000 5000)

scriptStakeCredFail :: StakeCredential C_Crypto
scriptStakeCredFail = ScriptHashObj alwaysFailsHash1

notValidatingBodyWithCert :: TxBody A
notValidatingBodyWithCert =
  TxBody
    (Set.singleton $ TxIn genesisId 4) --inputs
    (Set.singleton $ TxIn genesisId 14) --collateral
    (StrictSeq.singleton outEx4) --outputs
    (StrictSeq.fromList [DCertDeleg (DeRegKey scriptStakeCredFail)]) --txcerts
    (Wdrl mempty) --txwdrls
    (Coin 5) --txfee
    (ValidityInterval SNothing SNothing) --txvldt
    SNothing --txUpdates
    mempty -- reqSignerHashes
    mempty --mint
    (hashWPPD notValidatingRedeemersEx4) --wppHash
    SNothing --adHash
    SNothing --network id

notValidatingTxWithCert :: ValidatedTx A
notValidatingTxWithCert =
  ValidatedTx
    notValidatingBodyWithCert
    TxWitness
      { txwitsVKey =
          Set.singleton $
            makeWitnessVKey
              (hashAnnotated notValidatingBodyWithCert)
              someKeys,
        txwitsBoot = mempty,
        txscripts =
          Map.singleton alwaysFailsHash1 (alwaysFails 1),
        txdats = mempty,
        txrdmrs = notValidatingRedeemersEx4
      }
    (IsValidating False)
    SNothing

utxoEx4 :: UTxO A
utxoEx4 = expectedUTxO ExpectFailure 4

utxoStEx4 :: UTxOState A
utxoStEx4 = UTxOState utxoEx4 (Coin 0) (Coin 1000) def

-- ==============================================================================
--  Example 5: Process a WITHDRAWAL transaction with a succeeding Plutus script.
-- ==============================================================================

outEx5 :: TxOut A
outEx5 = TxOut someAddr (Val.inject $ Coin 1995) SNothing

redeemerExample5 :: Data A
redeemerExample5 = Data (Plutus.I 42)

validatingRedeemersEx5 :: Redeemers A
validatingRedeemersEx5 =
  Redeemers $
    Map.singleton (RdmrPtr Rewrd 0) (redeemerExample5, ExUnits 5000 5000)

validatingBodyWithWithdrawal :: TxBody A
validatingBodyWithWithdrawal =
  TxBody
    (Set.singleton $ TxIn genesisId 5) --inputs
    (Set.singleton $ TxIn genesisId 15) --collateral
    (StrictSeq.singleton outEx5) --outputs
    StrictSeq.empty
    ( Wdrl $
        Map.singleton
          (RewardAcnt Testnet scriptStakeCredSuceed)
          (Coin 1000) --txwdrls
    )
    (Coin 5) --txfee
    (ValidityInterval SNothing SNothing) --txvldt
    SNothing --txUpdates
    mempty -- reqSignerHashes
    mempty --mint
    (hashWPPD validatingRedeemersEx5) --wppHash
    SNothing --adHash
    SNothing --network id

validatingTxWithWithdrawal :: ValidatedTx A
validatingTxWithWithdrawal =
  ValidatedTx
    validatingBodyWithWithdrawal
    TxWitness
      { txwitsVKey =
          Set.singleton $
            makeWitnessVKey
              (hashAnnotated validatingBodyWithWithdrawal)
              someKeys,
        txwitsBoot = mempty,
        txscripts =
          Map.singleton alwaysSucceedsHash2 (alwaysSucceeds 2),
        txdats = mempty,
        txrdmrs = validatingRedeemersEx5
      }
    (IsValidating True)
    SNothing

utxoEx5 :: UTxO A
utxoEx5 = expectedUTxO (ExpectSuccess validatingBodyWithWithdrawal outEx5) 5

utxoStEx5 :: UTxOState A
utxoStEx5 = UTxOState utxoEx5 (Coin 0) (Coin 5) def

-- ===========================================================================
--  Example 6: Process a WITHDRAWAL transaction with a failing Plutus script.
-- ===========================================================================

outEx6 :: TxOut A
outEx6 = TxOut someAddr (Val.inject $ Coin 1995) SNothing

redeemerExample6 :: Data A
redeemerExample6 = Data (Plutus.I 0)

notValidatingRedeemersEx6 :: Redeemers A
notValidatingRedeemersEx6 =
  Redeemers $
    Map.singleton (RdmrPtr Rewrd 0) (redeemerExample6, ExUnits 5000 5000)

notValidatingBodyWithWithdrawal :: TxBody A
notValidatingBodyWithWithdrawal =
  TxBody
    (Set.singleton $ TxIn genesisId 6) --inputs
    (Set.singleton $ TxIn genesisId 16) --collateral
    (StrictSeq.singleton outEx6) --outputs
    StrictSeq.empty
    ( Wdrl $
        Map.singleton
          (RewardAcnt Testnet scriptStakeCredFail)
          (Coin 1000) --txwdrls
    )
    (Coin 5) --txfee
    (ValidityInterval SNothing SNothing) --txvldt
    SNothing --txUpdates
    mempty -- reqSignerHashes
    mempty --mint
    (hashWPPD notValidatingRedeemersEx6) --wppHash
    SNothing --adHash
    SNothing --network id

notValidatingTxWithWithdrawal :: ValidatedTx A
notValidatingTxWithWithdrawal =
  ValidatedTx
    notValidatingBodyWithWithdrawal
    TxWitness
      { txwitsVKey =
          Set.singleton $
            makeWitnessVKey
              (hashAnnotated notValidatingBodyWithWithdrawal)
              someKeys,
        txwitsBoot = mempty,
        txscripts =
          Map.singleton alwaysFailsHash1 (alwaysFails 1),
        txdats = mempty,
        txrdmrs = notValidatingRedeemersEx6
      }
    (IsValidating False)
    SNothing

utxoEx6 :: UTxO A
utxoEx6 = expectedUTxO ExpectFailure 6

utxoStEx6 :: UTxOState A
utxoStEx6 = UTxOState utxoEx6 (Coin 0) (Coin 1000) def

-- =============================================================================
--  Example 7: Process a MINT transaction with a succeeding Plutus script.
-- =============================================================================

pidEx7 :: PolicyID C_Crypto
pidEx7 = PolicyID alwaysSucceedsHash2

an :: AssetName
an = AssetName $ BS.pack "an"

mintEx7 :: Value C_Crypto
mintEx7 =
  Value 0 $
    Map.singleton pidEx7 (Map.singleton an 1)

outEx7 :: TxOut A
outEx7 = TxOut someAddr (mintEx7 <+> Val.inject (Coin 995)) SNothing

redeemerExample7 :: Data A
redeemerExample7 = Data (Plutus.I 42)

validatingRedeemersEx7 :: Redeemers A
validatingRedeemersEx7 =
  Redeemers $
    Map.singleton (RdmrPtr Mint 0) (redeemerExample7, ExUnits 5000 5000)

validatingBodyWithMint :: TxBody A
validatingBodyWithMint =
  TxBody
    (Set.singleton $ TxIn genesisId 7) --inputs
    (Set.singleton $ TxIn genesisId 17) --collateral
    (StrictSeq.singleton outEx7) --outputs
    StrictSeq.empty
    (Wdrl mempty)
    (Coin 5) --txfee
    (ValidityInterval SNothing SNothing) --txvldt
    SNothing --txUpdates
    mempty -- reqSignerHashes
    mintEx7 --mint
    (hashWPPD validatingRedeemersEx7) --wppHash
    SNothing --adHash
    SNothing --network id

validatingTxWithMint :: ValidatedTx A
validatingTxWithMint =
  ValidatedTx
    validatingBodyWithMint
    TxWitness
      { txwitsVKey =
          Set.singleton $
            makeWitnessVKey
              (hashAnnotated validatingBodyWithMint)
              someKeys,
        txwitsBoot = mempty,
        txscripts =
          Map.singleton alwaysSucceedsHash2 (alwaysSucceeds 2),
        txdats = mempty,
        txrdmrs = validatingRedeemersEx7
      }
    (IsValidating True)
    SNothing

utxoEx7 :: UTxO A
utxoEx7 = expectedUTxO (ExpectSuccess validatingBodyWithMint outEx7) 7

utxoStEx7 :: UTxOState A
utxoStEx7 = UTxOState utxoEx7 (Coin 0) (Coin 5) def

-- ==============================================================================
--  Example 8: Process a MINT transaction with a failing Plutus script.
-- ==============================================================================

pidEx8 :: PolicyID C_Crypto
pidEx8 = PolicyID alwaysFailsHash1

mintEx8 :: Value C_Crypto
mintEx8 =
  Value 0 $
    Map.singleton pidEx8 (Map.singleton an 1)

outEx8 :: TxOut A
outEx8 = TxOut someAddr (mintEx8 <+> Val.inject (Coin 995)) SNothing

redeemerExample8 :: Data A
redeemerExample8 = Data (Plutus.I 0)

notValidatingRedeemersEx8 :: Redeemers A
notValidatingRedeemersEx8 =
  Redeemers $
    Map.singleton (RdmrPtr Mint 0) (redeemerExample8, ExUnits 5000 5000)

notValidatingBodyWithMint :: TxBody A
notValidatingBodyWithMint =
  TxBody
    (Set.singleton $ TxIn genesisId 8) --inputs
    (Set.singleton $ TxIn genesisId 18) --collateral
    (StrictSeq.singleton outEx8) --outputs
    StrictSeq.empty
    (Wdrl mempty)
    (Coin 5) --txfee
    (ValidityInterval SNothing SNothing) --txvldt
    SNothing --txUpdates
    mempty -- reqSignerHashes
    mintEx8 --mint
    (hashWPPD notValidatingRedeemersEx8) --wppHash
    SNothing --adHash
    SNothing --network id

notValidatingTxWithMint :: ValidatedTx A
notValidatingTxWithMint =
  ValidatedTx
    notValidatingBodyWithMint
    TxWitness
      { txwitsVKey =
          Set.singleton $
            makeWitnessVKey
              (hashAnnotated notValidatingBodyWithMint)
              someKeys,
        txwitsBoot = mempty,
        txscripts =
          Map.singleton alwaysFailsHash1 (alwaysFails 1),
        txdats = mempty,
        txrdmrs = notValidatingRedeemersEx8
      }
    (IsValidating False)
    SNothing

utxoEx8 :: UTxO A
utxoEx8 = expectedUTxO ExpectFailure 8

utxoStEx8 :: UTxOState A
utxoStEx8 = UTxOState utxoEx8 (Coin 0) (Coin 1000) def

-- ====================================================================================
--  Example 9: Process a transaction with a succeeding script in every place possible,
--  and also with succeeding timelock scripts.
-- ====================================================================================

validatingRedeemersEx9 :: Redeemers A
validatingRedeemersEx9 =
  Redeemers . Map.fromList $
    [ (RdmrPtr Spend 0, (Data (Plutus.I 101), ExUnits 5000 5000)),
      (RdmrPtr Cert 1, (Data (Plutus.I 102), ExUnits 5000 5000)),
      (RdmrPtr Rewrd 1, (Data (Plutus.I 103), ExUnits 5000 5000)),
      (RdmrPtr Mint 1, (Data (Plutus.I 104), ExUnits 5000 5000))
    ]

pidEx9 :: PolicyID C_Crypto
pidEx9 = PolicyID timelockHash1

mintEx9 :: Value C_Crypto
mintEx9 =
  Value 0 $
    Map.fromList
      [ (pidEx7, Map.singleton an 1),
        (pidEx9, Map.singleton an 1)
      ]

outEx9 :: TxOut A
outEx9 = TxOut someAddr (mintEx9 <+> Val.inject (Coin 4996)) SNothing

timelockStakeCred :: StakeCredential C_Crypto
timelockStakeCred = ScriptHashObj timelockHash2

validatingBodyManyScripts :: TxBody A
validatingBodyManyScripts =
  TxBody
    (Set.fromList [TxIn genesisId 1, TxIn genesisId 100]) --inputs
    (Set.singleton $ TxIn genesisId 11) --collateral
    (StrictSeq.singleton outEx9) --outputs
    ( StrictSeq.fromList
        [ DCertDeleg (DeRegKey timelockStakeCred),
          DCertDeleg (DeRegKey scriptStakeCredSuceed)
        ] --txcerts
    )
    ( Wdrl $
        Map.fromList
          [ (RewardAcnt Testnet scriptStakeCredSuceed, Coin 0),
            (RewardAcnt Testnet timelockStakeCred, Coin 0)
          ] --txwdrls
    )
    (Coin 5) --txfee
    (ValidityInterval SNothing (SJust $ SlotNo 1)) --txvldt
    SNothing --txUpdates
    mempty -- reqSignerHashes
    mintEx9 --mint
    (hashWPPD validatingRedeemersEx9) --wppHash
    SNothing --adHash
    SNothing --network id

validatingTxManyScripts :: ValidatedTx A
validatingTxManyScripts =
  ValidatedTx
    validatingBodyManyScripts
    TxWitness
      { txwitsVKey =
          Set.fromList
            [ makeWitnessVKey (hashAnnotated validatingBodyManyScripts) someKeys,
              makeWitnessVKey (hashAnnotated validatingBodyManyScripts) timelockKeys
            ],
        txwitsBoot = mempty,
        txscripts =
          Map.fromList
            [ (alwaysSucceedsHash3, alwaysSucceeds 3),
              (alwaysSucceedsHash2, alwaysSucceeds 2),
              (timelockHash0, timelockScript 0),
              (timelockHash1, timelockScript 1),
              (timelockHash2, timelockScript 2)
            ],
        txdats = Map.singleton (hashData datumExample1) datumExample1,
        txrdmrs = validatingRedeemersEx9
      }
    (IsValidating True)
    SNothing

utxoEx9 :: UTxO A
utxoEx9 = UTxO utxo
  where
    utxo =
      Map.insert (TxIn (txid @A validatingBodyManyScripts) 0) outEx9 $
        Map.filterWithKey
          (\k _ -> k /= (TxIn genesisId 1) && k /= (TxIn genesisId 100))
          (unUTxO initUTxO)

utxoStEx9 :: UTxOState A
utxoStEx9 = UTxOState utxoEx9 (Coin 0) (Coin 5) def

-- =======================
-- Invalid Transactions
-- =======================

incorrectNetworkIDTxBody :: TxBody A
incorrectNetworkIDTxBody =
  TxBody
    (Set.singleton $ TxIn genesisId 3) --inputs
    mempty --collateral
    (StrictSeq.singleton (TxOut someAddr (Val.inject $ Coin 995) SNothing)) --outputs
    StrictSeq.empty --txcerts
    (Wdrl mempty) --txwdrls
    (Coin 5) --txfee
    (ValidityInterval SNothing SNothing) --txvldt
    SNothing --txUpdates
    mempty -- reqSignerHashes
    mempty --mint
    SNothing --wppHash
    SNothing --adHash
    (SJust Mainnet) --network id

incorrectNetworkIDTx :: ValidatedTx A
incorrectNetworkIDTx =
  ValidatedTx
    incorrectNetworkIDTxBody
    TxWitness
      { txwitsVKey =
          Set.singleton $
            makeWitnessVKey (hashAnnotated incorrectNetworkIDTxBody) someKeys,
        txwitsBoot = mempty,
        txscripts = mempty,
        txdats = mempty,
        txrdmrs = Redeemers mempty
      }
    (IsValidating True)
    SNothing

extraneousKeyHash :: KeyHash 'Witness C_Crypto
extraneousKeyHash = hashKey . snd . mkKeyPair @C_Crypto $ (0, 0, 0, 0, 99)

missingRequiredWitnessTxBody :: TxBody A
missingRequiredWitnessTxBody =
  TxBody
    (Set.singleton $ TxIn genesisId 3) --inputs
    mempty --collateral
    (StrictSeq.singleton (TxOut someAddr (Val.inject $ Coin 995) SNothing)) --outputs
    StrictSeq.empty --txcerts
    (Wdrl mempty) --txwdrls
    (Coin 5) --txfee
    (ValidityInterval SNothing SNothing) --txvldt
    SNothing --txUpdates
    (Set.singleton extraneousKeyHash) -- reqSignerHashes
    mempty --mint
    SNothing --wppHash
    SNothing --adHash
    (SJust Testnet) --network id

missingRequiredWitnessTx :: ValidatedTx A
missingRequiredWitnessTx =
  ValidatedTx
    missingRequiredWitnessTxBody
    TxWitness
      { txwitsVKey =
          Set.singleton $
            makeWitnessVKey (hashAnnotated missingRequiredWitnessTxBody) someKeys,
        txwitsBoot = mempty,
        txscripts = mempty,
        txdats = mempty,
        txrdmrs = Redeemers mempty
      }
    (IsValidating True)
    SNothing

missingRedeemerTxBody :: TxBody A
missingRedeemerTxBody =
  TxBody
    (Set.singleton $ TxIn genesisId 1) --inputs
    (Set.singleton $ TxIn genesisId 11) --collateral
    (StrictSeq.singleton outEx1) --outputs
    StrictSeq.empty --txcerts
    (Wdrl mempty) --txwdrls
    (Coin 5) --txfee
    (ValidityInterval SNothing SNothing) --txvldt
    SNothing --txUpdates
    (Set.singleton . asWitness . hashKey . vKey $ someKeys) -- reqSignerHashes
    mempty --mint
    (hashWPPD (Redeemers mempty)) --wppHash
    SNothing --adHash
    SNothing --network id

missingRedeemerTx :: ValidatedTx A
missingRedeemerTx =
  ValidatedTx
    missingRedeemerTxBody
    TxWitness
      { txwitsVKey =
          Set.singleton $
            makeWitnessVKey (hashAnnotated missingRedeemerTxBody) someKeys,
        txwitsBoot = mempty,
        txscripts =
          Map.singleton alwaysSucceedsHash3 (alwaysSucceeds 3),
        txdats = Map.singleton (hashData datumExample1) datumExample1,
        txrdmrs = Redeemers mempty
      }
    (IsValidating True)
    SNothing

wrongRedeemerHashTx :: ValidatedTx A
wrongRedeemerHashTx =
  ValidatedTx
    missingRedeemerTxBody
    TxWitness
      { txwitsVKey =
          Set.singleton $
            makeWitnessVKey (hashAnnotated missingRedeemerTxBody) someKeys,
        txwitsBoot = mempty,
        txscripts =
          Map.singleton alwaysSucceedsHash3 (alwaysSucceeds 3),
        txdats = Map.singleton (hashData datumExample1) datumExample1,
        txrdmrs = validatingRedeemersEx1
      }
    (IsValidating True)
    SNothing

missing1phaseScriptWitnessTx :: ValidatedTx A
missing1phaseScriptWitnessTx =
  ValidatedTx
    validatingBodyManyScripts
    TxWitness
      { txwitsVKey =
          Set.fromList
            [ makeWitnessVKey (hashAnnotated validatingBodyManyScripts) someKeys,
              makeWitnessVKey (hashAnnotated validatingBodyManyScripts) timelockKeys
            ],
        txwitsBoot = mempty,
        txscripts =
          Map.fromList
            [ (alwaysSucceedsHash3, alwaysSucceeds 3),
              (alwaysSucceedsHash2, alwaysSucceeds 2),
              -- intentionally missing -> (timelockHash0, timelockScript 0),
              (timelockHash1, timelockScript 1),
              (timelockHash2, timelockScript 2)
            ],
        txdats = Map.singleton (hashData datumExample1) datumExample1,
        txrdmrs = validatingRedeemersEx9
      }
    (IsValidating True)
    SNothing

missing2phaseScriptWitnessTx :: ValidatedTx A
missing2phaseScriptWitnessTx =
  ValidatedTx
    validatingBodyManyScripts
    TxWitness
      { txwitsVKey =
          Set.fromList
            [ makeWitnessVKey (hashAnnotated validatingBodyManyScripts) someKeys,
              makeWitnessVKey (hashAnnotated validatingBodyManyScripts) timelockKeys
            ],
        txwitsBoot = mempty,
        txscripts =
          Map.fromList
            [ (alwaysSucceedsHash3, alwaysSucceeds 3),
              -- intentionally missing -> (alwaysSucceedsHash2, alwaysSucceeds 2),
              (timelockHash0, timelockScript 0),
              (timelockHash1, timelockScript 1),
              (timelockHash2, timelockScript 2)
            ],
        txdats = Map.singleton (hashData datumExample1) datumExample1,
        txrdmrs = validatingRedeemersEx9
      }
    (IsValidating True)
    SNothing

misPurposedRedeemer :: Redeemers A
misPurposedRedeemer =
  Redeemers $
    -- The label *should* be Spend, not Mint
    Map.singleton (RdmrPtr Mint 1) (redeemerExample1, ExUnits 5000 5000)

wrongRedeemerLabelTxBody :: TxBody A
wrongRedeemerLabelTxBody =
  TxBody
    (Set.singleton $ TxIn genesisId 1) --inputs
    (Set.singleton $ TxIn genesisId 11) --collateral
    (StrictSeq.singleton outEx1) --outputs
    StrictSeq.empty --txcerts
    (Wdrl mempty) --txwdrls
    (Coin 5) --txfee
    (ValidityInterval SNothing SNothing) --txvldt
    SNothing --txUpdates
    (Set.singleton . asWitness . hashKey . vKey $ someKeys) -- reqSignerHashes
    mempty --mint
    (hashWPPD misPurposedRedeemer) --wppHash
    SNothing --adHash
    SNothing --network id

wrongRedeemerLabelTx :: ValidatedTx A
wrongRedeemerLabelTx =
  ValidatedTx
    wrongRedeemerLabelTxBody
    TxWitness
      { txwitsVKey =
          Set.singleton $
            makeWitnessVKey (hashAnnotated wrongRedeemerLabelTxBody) someKeys,
        txwitsBoot = mempty,
        txscripts =
          Map.singleton alwaysSucceedsHash3 (alwaysSucceeds 3),
        txdats = Map.singleton (hashData datumExample1) datumExample1,
        txrdmrs = misPurposedRedeemer
      }
    (IsValidating True)
    SNothing

missingDatumTx :: ValidatedTx A
missingDatumTx =
  ValidatedTx
    validatingBody
    TxWitness
      { txwitsVKey =
          Set.singleton $
            makeWitnessVKey (hashAnnotated validatingBody) someKeys,
        txwitsBoot = mempty,
        txscripts =
          Map.singleton alwaysSucceedsHash3 (alwaysSucceeds 3),
        txdats = mempty,
        txrdmrs = validatingRedeemersEx1
      }
    (IsValidating True)
    SNothing

phase1FailureTx :: ValidatedTx A
phase1FailureTx =
  ValidatedTx
    validatingBodyManyScripts
    TxWitness
      { txwitsVKey =
          Set.singleton $
            makeWitnessVKey (hashAnnotated validatingBodyManyScripts) someKeys,
        txwitsBoot = mempty,
        txscripts =
          Map.fromList
            [ (alwaysSucceedsHash3, alwaysSucceeds 3),
              (alwaysSucceedsHash2, alwaysSucceeds 2),
              (timelockHash0, timelockScript 0),
              (timelockHash1, timelockScript 1),
              (timelockHash2, timelockScript 2)
            ],
        txdats = Map.singleton (hashData datumExample1) datumExample1,
        txrdmrs = validatingRedeemersEx9
      }
    (IsValidating True)
    SNothing

mislabelValidAsInvalid :: ValidatedTx A
mislabelValidAsInvalid =
  ValidatedTx
    validatingBody
    TxWitness
      { txwitsVKey =
          Set.singleton $
            makeWitnessVKey (hashAnnotated validatingBody) someKeys,
        txwitsBoot = mempty,
        txscripts =
          Map.singleton alwaysSucceedsHash3 (alwaysSucceeds 3),
        txdats = Map.singleton (hashData datumExample1) datumExample1,
        txrdmrs = validatingRedeemersEx1
      }
    (IsValidating False)
    SNothing

mislabelInvalidAsValid :: ValidatedTx A
mislabelInvalidAsValid =
  ValidatedTx
    notValidatingBody
    TxWitness
      { txwitsVKey =
          Set.singleton $
            makeWitnessVKey (hashAnnotated notValidatingBody) someKeys,
        txwitsBoot = mempty,
        txscripts =
          Map.singleton alwaysFailsHash0 (alwaysFails 0),
        txdats = Map.singleton (hashData datumExample2) datumExample2,
        txrdmrs = notValidatingRedeemers
      }
    (IsValidating True)
    SNothing

validatingRedeemersTooManyExUnits :: Redeemers A
validatingRedeemersTooManyExUnits =
  Redeemers $
    Map.singleton (RdmrPtr Spend 0) (redeemerExample1, ExUnits 1000001 5000)

tooManyExUnitsTxBody :: TxBody A
tooManyExUnitsTxBody =
  TxBody
    (Set.singleton $ TxIn genesisId 1) --inputs
    (Set.singleton $ TxIn genesisId 11) --collateral
    (StrictSeq.singleton outEx1) --outputs
    StrictSeq.empty --txcerts
    (Wdrl mempty) --txwdrls
    (Coin 5) --txfee
    (ValidityInterval SNothing SNothing) --txvldt
    SNothing --txUpdates
    (Set.singleton . asWitness . hashKey . vKey $ someKeys) -- reqSignerHashes
    mempty --mint
    (hashWPPD validatingRedeemersTooManyExUnits) --wppHash
    SNothing --adHash
    SNothing --network id

tooManyExUnitsTx :: ValidatedTx A
tooManyExUnitsTx =
  ValidatedTx
    tooManyExUnitsTxBody
    TxWitness
      { txwitsVKey =
          Set.singleton $
            makeWitnessVKey (hashAnnotated tooManyExUnitsTxBody) someKeys,
        txwitsBoot = mempty,
        txscripts =
          Map.singleton alwaysSucceedsHash3 (alwaysSucceeds 3),
        txdats = Map.singleton (hashData datumExample1) datumExample1,
        txrdmrs = validatingRedeemersTooManyExUnits
      }
    (IsValidating True)
    SNothing

-- =======
--  Tests
-- =======

plutusScriptExamples :: TestTree
plutusScriptExamples =
  testGroup
    "run plutus script directly"
    [ testCase "always true" $
        case P.evaluateScriptRestricting
          P.Verbose
          costModel
          (P.ExBudget (P.ExCPU 1) (P.ExMemory 2))
          (alwaysSucceedingNAryFunction 0)
          [] of
          (_, Left e) -> assertBool ("This script should have succeeded, but: " <> show e) False
          (_, Right _) -> assertBool "" True,
      testCase "always false" $
        case P.evaluateScriptRestricting
          P.Verbose
          costModel
          (P.ExBudget (P.ExCPU 1) (P.ExMemory 2))
          (alwaysFailingNAryFunction 0)
          [] of
          (_, Left (P.CekError _)) -> assertBool "" True -- TODO rule out cost model failure
          (_, Left e) -> assertBool ("Not the script failure we expected: " <> show e) False
          (_, Right _) -> assertBool "This script should have failed" False
    ]
  where
    costModel = fromMaybe (error "corrupt default cost model") P.defaultCostModelParams

testUTXOW ::
  UTxOState A ->
  ValidatedTx A ->
  Either [[PredicateFailure (AlonzoUTXOW A)]] (UTxOState A) ->
  Assertion
testUTXOW initSt tx (Right expectedSt) =
  checkTrace @(AlonzoUTXOW A) runShelleyBase utxoEnv $
    pure initSt .- tx .-> expectedSt
testUTXOW initSt tx predicateFailure@(Left _) = do
  let st = runShelleyBase $ applySTSTest @(AlonzoUTXOW A) (TRC (utxoEnv, initSt, tx))
  st @?= predicateFailure

collectTwoPhaseScriptInputsOutputOrdering :: Assertion
collectTwoPhaseScriptInputsOutputOrdering =
  collectTwoPhaseScriptInputs testEpochInfo testSystemStart pp validatingTx initUTxO
    @?= Right
      [ ( alwaysSucceeds 3,
          [datumExample1, redeemerExample1, context],
          ExUnits 5000 5000,
          CostModel mempty
        )
      ]
  where
    context =
      valContext
        (txInfo testEpochInfo testSystemStart initUTxO validatingTx)
        (Spending $ TxIn genesisId 1)

utxowExamples :: TestTree
utxowExamples =
  testGroup
    "utxow examples"
    [ testGroup
        "valid transactions"
        [ testCase "validating SPEND script" $
            testUTXOW
              initialUtxoSt
              validatingTx
              (Right utxoStEx1),
          testCase "not validating SPEND script" $
            testUTXOW
              initialUtxoSt
              notValidatingTx
              (Right utxoStEx2),
          testCase "validating CERT script" $
            testUTXOW
              initialUtxoSt
              validatingTxWithCert
              (Right utxoStEx3),
          testCase "not validating CERT script" $
            testUTXOW
              initialUtxoSt
              notValidatingTxWithCert
              (Right utxoStEx4),
          testCase "validating WITHDRAWAL script" $
            testUTXOW
              initialUtxoSt
              validatingTxWithWithdrawal
              (Right utxoStEx5),
          testCase "not validating WITHDRAWAL script" $
            testUTXOW
              initialUtxoSt
              notValidatingTxWithWithdrawal
              (Right utxoStEx6),
          testCase "validating MINT script" $
            testUTXOW
              initialUtxoSt
              validatingTxWithMint
              (Right utxoStEx7),
          testCase "not validating MINT script" $
            testUTXOW
              initialUtxoSt
              notValidatingTxWithMint
              (Right utxoStEx8),
          testCase "validating scripts everywhere" $
            testUTXOW
              initialUtxoSt
              validatingTxManyScripts
              (Right utxoStEx9)
        ],
      testGroup
        "invalid transactions"
        [ testCase "wrong network ID" $
            testUTXOW
              initialUtxoSt
              incorrectNetworkIDTx
              ( Left
                  [ [ WrappedShelleyEraFailure
                        (UtxoFailure (WrongNetworkInTxBody Testnet Mainnet))
                    ]
                  ]
              ),
          testCase "missing required key witness" $
            testUTXOW
              initialUtxoSt
              missingRequiredWitnessTx
              ( Left [[(MissingRequiredSigners . Set.singleton) extraneousKeyHash]]
              ),
          testCase "missing redeemer" $
            testUTXOW
              initialUtxoSt
              missingRedeemerTx
              ( Left
                  [ [ WrappedShelleyEraFailure . UtxoFailure
                        . UtxosFailure
                        . CollectErrors
                        $ [NoRedeemer (Spending (TxIn genesisId 1))],
                      UnRedeemableScripts
                        [ ( Spending (TxIn genesisId 1),
                            alwaysSucceedsHash3
                          )
                        ]
                    ]
                  ]
              ),
          testCase "wrong redeemer hash" $
            testUTXOW
              initialUtxoSt
              wrongRedeemerHashTx
              ( Left
                  [ [ PPViewHashesDontMatch
                        (hashWPPD (Redeemers mempty))
                        (hashWPPD validatingRedeemersEx1)
                    ]
                  ]
              ),
          testCase "missing 1-phase script witness" $
            testUTXOW
              initialUtxoSt
              missing1phaseScriptWitnessTx
              ( Left
                  [ [ WrappedShelleyEraFailure . UtxoFailure . UtxosFailure . CollectErrors $
                        [ NoRedeemer (Spending (TxIn genesisId 100)),
                          NoWitness (timelockHash0)
                        ],
                      WrappedShelleyEraFailure . MissingScriptWitnessesUTXOW . Set.singleton $
                        timelockHash0,
                      UnRedeemableScripts [(Spending $ TxIn genesisId 100, timelockHash0)]
                    ]
                  ]
              ),
          testCase "missing 2-phase script witness" $
            testUTXOW
              initialUtxoSt
              missing2phaseScriptWitnessTx
              ( Left
                  [ [ WrappedShelleyEraFailure . UtxoFailure . UtxosFailure . CollectErrors $
                        [ NoWitness alwaysSucceedsHash2,
                          NoWitness alwaysSucceedsHash2,
                          NoWitness alwaysSucceedsHash2
                        ],
                      WrappedShelleyEraFailure . MissingScriptWitnessesUTXOW . Set.singleton $
                        alwaysSucceedsHash2,
                      UnRedeemableScripts
                        [ ( Rewarding
                              ( RewardAcnt
                                  { getRwdNetwork = Testnet,
                                    getRwdCred = ScriptHashObj alwaysSucceedsHash2
                                  }
                              ),
                            alwaysSucceedsHash2
                          ),
                          ( Certifying . DCertDeleg . DeRegKey . ScriptHashObj $
                              alwaysSucceedsHash2,
                            alwaysSucceedsHash2
                          ),
                          ( Minting (PolicyID {policyID = alwaysSucceedsHash2}),
                            alwaysSucceedsHash2
                          )
                        ]
                    ]
                  ]
              ),
          testCase "redeemer with incorrect label" $
            testUTXOW
              initialUtxoSt
              wrongRedeemerLabelTx
              ( Left
                  [ [ WrappedShelleyEraFailure . UtxoFailure
                        . UtxosFailure
                        . CollectErrors
                        $ [NoRedeemer (Spending (TxIn genesisId 1))],
                      UnRedeemableScripts
                        [ ( Spending (TxIn genesisId 1),
                            alwaysSucceedsHash3
                          )
                        ]
                    ]
                  ]
              ),
          testCase "missing datum" $
            testUTXOW
              initialUtxoSt
              missingDatumTx
              ( Left
                  [ [ DataHashSetsDontAgree
                        mempty
                        (Set.singleton $ hashData datumExample1)
                    ]
                  ]
              ),
          testCase "phase 1 script failure" $
            testUTXOW
              initialUtxoSt
              phase1FailureTx
              ( Left
                  [ [ WrappedShelleyEraFailure . ScriptWitnessNotValidatingUTXOW $
                        Set.fromList [timelockHash0, timelockHash1, timelockHash2]
                    ]
                  ]
              ),
          testCase "valid transaction marked as invalid" $
            testUTXOW
              initialUtxoSt
              mislabelValidAsInvalid
              ( Left
                  [ [ WrappedShelleyEraFailure
                        ( UtxoFailure
                            (UtxosFailure (ValidationTagMismatch (IsValidating False)))
                        )
                    ]
                  ]
              ),
          testCase "invalid transaction marked as valid" $
            testUTXOW
              initialUtxoSt
              mislabelInvalidAsValid
              ( Left
                  [ [ WrappedShelleyEraFailure
                        (UtxoFailure (UtxosFailure (ValidationTagMismatch (IsValidating True))))
                    ]
                  ]
              ),
          testCase "too many execution units for tx" $
            testUTXOW
              initialUtxoSt
              tooManyExUnitsTx
              ( Left
                  [ [ WrappedShelleyEraFailure . UtxoFailure $
                        ExUnitsTooBigUTxO
                          (ExUnits {exUnitsMem = 1000000, exUnitsSteps = 1000000})
                          (ExUnits {exUnitsMem = 1000001, exUnitsSteps = 5000})
                    ]
                  ]
              )
        ],
      testCase
        "collectTwoPhaseScriptInputs output order"
        collectTwoPhaseScriptInputsOutputOrdering
    ]
