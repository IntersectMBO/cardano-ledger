{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Alonzo.Examples.Utxow
  ( plutusScriptExamples,
    utxowExamples,
  )
where

import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.Data (Data (..), hashData)
import Cardano.Ledger.Alonzo.Language (Language (..))
import Cardano.Ledger.Alonzo.PParams (PParams, PParams' (..))
import Cardano.Ledger.Alonzo.Rules.Utxow (AlonzoUTXOW)
import Cardano.Ledger.Alonzo.Scripts
  ( CostModel (..),
    ExUnits (..),
    Script,
    Tag (..),
    alwaysFails,
    alwaysSucceeds,
  )
import Cardano.Ledger.Alonzo.Tx
  ( IsValidating (..),
    ValidatedTx (..),
    hashWitnessPPData,
  )
import Cardano.Ledger.Alonzo.TxBody (TxBody (..), TxOut (..))
import Cardano.Ledger.Alonzo.TxWitness
  ( RdmrPtr (..),
    Redeemers (..),
    TxWitness (..),
  )
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Era (ValidateScript (..))
import Cardano.Ledger.Mary.Value
  ( AssetName (..),
    PolicyID (..),
    Value (..),
  )
import Cardano.Ledger.SafeHash (hashAnnotated)
import Cardano.Ledger.ShelleyMA.Timelocks (ValidityInterval (..))
import Cardano.Ledger.Val ((<+>))
import qualified Cardano.Ledger.Val as Val
import Control.State.Transition.Extended hiding (Assertion)
import Control.State.Transition.Trace (checkTrace, (.-), (.->))
import qualified Data.ByteString.Char8 as BS
import Data.Default.Class (def)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
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
import qualified PlutusCore.Evaluation.Machine.ExBudgetingDefaults as P (defaultCostModelParams)
import qualified PlutusCore.Evaluation.Machine.ExMemory as P (ExCPU (..), ExMemory (..))
import qualified PlutusTx as Plutus
import Shelley.Spec.Ledger.Address (Addr (..))
import Shelley.Spec.Ledger.BaseTypes (Network (..), StrictMaybe (..))
import Shelley.Spec.Ledger.Credential (Credential (..), StakeCredential, StakeReference (..))
import Shelley.Spec.Ledger.Keys (GenDelegs (..), KeyPair (..), KeyRole (..), hashKey)
import Shelley.Spec.Ledger.LedgerState (UTxOState (..))
import Shelley.Spec.Ledger.STS.Utxo (UtxoEnv (..))
import Shelley.Spec.Ledger.Slot (SlotNo (..))
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

feeKeys :: KeyPair 'Payment C_Crypto
feeKeys = KeyPair vk sk
  where
    (sk, vk) = mkKeyPair @C_Crypto (0, 0, 0, 0, 1)

feeAddr :: Addr C_Crypto
feeAddr = Addr Testnet pCred sCred
  where
    (_ssk, svk) = mkKeyPair @C_Crypto (0, 0, 0, 0, 2)
    pCred = KeyHashObj . hashKey . vKey $ feeKeys
    sCred = StakeRefBase . KeyHashObj . hashKey $ svk

feeOutput :: TxOut A
feeOutput =
  TxOut
    feeAddr
    (Val.inject $ Coin 1000)
    SNothing

initUTxO :: UTxO A
initUTxO =
  UTxO $
    Map.fromList
      [ (TxIn genesisId 0, alwaysSucceedsOutput),
        (TxIn genesisId 1, alwaysFailsOutput),
        (TxIn genesisId 2, feeOutput)
      ]

initUtxoSt :: UTxOState A
initUtxoSt = UTxOState initUTxO (Coin 0) (Coin 0) def

-- =========================================================================
--  Example 1: Process a SPEND transaction with a succeeding Plutus script.
-- =========================================================================

datumExample1 :: Data A
datumExample1 = Data (Plutus.I 0)

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
outEx1 = TxOut feeAddr (Val.inject $ Coin 5995) SNothing

validatingBody :: TxBody A
validatingBody =
  TxBody
    (Set.singleton $ TxIn genesisId 0) --inputs
    (Set.singleton $ TxIn genesisId 2) --txinputs_fee
    (StrictSeq.singleton outEx1) --outputs
    StrictSeq.empty --txcerts
    (Wdrl mempty) --txwdrls
    (Coin 5) --txfee
    (ValidityInterval SNothing SNothing) --txvldt
    SNothing --txUpdates
    mempty -- reqSignerHashes
    mempty --mint
    (hashWitnessPPData pp (Set.singleton PlutusV1) validatingRedeemersEx1) --wppHash
    SNothing --adHash
    SNothing --network id

validatingTx :: ValidatedTx A
validatingTx =
  ValidatedTx
    validatingBody
    TxWitness
      { txwitsVKey = Set.singleton $ makeWitnessVKey (hashAnnotated validatingBody) feeKeys,
        txwitsBoot = mempty,
        txscripts =
          Map.singleton (hashScript @A $ alwaysSucceeds 3) (alwaysSucceeds 3),
        txdats = Map.singleton (hashData datumExample1) datumExample1,
        txrdmrs = validatingRedeemersEx1
      }
    (IsValidating True)
    SNothing

utxoEx1 :: UTxO A
utxoEx1 =
  UTxO $
    Map.fromList
      [ (TxIn genesisId 1, alwaysFailsOutput),
        (TxIn (txid @A validatingBody) 0, outEx1)
      ]

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
outEx2 = TxOut feeAddr (Val.inject $ Coin 3995) SNothing

notValidatingBody :: TxBody A
notValidatingBody =
  TxBody
    (Set.singleton $ TxIn genesisId 1) --inputs
    (Set.singleton $ TxIn genesisId 2) --txinputs_fee
    (StrictSeq.singleton outEx2) --outputs
    StrictSeq.empty --txcerts
    (Wdrl mempty) --txwdrls
    (Coin 5) --txfee
    (ValidityInterval SNothing SNothing) --txvldt
    SNothing --txUpdates
    mempty -- reqSignerHashes
    mempty --mint
    (hashWitnessPPData pp (Set.singleton PlutusV1) notValidatingRedeemers) --wppHash
    SNothing --adHash
    SNothing --network id

notValidatingTx :: ValidatedTx A
notValidatingTx =
  ValidatedTx
    notValidatingBody
    TxWitness
      { txwitsVKey = Set.singleton $ makeWitnessVKey (hashAnnotated notValidatingBody) feeKeys,
        txwitsBoot = mempty,
        txscripts =
          Map.singleton (hashScript @A $ alwaysFails 0) (alwaysFails 0),
        txdats = Map.singleton (hashData datumExample2) datumExample2,
        txrdmrs = notValidatingRedeemers
      }
    (IsValidating False)
    SNothing

utxoEx2 :: UTxO A
utxoEx2 =
  UTxO $
    Map.fromList
      [ (TxIn genesisId 0, alwaysSucceedsOutput),
        (TxIn genesisId 1, alwaysFailsOutput)
      ]

utxoStEx2 :: UTxOState A
utxoStEx2 = UTxOState utxoEx2 (Coin 0) (Coin 1000) def

-- =========================================================================
--  Example 3: Process a CERT transaction with a succeeding Plutus script.
-- =========================================================================

outEx3 :: TxOut A
outEx3 = TxOut feeAddr (Val.inject $ Coin 995) SNothing

redeemerExample3 :: Data A
redeemerExample3 = Data (Plutus.I 42)

validatingRedeemersEx3 :: Redeemers A
validatingRedeemersEx3 =
  Redeemers $
    Map.singleton (RdmrPtr Cert 0) (redeemerExample3, ExUnits 5000 5000)

scriptStakeCredSuceed :: StakeCredential C_Crypto
scriptStakeCredSuceed = ScriptHashObj $ hashScript @A $ alwaysSucceeds 2

validatingBodyWithCert :: TxBody A
validatingBodyWithCert =
  TxBody
    mempty --inputs
    (Set.singleton $ TxIn genesisId 2) --txinputs_fee
    (StrictSeq.singleton outEx3) --outputs
    (StrictSeq.fromList [DCertDeleg (DeRegKey scriptStakeCredSuceed)]) --txcerts
    (Wdrl mempty) --txwdrls
    (Coin 5) --txfee
    (ValidityInterval SNothing SNothing) --txvldt
    SNothing --txUpdates
    mempty -- reqSignerHashes
    mempty --mint
    (hashWitnessPPData pp (Set.singleton PlutusV1) validatingRedeemersEx3) --wppHash
    SNothing --adHash
    SNothing --network id

validatingTxWithCert :: ValidatedTx A
validatingTxWithCert =
  ValidatedTx
    validatingBodyWithCert
    TxWitness
      { txwitsVKey = Set.singleton $ makeWitnessVKey (hashAnnotated validatingBodyWithCert) feeKeys,
        txwitsBoot = mempty,
        txscripts =
          Map.singleton (hashScript @A $ alwaysSucceeds 2) (alwaysSucceeds 2),
        txdats = mempty,
        txrdmrs = validatingRedeemersEx3
      }
    (IsValidating True)
    SNothing

utxoEx3 :: UTxO A
utxoEx3 =
  UTxO $
    Map.fromList
      [ (TxIn genesisId 0, alwaysSucceedsOutput),
        (TxIn genesisId 1, alwaysFailsOutput),
        (TxIn (txid @A validatingBodyWithCert) 0, outEx3)
      ]

utxoStEx3 :: UTxOState A
utxoStEx3 = UTxOState utxoEx3 (Coin 0) (Coin 5) def

-- =====================================================================
--  Example 4: Process a CERT transaction with a failing Plutus script.
-- =====================================================================

outEx4 :: TxOut A
outEx4 = TxOut feeAddr (Val.inject $ Coin 995) SNothing

redeemerExample4 :: Data A
redeemerExample4 = Data (Plutus.I 0)

notValidatingRedeemersEx4 :: Redeemers A
notValidatingRedeemersEx4 =
  Redeemers $
    Map.singleton (RdmrPtr Cert 0) (redeemerExample4, ExUnits 5000 5000)

scriptStakeCredFail :: StakeCredential C_Crypto
scriptStakeCredFail = ScriptHashObj $ hashScript @A $ alwaysFails 1

notValidatingBodyWithCert :: TxBody A
notValidatingBodyWithCert =
  TxBody
    mempty --inputs
    (Set.singleton $ TxIn genesisId 2) --txinputs_fee
    (StrictSeq.singleton outEx4) --outputs
    (StrictSeq.fromList [DCertDeleg (DeRegKey scriptStakeCredFail)]) --txcerts
    (Wdrl mempty) --txwdrls
    (Coin 5) --txfee
    (ValidityInterval SNothing SNothing) --txvldt
    SNothing --txUpdates
    mempty -- reqSignerHashes
    mempty --mint
    (hashWitnessPPData pp (Set.singleton PlutusV1) notValidatingRedeemersEx4) --wppHash
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
              feeKeys,
        txwitsBoot = mempty,
        txscripts =
          Map.singleton (hashScript @A $ alwaysFails 1) (alwaysFails 1),
        txdats = mempty,
        txrdmrs = notValidatingRedeemersEx4
      }
    (IsValidating False)
    SNothing

utxoEx4 :: UTxO A
utxoEx4 =
  UTxO $
    Map.fromList
      [ (TxIn genesisId 0, alwaysSucceedsOutput),
        (TxIn genesisId 1, alwaysFailsOutput)
      ]

utxoStEx4 :: UTxOState A
utxoStEx4 = UTxOState utxoEx4 (Coin 0) (Coin 1000) def

-- ==============================================================================
--  Example 5: Process a WITHDRAWAL transaction with a succeeding Plutus script.
-- ==============================================================================

outEx5 :: TxOut A
outEx5 = TxOut feeAddr (Val.inject $ Coin 1995) SNothing

redeemerExample5 :: Data A
redeemerExample5 = Data (Plutus.I 42)

validatingRedeemersEx5 :: Redeemers A
validatingRedeemersEx5 =
  Redeemers $
    Map.singleton (RdmrPtr Rewrd 0) (redeemerExample5, ExUnits 5000 5000)

validatingBodyWithWithdrawal :: TxBody A
validatingBodyWithWithdrawal =
  TxBody
    mempty --inputs
    (Set.singleton $ TxIn genesisId 2) --txinputs_fee
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
    (hashWitnessPPData pp (Set.singleton PlutusV1) validatingRedeemersEx5) --wppHash
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
              feeKeys,
        txwitsBoot = mempty,
        txscripts =
          Map.singleton (hashScript @A $ alwaysSucceeds 2) (alwaysSucceeds 2),
        txdats = mempty,
        txrdmrs = validatingRedeemersEx5
      }
    (IsValidating True)
    SNothing

utxoEx5 :: UTxO A
utxoEx5 =
  UTxO $
    Map.fromList
      [ (TxIn genesisId 0, alwaysSucceedsOutput),
        (TxIn genesisId 1, alwaysFailsOutput),
        (TxIn (txid @A validatingBodyWithWithdrawal) 0, outEx5)
      ]

utxoStEx5 :: UTxOState A
utxoStEx5 = UTxOState utxoEx5 (Coin 0) (Coin 5) def

-- ===========================================================================
--  Example 6: Process a WITHDRAWAL transaction with a failing Plutus script.
-- ===========================================================================

outEx6 :: TxOut A
outEx6 = TxOut feeAddr (Val.inject $ Coin 1995) SNothing

redeemerExample6 :: Data A
redeemerExample6 = Data (Plutus.I 0)

notValidatingRedeemersEx6 :: Redeemers A
notValidatingRedeemersEx6 =
  Redeemers $
    Map.singleton (RdmrPtr Rewrd 0) (redeemerExample6, ExUnits 5000 5000)

notValidatingBodyWithWithdrawal :: TxBody A
notValidatingBodyWithWithdrawal =
  TxBody
    mempty --inputs
    (Set.singleton $ TxIn genesisId 2) --txinputs_fee
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
    (hashWitnessPPData pp (Set.singleton PlutusV1) notValidatingRedeemersEx6) --wppHash
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
              feeKeys,
        txwitsBoot = mempty,
        txscripts =
          Map.singleton (hashScript @A $ alwaysFails 1) (alwaysFails 1),
        txdats = mempty,
        txrdmrs = notValidatingRedeemersEx6
      }
    (IsValidating False)
    SNothing

utxoEx6 :: UTxO A
utxoEx6 =
  UTxO $
    Map.fromList
      [ (TxIn genesisId 0, alwaysSucceedsOutput),
        (TxIn genesisId 1, alwaysFailsOutput)
      ]

utxoStEx6 :: UTxOState A
utxoStEx6 = UTxOState utxoEx6 (Coin 0) (Coin 1000) def

-- ==============================================================================
--  Example 7: Process a MINT transaction with a succeeding Plutus script.
-- ==============================================================================

pidEx7 :: PolicyID C_Crypto
pidEx7 = PolicyID $ hashScript @A $ alwaysSucceeds 2

an :: AssetName
an = AssetName $ BS.pack "an"

mintEx7 :: Value C_Crypto
mintEx7 =
  Value 0 $
    Map.singleton pidEx7 (Map.singleton an 1)

outEx7 :: TxOut A
outEx7 = TxOut feeAddr (mintEx7 <+> (Val.inject $ Coin 995)) SNothing

redeemerExample7 :: Data A
redeemerExample7 = Data (Plutus.I 42)

validatingRedeemersEx7 :: Redeemers A
validatingRedeemersEx7 =
  Redeemers $
    Map.singleton (RdmrPtr Mint 0) (redeemerExample7, ExUnits 5000 5000)

validatingBodyWithMint :: TxBody A
validatingBodyWithMint =
  TxBody
    mempty --inputs
    (Set.singleton $ TxIn genesisId 2) --txinputs_fee
    (StrictSeq.singleton outEx7) --outputs
    StrictSeq.empty
    (Wdrl mempty)
    (Coin 5) --txfee
    (ValidityInterval SNothing SNothing) --txvldt
    SNothing --txUpdates
    mempty -- reqSignerHashes
    mintEx7 --mint
    (hashWitnessPPData pp (Set.singleton PlutusV1) validatingRedeemersEx7) --wppHash
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
              feeKeys,
        txwitsBoot = mempty,
        txscripts =
          Map.singleton (hashScript @A $ alwaysSucceeds 2) (alwaysSucceeds 2),
        txdats = mempty,
        txrdmrs = validatingRedeemersEx7
      }
    (IsValidating True)
    SNothing

utxoEx7 :: UTxO A
utxoEx7 =
  UTxO $
    Map.fromList
      [ (TxIn genesisId 0, alwaysSucceedsOutput),
        (TxIn genesisId 1, alwaysFailsOutput),
        (TxIn (txid @A validatingBodyWithMint) 0, outEx7)
      ]

utxoStEx7 :: UTxOState A
utxoStEx7 = UTxOState utxoEx7 (Coin 0) (Coin 5) def

-- ==============================================================================
--  Example 8: Process a MINT transaction with a failing Plutus script.
-- ==============================================================================

pidEx8 :: PolicyID C_Crypto
pidEx8 = PolicyID $ hashScript @A $ alwaysFails 1

mintEx8 :: Value C_Crypto
mintEx8 =
  Value 0 $
    Map.singleton pidEx8 (Map.singleton an 1)

outEx8 :: TxOut A
outEx8 = TxOut feeAddr (mintEx8 <+> (Val.inject $ Coin 995)) SNothing

redeemerExample8 :: Data A
redeemerExample8 = Data (Plutus.I 0)

notValidatingRedeemersEx8 :: Redeemers A
notValidatingRedeemersEx8 =
  Redeemers $
    Map.singleton (RdmrPtr Mint 0) (redeemerExample8, ExUnits 5000 5000)

notValidatingBodyWithMint :: TxBody A
notValidatingBodyWithMint =
  TxBody
    mempty --inputs
    (Set.singleton $ TxIn genesisId 2) --txinputs_fee
    (StrictSeq.singleton outEx8) --outputs
    StrictSeq.empty
    (Wdrl mempty)
    (Coin 5) --txfee
    (ValidityInterval SNothing SNothing) --txvldt
    SNothing --txUpdates
    mempty -- reqSignerHashes
    mintEx8 --mint
    (hashWitnessPPData pp (Set.singleton PlutusV1) notValidatingRedeemersEx8) --wppHash
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
              feeKeys,
        txwitsBoot = mempty,
        txscripts =
          Map.singleton (hashScript @A $ alwaysFails 1) (alwaysFails 1),
        txdats = mempty,
        txrdmrs = notValidatingRedeemersEx8
      }
    (IsValidating False)
    SNothing

utxoEx8 :: UTxO A
utxoEx8 =
  UTxO $
    Map.fromList
      [ (TxIn genesisId 0, alwaysSucceedsOutput),
        (TxIn genesisId 1, alwaysFailsOutput)
      ]

utxoStEx8 :: UTxOState A
utxoStEx8 = UTxOState utxoEx8 (Coin 0) (Coin 1000) def

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

utxowExamples :: TestTree
utxowExamples =
  testGroup
    "utxow examples"
    [ testCase "validating SPEND script" $
        testUTXOW
          initUtxoSt
          validatingTx
          (Right utxoStEx1),
      testCase "not validating SPEND script" $
        testUTXOW
          initUtxoSt
          notValidatingTx
          (Right utxoStEx2),
      testCase "validating CERT script" $
        testUTXOW
          initUtxoSt
          validatingTxWithCert
          (Right utxoStEx3),
      testCase "not validating CERT script" $
        testUTXOW
          initUtxoSt
          notValidatingTxWithCert
          (Right utxoStEx4),
      testCase "validating WITHDRAWAL script" $
        testUTXOW
          initUtxoSt
          validatingTxWithWithdrawal
          (Right utxoStEx5),
      testCase "not validating WITHDRAWAL script" $
        testUTXOW
          initUtxoSt
          notValidatingTxWithWithdrawal
          (Right utxoStEx6),
      testCase "validating MINT script" $
        testUTXOW
          initUtxoSt
          validatingTxWithMint
          (Right utxoStEx7),
      testCase "not validating MINT script" $
        testUTXOW
          initUtxoSt
          notValidatingTxWithMint
          (Right utxoStEx8)
    ]
