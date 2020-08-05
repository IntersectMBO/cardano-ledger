{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-binds #-}

module BenchValidation
  ( ValidateInput (..),
    validateInput,
    benchValidate,
    benchreValidate,
    sizes,
    runUpdate,
    updateChain,
    genUpdateInputs,
    profileUpdate,
  )
where

import Cardano.Prelude (NFData (rnf))
import Cardano.Slotting.Slot (withOriginToMaybe)
import Control.Monad.Except ()
import Control.State.Transition.Extended (IRC (..))
import qualified Data.Map as Map
import Shelley.Spec.Ledger.API.Protocol
  ( ChainDepState (..),
    ChainTransitionError,
    LedgerView (..),
    currentLedgerView,
    updateChainDepState,
  )
import Shelley.Spec.Ledger.API.Validation (ShelleyState, applyBlockTransition, reapplyBlockTransition)
import Shelley.Spec.Ledger.BaseTypes (Globals (..))
import Shelley.Spec.Ledger.BlockChain (BHeader (..), Block (..), LastAppliedBlock (..), slotToNonce)
import Shelley.Spec.Ledger.Crypto (Crypto)
import Shelley.Spec.Ledger.EpochBoundary (unBlocksMade)
import Shelley.Spec.Ledger.LedgerState (nesBcur)
import Shelley.Spec.Ledger.STS.Chain (ChainState (..))
import Shelley.Spec.Ledger.STS.Prtcl (PrtclState (..))
import Shelley.Spec.Ledger.STS.Tickn (TicknState (..))
import Test.QuickCheck.Gen (generate)
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes (C)
import Test.Shelley.Spec.Ledger.Generator.Block (genBlock)
import Test.Shelley.Spec.Ledger.Generator.Constants (Constants (..))
import Test.Shelley.Spec.Ledger.Generator.Presets (genEnv)
import Test.Shelley.Spec.Ledger.Generator.Trace.Chain (mkGenesisChainState)
import Test.Shelley.Spec.Ledger.Utils (testGlobals)

cs ::
  -- | Size of the genesis UTxO
  Int ->
  Constants
cs utxoSize =
  Constants
    { minNumGenInputs = 10,
      maxNumGenInputs = 25,
      frequencyRegKeyCert = 2,
      frequencyRegPoolCert = 2,
      frequencyDelegationCert = 3,
      frequencyGenesisDelegationCert = 1,
      frequencyDeRegKeyCert = 1,
      frequencyRetirePoolCert = 1,
      frequencyMIRCert = 1,
      frequencyScriptCredReg = 1,
      frequencyKeyCredReg = 2,
      frequencyScriptCredDeReg = 1,
      frequencyKeyCredDeReg = 2,
      frequencyScriptCredDelegation = 1,
      frequencyKeyCredDelegation = 2,
      frequencyTxUpdates = 10,
      frequencyTxWithMetaData = 10,
      minGenesisUTxOouts = utxoSize,
      maxGenesisUTxOouts = utxoSize,
      maxCertsPerTx = 3,
      maxTxsPerBlock = 10,
      maxNumKeyPairs = 150,
      minGenesisOutputVal = 5000000,
      maxGenesisOutputVal = 10000000,
      numBaseScripts = 3,
      frequencyNoWithdrawals = 75,
      frequencyAFewWithdrawals = 20,
      maxAFewWithdrawals = 10,
      frequencyPotentiallyManyWithdrawals = 5,
      minSlotTrace = 100,
      maxSlotTrace = 500,
      frequencyLowMaxEpoch = 6,
      maxMinFeeA = 10,
      maxMinFeeB = 3,
      numCoreNodes = 7,
      minTreasury = 1000000,
      maxTreasury = 10000000,
      minReserves = 1000000,
      maxReserves = 10000000,
      genTxRetries = 5
    }

data ValidateInput = ValidateInput Globals (ShelleyState C) (Block C)

sizes :: ValidateInput -> String
sizes (ValidateInput _gs ss _blk) = "blockMap size=" ++ show (Map.size (unBlocksMade (nesBcur ss)))

instance NFData ValidateInput where
  rnf (ValidateInput a b c) = seq a (seq b (seq c ()))

  block <- generate (genBlock (genEnv ([] :: [C])) chainstate)
validateInput :: Int -> IO ValidateInput
validateInput utxoSize = do
  Right chainstate <- generate (mkGenesisChainState (cs utxoSize) (IRC ()))
  pure (ValidateInput testGlobals (chainNes chainstate) block)

benchValidate :: ValidateInput -> IO (ShelleyState C)
benchValidate (ValidateInput globals state block) =
  case applyBlockTransition globals state block of
    Right x -> pure x
    Left x -> error (show x)

benchreValidate :: ValidateInput -> IO (ShelleyState C)
benchreValidate (ValidateInput globals state block) =
  pure $ reapplyBlockTransition globals state block

-- ==============================================================

data UpdateInputs = UpdateInputs !Globals !(LedgerView C) !(BHeader C) !(ChainDepState C)

instance Show UpdateInputs where
  show (UpdateInputs _globals vl bh st) = show vl ++ "\n" ++ show bh ++ "\n" ++ show st

instance NFData (LedgerView c) where
  rnf (LedgerView _pp _ov _pool _delegs) = ()

instance Crypto c => NFData (BHeader c) where
  rnf (BHeader _ _) = ()

instance NFData (ChainDepState c) where
  rnf (ChainDepState _ _ _) = ()

instance NFData Globals where
  rnf (Globals _ _ _ _ _ _ _ _ _ _ _) = ()

instance NFData UpdateInputs where
  rnf (UpdateInputs g lv bh st) = seq (rnf g) (seq (rnf lv) (seq (rnf bh) (rnf st)))

  Block blockheader _ <- generate (genBlock (genEnv ([] :: [C])) chainstate)
genUpdateInputs :: Int -> IO UpdateInputs
genUpdateInputs utxoSize = do
  Right chainstate <- generate (mkGenesisChainState (cs utxoSize) (IRC ()))
  let ledgerview = currentLedgerView (chainNes chainstate)
  let (ChainState newepochState keys eta0 etaV etaC etaH slot) = chainstate
  let prtclState = PrtclState keys eta0 etaV
  let ticknState = TicknState etaC etaH
  let nonce = case withOriginToMaybe slot of
        Just (LastAppliedBlock _blknum slotnum _hash) -> slotToNonce slotnum
        Nothing -> error "Empty Slot"
  pure (UpdateInputs testGlobals ledgerview blockheader (ChainDepState prtclState ticknState nonce))

updateChain :: UpdateInputs -> Either (ChainTransitionError C) (ChainDepState C)
updateChain (UpdateInputs gl lv bh st) = updateChainDepState gl lv bh st

runUpdate :: IO ()
runUpdate = do
  state <- genUpdateInputs
  case updateChain state of
    Left s -> error (show s)
    Right n -> putStrLn (show n)
  pure ()

profileUpdate :: IO ()
profileUpdate = do
  state <- genUpdateInputs
  let eithers = [updateChain state | _n <- ([1 .. 1000] :: [Int])]
  let size (Left _) = 0
      size (Right _) = 1 :: Int
  putStrLn (show (sum (map size eithers)))
  pure ()
