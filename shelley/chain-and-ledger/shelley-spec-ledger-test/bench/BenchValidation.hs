{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-binds #-}

module BenchValidation
  ( ValidateInput (..),
    validateInput,
    benchValidate,
    benchreValidate,
    applyBlock,
    sizes,
    updateChain,
    updateAndTickChain,
    genUpdateInputs,
  )
where

import Cardano.Crypto.DSIGN
import Cardano.Crypto.Hash
import Cardano.Crypto.KES
import Cardano.Crypto.VRF.Praos
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
    tickChainDepState,
    updateChainDepState,
  )
import Shelley.Spec.Ledger.API.Validation
  ( ShelleyState,
    applyBlockTransition,
    reapplyBlockTransition,
  )
import Shelley.Spec.Ledger.BaseTypes (Globals (..))
import Shelley.Spec.Ledger.BlockChain
  ( BHeader (..),
    Block (..),
    LastAppliedBlock (..),
    slotToNonce,
  )
import Shelley.Spec.Ledger.Crypto
import Shelley.Spec.Ledger.EpochBoundary (unBlocksMade)
import Shelley.Spec.Ledger.LedgerState (nesBcur)
import Shelley.Spec.Ledger.STS.Chain (ChainState (..))
import Shelley.Spec.Ledger.STS.Prtcl (PrtclState (..))
import Shelley.Spec.Ledger.STS.Tickn (TicknState (..))
import Test.QuickCheck.Gen (generate)
import Test.Shelley.Spec.Ledger.Generator.Block (genBlock)
import Test.Shelley.Spec.Ledger.Generator.Constants (Constants (..))
import Test.Shelley.Spec.Ledger.Generator.Presets (genEnv)
import Test.Shelley.Spec.Ledger.Generator.Trace.Chain (mkGenesisChainState)
import Test.Shelley.Spec.Ledger.Utils (testGlobals)

data BenchCrypto

instance Crypto BenchCrypto where
  type DSIGN BenchCrypto = Ed25519DSIGN
  type KES BenchCrypto = Sum6KES Ed25519DSIGN Blake2b_256
  type VRF BenchCrypto = PraosVRF
  type HASH BenchCrypto = Blake2b_256
  type ADDRHASH BenchCrypto = Blake2b_224

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
      maxMinFeeA = 0,
      maxMinFeeB = 3,
      numCoreNodes = 7,
      minTreasury = 1000000,
      maxTreasury = 10000000,
      minReserves = 1000000,
      maxReserves = 10000000,
      genTxRetries = 5
    }

data ValidateInput = ValidateInput Globals (ShelleyState BenchCrypto) (Block BenchCrypto)

sizes :: ValidateInput -> String
sizes (ValidateInput _gs ss _blk) = "blockMap size=" ++ show (Map.size (unBlocksMade (nesBcur ss)))

instance NFData ValidateInput where
  rnf (ValidateInput a b c) = seq a (seq b (seq c ()))

validateInput :: Int -> IO ValidateInput
validateInput utxoSize = do
  Right chainstate <- generate (mkGenesisChainState (cs utxoSize) (IRC ()))
  block <- generate (genBlock (genEnv ([] :: [BenchCrypto])) chainstate)
  pure (ValidateInput testGlobals (chainNes chainstate) block)

benchValidate :: ValidateInput -> IO (ShelleyState BenchCrypto)
benchValidate (ValidateInput globals state block) =
  case applyBlockTransition globals state block of
    Right x -> pure x
    Left x -> error (show x)

applyBlock :: ValidateInput -> Int -> Int
applyBlock (ValidateInput globals state block) n =
  case applyBlockTransition globals state block of
    Right x -> seq (rnf x) (n+1)
    Left x -> error (show x)

benchreValidate :: ValidateInput -> ShelleyState BenchCrypto
benchreValidate (ValidateInput globals state block) =
  reapplyBlockTransition globals state block

-- ==============================================================

data UpdateInputs = UpdateInputs !Globals !(LedgerView BenchCrypto) !(BHeader BenchCrypto) !(ChainDepState BenchCrypto)

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

instance NFData (ChainTransitionError c) where
  rnf _ = ()

instance NFData UpdateInputs where
  rnf (UpdateInputs g lv bh st) = seq (rnf g) (seq (rnf lv) (seq (rnf bh) (rnf st)))

genUpdateInputs :: Int -> IO UpdateInputs
genUpdateInputs utxoSize = do
  Right chainstate <- generate (mkGenesisChainState (cs utxoSize) (IRC ()))
  Block blockheader _ <- generate (genBlock (genEnv ([] :: [BenchCrypto])) chainstate)
  let ledgerview = currentLedgerView (chainNes chainstate)
  let (ChainState newepochState keys eta0 etaV etaC etaH slot) = chainstate
  let prtclState = PrtclState keys eta0 etaV
  let ticknState = TicknState etaC etaH
  let nonce = case withOriginToMaybe slot of
        Just (LastAppliedBlock _blknum slotnum _hash) -> slotToNonce slotnum
        Nothing -> error "Empty Slot"
  pure (UpdateInputs testGlobals ledgerview blockheader (ChainDepState prtclState ticknState nonce))

updateChain ::
  UpdateInputs ->
  Either (ChainTransitionError BenchCrypto) (ChainDepState BenchCrypto)
updateChain (UpdateInputs gl lv bh st) = updateChainDepState gl lv bh st

updateAndTickChain ::
  UpdateInputs ->
  Either (ChainTransitionError BenchCrypto) (ChainDepState BenchCrypto)
updateAndTickChain (UpdateInputs gl lv bh st) =
  updateChainDepState gl lv bh
    . tickChainDepState gl lv True
    $ st
