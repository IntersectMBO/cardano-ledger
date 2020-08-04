{-# OPTIONS_GHC -Wno-unused-binds #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}


module BenchValidation
   ( benchvalid,
     ValidateInput(..),
     validateInput,
     benchValidate,
   ) where

import Control.Monad.Except()
import Control.State.Transition.Extended(IRC(..))
import Test.QuickCheck.Gen(generate)
import Cardano.Prelude (NFData(rnf))

import Shelley.Spec.Ledger.API.Validation(applyBlockTransition,ShelleyState)
import Shelley.Spec.Ledger.BaseTypes(Globals(..))
import Shelley.Spec.Ledger.Crypto()
import Shelley.Spec.Ledger.BlockChain(Block(..))
import Shelley.Spec.Ledger.STS.Chain(ChainState(chainNes))

import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes(C)
import Test.Shelley.Spec.Ledger.Generator.Constants(Constants(..))
import Test.Shelley.Spec.Ledger.Generator.Trace.Chain(mkGenesisChainState)
import Test.Shelley.Spec.Ledger.Generator.Block(genBlock)
import Test.Shelley.Spec.Ledger.Generator.Presets(genEnv)
import Test.Shelley.Spec.Ledger.Utils(testGlobals)

cs :: Constants
cs = Constants
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
      minGenesisUTxOouts = 10000,
      maxGenesisUTxOouts = 100000,
      maxCertsPerTx = 3,
      maxTxsPerBlock = 10,
      maxNumKeyPairs = 150,
      minGenesisOutputVal = 10000,
      maxGenesisOutputVal = 100000,
      numBaseScripts = 3,
      frequencyNoWithdrawals = 75,
      frequencyAFewWithdrawals = 20,
      maxAFewWithdrawals = 10,
      frequencyPotentiallyManyWithdrawals = 5,
      minSlotTrace = 100,
      maxSlotTrace = 500,
      frequencyLowMaxEpoch = 6,
      maxMinFeeA = 1000,
      maxMinFeeB = 3,
      numCoreNodes = 7,
      minTreasury = 1000000,
      maxTreasury = 10000000,
      minReserves = 1000000,
      maxReserves = 10000000,
      genTxRetries = 5
    }

benchvalid :: IO (ShelleyState C)
benchvalid = do
  input <- validateInput
  benchValidate input

newtype ValidateInput = ValidateInput (Globals,ShelleyState C,Block C)


instance NFData ValidateInput where
   rnf (ValidateInput(a,b,c)) = seq a (seq b (seq c ()))

validateInput :: IO ValidateInput
validateInput = do
  Right chainstate <- generate (mkGenesisChainState cs (IRC ()))
  block <- generate (genBlock (genEnv ([]::[C])) chainstate)
  pure(ValidateInput (testGlobals,chainNes chainstate, block))

benchValidate:: ValidateInput -> IO (ShelleyState C)
benchValidate (ValidateInput (globals,state,block)) =
   case applyBlockTransition globals state block of
     Right x -> pure x
     Left x -> error (show x)
