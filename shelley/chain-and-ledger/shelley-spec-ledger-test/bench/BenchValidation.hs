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
    BenchCrypto,
  )
where

import Data.Proxy
import Cardano.Crypto.DSIGN
import Cardano.Crypto.Hash
import Cardano.Crypto.KES
import Cardano.Crypto.VRF.Praos
import Cardano.Prelude (NFData (rnf))
import Cardano.Slotting.Slot (withOriginToMaybe)
import Control.Monad.Except ()
import qualified Data.Map as Map
import Shelley.Spec.Ledger.Coin(Coin)
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
import Test.Shelley.Spec.Ledger.Generator.Presets (genEnv)
import Test.Shelley.Spec.Ledger.Utils (testGlobals)
import Test.Shelley.Spec.Ledger.Serialisation.Generators()  -- Arbitrary Coin
import Shelley.Spec.Ledger.Bench.Gen(genBlock,genChainState)

data BenchCrypto

instance Crypto BenchCrypto where
  type DSIGN BenchCrypto = Ed25519DSIGN
  type KES BenchCrypto = Sum6KES Ed25519DSIGN Blake2b_256
  type VRF BenchCrypto = PraosVRF
  type HASH BenchCrypto = Blake2b_256
  type ADDRHASH BenchCrypto = Blake2b_224

-- ====================================================================

data ValidateInput = ValidateInput Globals (ShelleyState BenchCrypto Coin) (Block BenchCrypto Coin)

sizes :: ValidateInput -> String
sizes (ValidateInput _gs ss _blk) = "blockMap size=" ++ show (Map.size (unBlocksMade (nesBcur ss)))

instance NFData ValidateInput where
  rnf (ValidateInput a b c) = seq a (seq b (seq c ()))

validateInput :: Int -> IO ValidateInput
validateInput utxoSize = genValidateInput utxoSize

genValidateInput:: Int -> IO ValidateInput
genValidateInput n = do
  let ge = genEnv (Proxy :: Proxy BenchCrypto)
  chainstate <- genChainState n ge
  block <- genBlock ge chainstate
  pure (ValidateInput testGlobals (chainNes chainstate) block)



benchValidate :: ValidateInput -> IO (ShelleyState BenchCrypto Coin)
benchValidate (ValidateInput globals state block) =
  case applyBlockTransition globals state block of
    Right x -> pure x
    Left x -> error (show x)

applyBlock :: ValidateInput -> Int -> Int
applyBlock (ValidateInput globals state block) n =
  case applyBlockTransition globals state block of
    Right x -> seq (rnf x) (n+1)
    Left x -> error (show x)

benchreValidate :: ValidateInput -> ShelleyState BenchCrypto Coin
benchreValidate (ValidateInput globals state block) =
  reapplyBlockTransition globals state block

-- ==============================================================

data UpdateInputs = UpdateInputs !Globals !(LedgerView BenchCrypto) !(BHeader BenchCrypto Coin) !(ChainDepState BenchCrypto)

instance Show UpdateInputs where
  show (UpdateInputs _globals vl bh st) = show vl ++ "\n" ++ show bh ++ "\n" ++ show st

instance NFData (LedgerView c) where
  rnf (LedgerView _pp _ov _pool _delegs) = ()

instance Crypto c => NFData (BHeader c Coin) where
  rnf (BHeader _ _) = ()

instance NFData (ChainDepState c) where
  rnf (ChainDepState _ _ _) = ()

instance NFData Globals where
  rnf (Globals _ _ _ _ _ _ _ _ _ _ _) = ()

instance NFData (ChainTransitionError c Coin) where
  rnf _ = ()

instance NFData UpdateInputs where
  rnf (UpdateInputs g lv bh st) = seq (rnf g) (seq (rnf lv) (seq (rnf bh) (rnf st)))

genUpdateInputs :: Int -> IO UpdateInputs
genUpdateInputs utxoSize = do
  let ge = genEnv (Proxy :: Proxy BenchCrypto)
  chainstate <- genChainState utxoSize ge
  (Block blockheader _) <- genBlock ge chainstate
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
  Either (ChainTransitionError BenchCrypto Coin) (ChainDepState BenchCrypto)
updateChain (UpdateInputs gl lv bh st) = updateChainDepState gl lv bh st

updateAndTickChain ::
  UpdateInputs ->
  Either (ChainTransitionError BenchCrypto Coin) (ChainDepState BenchCrypto)
updateAndTickChain (UpdateInputs gl lv bh st) =
  updateChainDepState gl lv bh
    . tickChainDepState gl lv True
    $ st
