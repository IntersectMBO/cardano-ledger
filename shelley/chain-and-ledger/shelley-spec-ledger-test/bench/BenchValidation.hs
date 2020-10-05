{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
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

import Cardano.Ledger.Era (Era (..))
import Cardano.Prelude (NFData (rnf))
import Cardano.Slotting.Slot (withOriginToMaybe)
import Control.Monad.Except ()
import qualified Data.Map as Map
import Data.Proxy
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
import Shelley.Spec.Ledger.Bench.Gen (genBlock, genChainState)
import Shelley.Spec.Ledger.BlockChain
  ( BHeader (..),
    Block (..),
    LastAppliedBlock (..),
    slotToNonce,
  )
import Shelley.Spec.Ledger.EpochBoundary (unBlocksMade)
import Shelley.Spec.Ledger.LedgerState (nesBcur)
import Shelley.Spec.Ledger.STS.Chain (ChainState (..))
import Shelley.Spec.Ledger.STS.Prtcl (PrtclState (..))
import Shelley.Spec.Ledger.STS.Tickn (TicknState (..))
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes (Mock)
import Test.Shelley.Spec.Ledger.Generator.Presets (genEnv)
import Test.Shelley.Spec.Ledger.Serialisation.Generators ()
import Test.Shelley.Spec.Ledger.Utils (ShelleyTest, testGlobals)

-- ====================================================================

data ValidateInput era = ValidateInput Globals (ShelleyState era) (Block era)

sizes :: ValidateInput era -> String
sizes (ValidateInput _gs ss _blk) = "blockMap size=" ++ show (Map.size (unBlocksMade (nesBcur ss)))

instance NFData (ValidateInput era) where
  rnf (ValidateInput a b c) = seq a (seq b (seq c ()))

validateInput :: (ShelleyTest era, Mock (Crypto era)) => Int -> IO (ValidateInput era)
validateInput utxoSize = genValidateInput utxoSize

genValidateInput :: (ShelleyTest era, Mock (Crypto era)) => Int -> IO (ValidateInput era)
genValidateInput n = do
  let ge = genEnv (Proxy :: Proxy era)
  chainstate <- genChainState n ge
  block <- genBlock ge chainstate
  pure (ValidateInput testGlobals (chainNes chainstate) block)

benchValidate :: forall era. (ShelleyTest era, Mock (Crypto era)) => ValidateInput era -> IO (ShelleyState era)
benchValidate (ValidateInput globals state block) =
  case applyBlockTransition @era globals state block of
    Right x -> pure x
    Left x -> error (show x)

applyBlock :: forall era. (ShelleyTest era, Mock (Crypto era)) => ValidateInput era -> Int -> Int
applyBlock (ValidateInput globals state block) n =
  case applyBlockTransition @era globals state block of
    Right x -> seq (rnf x) (n + 1)
    Left x -> error (show x)

benchreValidate :: (ShelleyTest era, Mock (Crypto era)) => ValidateInput era -> ShelleyState era
benchreValidate (ValidateInput globals state block) =
  reapplyBlockTransition globals state block

-- ==============================================================

data UpdateInputs era
  = UpdateInputs
      !Globals
      !(LedgerView era)
      !(BHeader era)
      !(ChainDepState era)

instance Era era => Show (UpdateInputs era) where
  show (UpdateInputs _globals vl bh st) = show vl ++ "\n" ++ show bh ++ "\n" ++ show st

instance NFData (LedgerView era) where
  rnf (LedgerView _pp _pool _delegs) = ()

instance Era era => NFData (BHeader era) where
  rnf (BHeader _ _) = ()

instance NFData (ChainDepState c) where
  rnf (ChainDepState _ _ _) = ()

instance NFData Globals where
  rnf (Globals _ _ _ _ _ _ _ _ _ _ _) = ()

instance NFData (ChainTransitionError c) where
  rnf _ = ()

instance Era era => NFData (UpdateInputs era) where
  rnf (UpdateInputs g lv bh st) = seq (rnf g) (seq (rnf lv) (seq (rnf bh) (rnf st)))

genUpdateInputs :: forall era. (ShelleyTest era, Mock (Crypto era)) => Int -> IO (UpdateInputs era)
genUpdateInputs utxoSize = do
  let ge = genEnv (Proxy :: Proxy era)
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
  (Era era, Mock (Crypto era)) =>
  UpdateInputs era ->
  Either (ChainTransitionError era) (ChainDepState era)
updateChain (UpdateInputs gl lv bh st) = updateChainDepState gl lv bh st

updateAndTickChain ::
  (Era era, Mock (Crypto era)) =>
  UpdateInputs era ->
  Either (ChainTransitionError era) (ChainDepState era)
updateAndTickChain (UpdateInputs gl lv bh st) =
  updateChainDepState gl lv bh
    . tickChainDepState gl lv True
    $ st
