{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-binds #-}

module BenchValidation (
  ValidateInput (..),
  validateInput,
  benchValidate,
  benchreValidate,
  applyBlock,
  sizes,
  updateChain,
  updateAndTickChain,
  genUpdateInputs,
) where

import Cardano.Ledger.BaseTypes (Globals (..), unBlocksMade)
import Cardano.Ledger.Block (Block (..))
import Cardano.Ledger.CertState (EraCertState)
import qualified Cardano.Ledger.Shelley.API as API
import Cardano.Ledger.Shelley.Bench.Gen (genBlock, genChainState)
import Cardano.Ledger.Shelley.BlockChain (slotToNonce)
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.LedgerState (
  NewEpochState,
  StashedAVVMAddresses,
  nesBcur,
 )
import Cardano.Ledger.Shelley.State
import Cardano.Protocol.Crypto
import Cardano.Protocol.TPraos.API (
  ChainDepState (..),
  ChainTransitionError,
  GetLedgerView,
  LedgerView (..),
  currentLedgerView,
  tickChainDepState,
  updateChainDepState,
 )
import Cardano.Protocol.TPraos.BHeader (BHeader (..), LastAppliedBlock (..), makeHeaderView)
import Cardano.Protocol.TPraos.Rules.Prtcl (PrtclState (..))
import Cardano.Protocol.TPraos.Rules.Tickn (TicknState (..))
import Cardano.Slotting.Slot (withOriginToMaybe)
import Control.DeepSeq (NFData (rnf), deepseq)
import Control.Monad.Except ()
import Control.State.Transition
import qualified Data.Map.Strict as Map
import Data.Proxy
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes (MockCrypto)
import Test.Cardano.Ledger.Shelley.Constants (defaultConstants)
import Test.Cardano.Ledger.Shelley.Generator.Core (GenEnv)
import Test.Cardano.Ledger.Shelley.Generator.EraGen (EraGen, MinLEDGER_STS)
import Test.Cardano.Ledger.Shelley.Generator.Presets (genEnv)
import Test.Cardano.Ledger.Shelley.Rules.Chain (ChainState (..))
import Test.Cardano.Ledger.Shelley.Serialisation.Generators ()
import Test.Cardano.Ledger.Shelley.Utils (testGlobals)
import qualified Test.Control.State.Transition.Trace.Generator.QuickCheck as QC

data ValidateInput era = ValidateInput Globals (NewEpochState era) (Block (BHeader MockCrypto) era)

sizes :: ValidateInput era -> String
sizes (ValidateInput _gs ss _blk) = "blockMap size=" ++ show (Map.size (unBlocksMade (nesBcur ss)))

instance (EraTxOut era, NFData (NewEpochState era)) => NFData (ValidateInput era) where
  rnf (ValidateInput a b c) = a `deepseq` b `deepseq` rnf c

validateInput ::
  ( EraGen era
  , EraStake era
  , EraRule "LEDGERS" era ~ API.ShelleyLEDGERS era
  , QC.HasTrace (API.ShelleyLEDGERS era) (GenEnv MockCrypto era)
  , API.ApplyBlock era
  , GetLedgerView era
  , MinLEDGER_STS era
  ) =>
  Int ->
  IO (ValidateInput era)
validateInput n = do
  let ge = genEnv (Proxy :: Proxy era) defaultConstants
  chainstate <- genChainState n ge
  block <- genBlock ge chainstate
  pure (ValidateInput testGlobals (chainNes chainstate) block)

benchValidate ::
  (API.ApplyBlock era, Show (PredicateFailure (EraRule "BBODY" era))) =>
  ValidateInput era ->
  IO (NewEpochState era)
benchValidate (ValidateInput globals state (Block bh txs)) =
  let block = UnsafeUnserialisedBlock (makeHeaderView bh) txs
   in case API.applyBlockEitherNoEvents ValidateAll globals state block of
        Right x -> pure x
        Left x -> error (show x)

applyBlock ::
  forall era.
  ( API.ApplyBlock era
  , NFData (StashedAVVMAddresses era)
  , NFData (InstantStake era)
  , GovState era ~ ShelleyGovState era
  , EraCertState era
  , Show (PredicateFailure (EraRule "BBODY" era))
  ) =>
  ValidateInput era ->
  Int ->
  Int
applyBlock (ValidateInput globals state (Block bh txs)) n =
  let block = UnsafeUnserialisedBlock (makeHeaderView bh) txs
   in case API.applyBlockEitherNoEvents ValidateAll globals state block of
        Right x -> x `deepseq` n + 1
        Left x -> error (show x)

benchreValidate ::
  API.ApplyBlock era =>
  ValidateInput era ->
  NewEpochState era
benchreValidate (ValidateInput globals state (Block bh txs)) =
  API.applyBlockNoValidaton globals state (UnsafeUnserialisedBlock (makeHeaderView bh) txs)

-- ==============================================================

data UpdateInputs
  = UpdateInputs
      !Globals
      !LedgerView
      !(BHeader MockCrypto)
      !ChainDepState

instance Show UpdateInputs where
  show (UpdateInputs _globals vl bh st) =
    show vl ++ "\n" ++ show bh ++ "\n" ++ show st

instance NFData LedgerView where
  rnf (LedgerView _D _extraEntropy _pool _delegs _ccd) = ()

-- TODO: move upstream
instance Crypto c => NFData (BHeader c) where
  rnf (BHeader _ _) = ()

instance NFData ChainDepState where
  rnf (ChainDepState _ _ _) = ()

instance NFData (ChainTransitionError c) where
  rnf _ = ()

instance NFData UpdateInputs where
  rnf (UpdateInputs g lv bh st) =
    g `deepseq` lv `deepseq` bh `deepseq` rnf st

genUpdateInputs ::
  forall era.
  ( EraGen era
  , EraStake era
  , MinLEDGER_STS era
  , GetLedgerView era
  , EraRule "LEDGERS" era ~ API.ShelleyLEDGERS era
  , QC.HasTrace (API.ShelleyLEDGERS era) (GenEnv MockCrypto era)
  , API.ApplyBlock era
  ) =>
  Int ->
  IO UpdateInputs
genUpdateInputs utxoSize = do
  let ge = genEnv (Proxy :: Proxy era) defaultConstants
  chainstate <- genChainState utxoSize ge
  Block blockheader _ <- genBlock ge chainstate
  let ledgerview = currentLedgerView (chainNes chainstate)
  let ChainState _newepochState keys eta0 etaV etaC etaH slot = chainstate
  let prtclState = PrtclState keys eta0 etaV
  let ticknState = TicknState etaC etaH
  let nonce = case withOriginToMaybe slot of
        Just (LastAppliedBlock _blknum slotnum _hash) -> slotToNonce slotnum
        Nothing -> error "Empty Slot"
  pure
    ( UpdateInputs
        testGlobals
        ledgerview
        blockheader
        (ChainDepState prtclState ticknState nonce)
    )

updateChain ::
  UpdateInputs ->
  Either (ChainTransitionError MockCrypto) ChainDepState
updateChain (UpdateInputs gl lv bh st) = updateChainDepState gl lv bh st

updateAndTickChain ::
  UpdateInputs ->
  Either (ChainTransitionError MockCrypto) ChainDepState
updateAndTickChain (UpdateInputs gl lv bh st) =
  updateChainDepState gl lv bh
    . tickChainDepState gl lv True
    $ st
