{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
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
import Cardano.Ledger.Crypto
import qualified Cardano.Ledger.Shelley.API as API
import Cardano.Ledger.Shelley.Bench.Gen (genBlock, genChainState)
import Cardano.Ledger.Shelley.BlockChain (slotToNonce)
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.LedgerState (
  NewEpochState,
  StashedAVVMAddresses,
  nesBcur,
 )
import Cardano.Protocol.HeaderCrypto
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
import Control.DeepSeq (NFData (rnf))
import Control.Monad.Except ()
import qualified Control.State.Transition.Trace.Generator.QuickCheck as QC
import qualified Data.Map.Strict as Map
import Data.Proxy
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes (Mock)
import Test.Cardano.Ledger.Shelley.Constants (defaultConstants)
import Test.Cardano.Ledger.Shelley.Generator.Core (GenEnv)
import Test.Cardano.Ledger.Shelley.Generator.EraGen (EraGen, MinLEDGER_STS)
import Test.Cardano.Ledger.Shelley.Generator.Presets (genEnv)
import Test.Cardano.Ledger.Shelley.Rules.Chain (ChainState (..))
import Test.Cardano.Ledger.Shelley.Serialisation.Generators ()
import Test.Cardano.Ledger.Shelley.Utils (testGlobals)

data ValidateInput era hc = ValidateInput Globals (NewEpochState era) (Block (BHeader (EraCrypto era) hc) era)

sizes :: ValidateInput era hc -> String
sizes (ValidateInput _gs ss _blk) = "blockMap size=" ++ show (Map.size (unBlocksMade (nesBcur ss)))

instance NFData (ValidateInput era hc) where
  rnf (ValidateInput a b c) = seq a (seq b (seq c ()))

validateInput ::
  forall era hc.
  ( EraGen era
  , Mock (EraCrypto era) hc
  , EraRule "LEDGERS" era ~ API.ShelleyLEDGERS era
  , QC.HasTrace (API.ShelleyLEDGERS era) (GenEnv era hc)
  , API.ApplyBlock era
  , GetLedgerView era
  , MinLEDGER_STS era
  , EraGovernance era
  ) =>
  Int ->
  IO (ValidateInput era hc)
validateInput utxoSize = genValidateInput utxoSize

genValidateInput ::
  ( EraGen era
  , Mock (EraCrypto era) hc
  , EraRule "LEDGERS" era ~ API.ShelleyLEDGERS era
  , QC.HasTrace (API.ShelleyLEDGERS era) (GenEnv era hc)
  , API.ApplyBlock era
  , GetLedgerView era
  , MinLEDGER_STS era
  , EraGovernance era
  ) =>
  Int ->
  IO (ValidateInput era hc)
genValidateInput n = do
  let ge = genEnv (Proxy :: Proxy era) (Proxy :: Proxy hc) defaultConstants
  chainstate <- genChainState n ge
  block <- genBlock ge chainstate
  pure (ValidateInput testGlobals (chainNes chainstate) block)

benchValidate ::
  forall era hc.
  ( Era era
  , HeaderCrypto hc
  , API.ApplyBlock era
  ) =>
  ValidateInput era hc ->
  IO (NewEpochState era)
benchValidate (ValidateInput globals state (Block bh txs)) =
  case API.applyBlock @era globals state (UnsafeUnserialisedBlock (makeHeaderView bh) txs) of
    Right x -> pure x
    Left x -> error (show x)

applyBlock ::
  forall era hc.
  ( EraTxOut era
  , HeaderCrypto hc
  , API.ApplyBlock era
  , NFData (StashedAVVMAddresses era)
  , GovernanceState era ~ ShelleyPPUPState era
  ) =>
  ValidateInput era hc ->
  Int ->
  Int
applyBlock (ValidateInput globals state (Block bh txs)) n =
  case API.applyBlock @era globals state (UnsafeUnserialisedBlock (makeHeaderView bh) txs) of
    Right x -> seq (rnf x) (n + 1)
    Left x -> error (show x)

benchreValidate ::
  ( Era era
  , HeaderCrypto hc
  , API.ApplyBlock era
  ) =>
  ValidateInput era hc ->
  NewEpochState era
benchreValidate (ValidateInput globals state (Block bh txs)) =
  API.reapplyBlock globals state (UnsafeUnserialisedBlock (makeHeaderView bh) txs)

-- ==============================================================

data UpdateInputs c hc
  = UpdateInputs
      !Globals
      !(LedgerView c)
      !(BHeader c hc)
      !(ChainDepState c)

instance (Crypto c, HeaderCrypto hc) => Show (UpdateInputs c hc) where
  show (UpdateInputs _globals vl bh st) =
    show vl ++ "\n" ++ show bh ++ "\n" ++ show st

instance NFData (LedgerView era) where
  rnf (LedgerView _D _extraEntropy _pool _delegs _ccd) = ()

instance (Crypto c, HeaderCrypto hc) => NFData (BHeader c hc) where
  rnf (BHeader _ _) = ()

instance NFData (ChainDepState c) where
  rnf (ChainDepState _ _ _) = ()

instance NFData (ChainTransitionError c hc) where
  rnf _ = ()

instance (Crypto c, HeaderCrypto hc) => NFData (UpdateInputs c hc) where
  rnf (UpdateInputs g lv bh st) =
    seq (rnf g) (seq (rnf lv) (seq (rnf bh) (rnf st)))

genUpdateInputs ::
  forall era hc.
  ( EraGen era
  , Mock (EraCrypto era) hc
  , MinLEDGER_STS era
  , GetLedgerView era
  , EraRule "LEDGERS" era ~ API.ShelleyLEDGERS era
  , QC.HasTrace (API.ShelleyLEDGERS era) (GenEnv era hc)
  , API.ApplyBlock era
  , EraGovernance era
  ) =>
  Int ->
  IO (UpdateInputs (EraCrypto era) hc)
genUpdateInputs utxoSize = do
  let ge = genEnv (Proxy :: Proxy era) (Proxy :: Proxy hc) defaultConstants
  chainstate <- genChainState utxoSize ge
  (Block blockheader _) <- genBlock ge chainstate
  let ledgerview = currentLedgerView (chainNes chainstate)
  let (ChainState _newepochState keys eta0 etaV etaC etaH slot) = chainstate
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
  (Mock c hc) =>
  UpdateInputs c hc ->
  Either (ChainTransitionError c hc) (ChainDepState c)
updateChain (UpdateInputs gl lv bh st) = updateChainDepState gl lv bh st

updateAndTickChain ::
  (Mock c hc) =>
  UpdateInputs c hc ->
  Either (ChainTransitionError c hc) (ChainDepState c)
updateAndTickChain (UpdateInputs gl lv bh st) =
  updateChainDepState gl lv bh
    . tickChainDepState gl lv True
    $ st
