{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.ModelChain.Utils where

import Cardano.Ledger.Coin
import Data.Bifunctor
import Cardano.Slotting.Slot (EpochNo(..))
import Control.State.Transition.Extended
import Data.Default.Class
import Data.List ((\\), nub)
import Data.Time.Clock (secondsToNominalDiffTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Shelley.Spec.Ledger.API.Genesis
import Cardano.Ledger.BaseTypes (unitIntervalFromRational, unitIntervalToRational)
import Cardano.Ledger.BaseTypes (activeSlotVal)
import Cardano.Ledger.BaseTypes (Globals(..), epochInfo)
import Shelley.Spec.Ledger.Genesis
import Shelley.Spec.Ledger.LedgerState (NewEpochState)
import Shelley.Spec.Ledger.PParams (emptyPParams)
import qualified Shelley.Spec.Ledger.PParams as PParams
import Test.Cardano.Ledger.ModelChain
import Test.Shelley.Spec.Ledger.Utils (testGlobals)
import Test.Tasty.QuickCheck
import qualified Cardano.Ledger.Core as Core
import qualified Data.Map as Map
import qualified Data.Set as Set
import Shelley.Spec.Ledger.API.Mempool (ApplyTxError(..))
import Data.Functor.Identity
import Cardano.Slotting.EpochInfo.API (epochInfoSize)

type ApplyBlockError era = (ApplyBlockTransitionError era)

chainModelInteractionWith
  :: forall era proxy .
  ( Default (AdditionalGenesisConfig era)
  , Default (ElaborateEraModelState era)
  , ElaborateEraModel era
  )
  => proxy era
  -> Map.Map ModelAddress Coin
  -> [ModelChainInteraction]
  -> Either (ApplyBlockError era) (NewEpochState era, ElaborateEraModelState era)
chainModelInteractionWith _ genesisAccounts modelBlocks =
  let
    -- TODO, pass this in as a generator.

    globals = testGlobals

    sg :: ShelleyGenesis era
    sg = ShelleyGenesis
      { sgSystemStart       = (posixSecondsToUTCTime 0)
      , sgNetworkMagic      = 1 -- genNetworkMagic
      , sgNetworkId         = networkId globals
      , sgActiveSlotsCoeff  = unitIntervalToRational $ activeSlotVal $ activeSlotCoeff globals
      , sgSecurityParam     = securityParameter globals
      , sgEpochLength       = runIdentity $ flip epochInfoSize (EpochNo 1) $ epochInfo globals
      , sgSlotsPerKESPeriod = slotsPerKESPeriod globals
      , sgMaxKESEvolutions  = maxKESEvo globals
      , sgSlotLength        = (secondsToNominalDiffTime 1)
      , sgUpdateQuorum      = quorum globals
      , sgMaxLovelaceSupply = maxLovelaceSupply globals
      , sgProtocolParams    = emptyPParams
        { PParams._rho = unitIntervalFromRational 0.02
        } -- genPParams
      , sgGenDelegs         = mempty --  genGenesisDelegationList
      , sgInitialFunds      = mempty -- genFundsList
      , sgStaking           = emptyGenesisStaking -- genStaking
      }

    elabState = elaborateInitialState sg def genesisAccounts def
    (x, y) = elaborateBlocks_ globals modelBlocks elabState
  in bimap ApplyBlockTransitionError_Tx (\() -> y) x


testChainModelInteractionWith ::
  ( Testable prop
  , ElaborateEraModel era
  -- , ApplyBlock era
  , Default (AdditionalGenesisConfig era)
  , Show (PredicateFailure (Core.EraRule "LEDGER" era))
  , Default (ElaborateEraModelState era)
  )
  => proxy era
  -> (NewEpochState era -> ElaborateEraModelState era -> prop)
  -> Map.Map ModelAddress Coin
  -> [ModelChainInteraction]
  -> Property
testChainModelInteractionWith proxy p a b =
  case chainModelInteractionWith proxy a b of
    Right good -> property $! uncurry p good
    Left bad -> counterexample (show bad) False

compareLists :: forall a. (Show a, Eq a) => [a] -> [a] -> Property
compareLists a b = case nub a \\ nub b of
  [] -> property True
  _ -> a === b

testChainModelInteractionRejection
  :: forall era proxy.
  ( ElaborateEraModel era
  , Default (AdditionalGenesisConfig era)
  , Eq (PredicateFailure (Core.EraRule "LEDGER" era))
  , Show (PredicateFailure (Core.EraRule "LEDGER" era ))
  , Default (ElaborateEraModelState era)
  )
  => proxy era
  -> ModelPredicateFailure
  -> Map.Map ModelAddress Coin
  -> [ModelChainInteraction]
  -> Property
testChainModelInteractionRejection proxy e a b =
  case chainModelInteractionWith proxy a b of
    Left e' ->
      let
        elaboratedError = toEraPredicateFailure @era e
      in case (e', elaboratedError) of
        (ApplyBlockTransitionError_Tx (ApplyTxError te), ApplyBlockTransitionError_Tx (ApplyTxError te')) ->
          compareLists te te'
        -- fallthrough if/when more error types are added
        -- (te, te') -> te === te'

    Right _ -> counterexample "no error encountered" False

testChainModelInteraction ::
  ( Show (PredicateFailure (Core.EraRule "LEDGER" era))
  , ElaborateEraModel era
  -- , ApplyBlock era
  , Default (AdditionalGenesisConfig era)
  , Default (ElaborateEraModelState era)
  )
  => proxy era
  -> Map.Map ModelAddress Coin
  -> [ModelChainInteraction]
  -> Property
testChainModelInteraction proxy = testChainModelInteractionWith proxy $ (\x y -> x `seq` y `seq` True)


-- | helper to produce a "blank" ModelTx with most fields set to a reasonable
-- "default"
modelTx :: ModelTxId -> ModelTx
modelTx txId = ModelTx
  { _mtxId = txId
  , _mtxInputs = Set.empty
  , _mtxOutputs = []
  , _mtxFee = 0
  , _mtxDCert = []
  , _mtxWdrl = Map.empty
  }
