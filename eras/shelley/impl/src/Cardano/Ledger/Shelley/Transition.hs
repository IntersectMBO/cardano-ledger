{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- | Besides capturing all configuration that is necessary to progress to a specific era,
-- this interface also provides fast forward functionality that is used in testing and
-- benchmarking in order to initilize a chain in a particular era without going through
-- the trouble of generating all the history for preceeding eras.
module Cardano.Ledger.Shelley.Transition (
  EraTransition (..),
  tcInitialFundsL,
  tcInitialStakingL,
  mkShelleyTransitionConfig,
  createInitialState,
  registerInitialFundsThenStaking,
  toShelleyTransitionConfigPairs,
  protectMainnet,
  protectMainnetLens,
) where

import Cardano.Ledger.Address
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.CertState
import Cardano.Ledger.Coin
import Cardano.Ledger.Core
import Cardano.Ledger.Credential
import Cardano.Ledger.Crypto
import Cardano.Ledger.EpochBoundary
import Cardano.Ledger.Genesis (EraGenesis)
import Cardano.Ledger.Keys
import Cardano.Ledger.PoolDistr
import Cardano.Ledger.Shelley.Era
import Cardano.Ledger.Shelley.Genesis
import Cardano.Ledger.Shelley.Governance
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Shelley.Translation (
  FromByronTranslationContext (..),
  toFromByronTranslationContext,
 )
import qualified Cardano.Ledger.UMap as UM
import Cardano.Ledger.UTxO
import Cardano.Ledger.Val
import Data.Aeson (FromJSON (..), KeyValue (..), ToJSON (..), object, pairs, withObject, (.:))
import Data.Default.Class
import Data.Kind
import qualified Data.ListMap as LM
import qualified Data.ListMap as ListMap
import qualified Data.Map.Strict as Map
import Data.Void (Void)
import GHC.Generics (Generic)
import GHC.Stack
import Lens.Micro
import NoThunks.Class (NoThunks (..))

class
  ( EraTxOut era
  , EraGov era
  , EraGenesis era
  , ToJSON (TransitionConfig era)
  , FromJSON (TransitionConfig era)
  , Default (StashedAVVMAddresses era)
  ) =>
  EraTransition era
  where
  -- | Cumulative configuration that is needed to be able to start in a current era
  data TransitionConfig era :: Type

  mkTransitionConfig ::
    -- | Translation context necessary for advancing from previous era into the current
    -- one. This will usually be the contents of genesis file, if one exists for the
    -- current era
    TranslationContext era ->
    -- | Transition configuration for the previous era.
    TransitionConfig (PreviousEra era) ->
    TransitionConfig era

  injectIntoTestState ::
    -- | Extract data from the given transition configuration and store it in the given state.
    --
    -- /Warning/ - Should only be used in testing and benchmarking. Will result in an error
    -- when 'NetworkId' is set to 'Mainnet'.
    TransitionConfig era ->
    NewEpochState era ->
    NewEpochState era

  -- | In case when a previous era is available, we should always be able to access
  -- `TransitionConfig` for the previous era, from within the current era's
  -- `TransitionConfig`
  tcPreviousEraConfigL ::
    EraTransition (PreviousEra era) =>
    Lens' (TransitionConfig era) (TransitionConfig (PreviousEra era))

  -- | Lens for the `TranslationContext` for the current era from the `TransitionConfig`
  -- Translation context is a different name for the Genesis type for each era, they are
  -- one and the same concept.
  tcTranslationContextL ::
    Lens' (TransitionConfig era) (TranslationContext era)

  -- | Lens for the `ShelleyGenesis` from the `TransitionConfig`. Default implementation
  -- looks in the previous era's config
  tcShelleyGenesisL :: Lens' (TransitionConfig era) (ShelleyGenesis (EraCrypto era))
  default tcShelleyGenesisL ::
    (EraTransition (PreviousEra era), EraCrypto (PreviousEra era) ~ EraCrypto era) =>
    Lens' (TransitionConfig era) (ShelleyGenesis (EraCrypto era))
  tcShelleyGenesisL = tcPreviousEraConfigL . tcShelleyGenesisL

  -- | Get the initial PParams for the current era from the `TransitionConfig`. Note that
  -- this is only useful for testing and fast forward functionality, because this function
  -- assumes no on-chain changes to PParams through PParamsUpdate functionality.
  --
  -- Default implementation will use the PParams from the Previous era and the current
  -- `TranslationContext` to construct PParams for the current era.
  --
  -- /Warning/ - Should only be used in testing and benchmarking
  tcInitialPParamsG :: SimpleGetter (TransitionConfig era) (PParams era)
  default tcInitialPParamsG ::
    ( EraTransition (PreviousEra era)
    , TranslateEra era PParams
    , TranslationError era PParams ~ Void
    ) =>
    SimpleGetter (TransitionConfig era) (PParams era)
  tcInitialPParamsG =
    to $ \tc ->
      translateEra'
        (tc ^. tcTranslationContextL)
        (tc ^. tcPreviousEraConfigL . tcInitialPParamsG)

tcNetworkIDG :: EraTransition era => SimpleGetter (TransitionConfig era) Network
tcNetworkIDG = tcShelleyGenesisL . to sgNetworkId

registerInitialFundsThenStaking ::
  EraTransition era =>
  TransitionConfig era ->
  NewEpochState era ->
  NewEpochState era
registerInitialFundsThenStaking cfg =
  -- We must first register the initial funds, because the stake
  -- information depends on it.
  registerInitialStaking cfg . registerInitialFunds cfg

instance Crypto c => EraTransition (ShelleyEra c) where
  newtype TransitionConfig (ShelleyEra c) = ShelleyTransitionConfig
    { stcShelleyGenesis :: ShelleyGenesis c
    }
    deriving (Eq, Show, Generic)

  mkTransitionConfig =
    error "Impossible: There is no EraTransition instance for ByronEra"

  injectIntoTestState = registerInitialFundsThenStaking

  tcPreviousEraConfigL = notSupportedInThisEraL

  tcTranslationContextL =
    tcShelleyGenesisL . lens toFromByronTranslationContext setFBTC
    where
      setFBTC shelleyGenesis FromByronTranslationContext {..} =
        shelleyGenesis
          { sgGenDelegs = fbtcGenDelegs
          , sgProtocolParams = fbtcProtocolParams
          , sgMaxLovelaceSupply = fbtcMaxLovelaceSupply
          }

  tcShelleyGenesisL = lens stcShelleyGenesis (\tc sg -> tc {stcShelleyGenesis = sg})

  tcInitialPParamsG = to (sgProtocolParams . stcShelleyGenesis)

-- | Get the initial funds from the `TransitionConfig`. This value must be non-empty
-- only during testing and benchmarking, it must never contain anything on a real system.
--
-- /Warning/ - Should only be used in testing and benchmarking. Will result in an error
-- when NetworkId is set to Mainnet
tcInitialFundsL ::
  (HasCallStack, EraTransition era) =>
  Lens' (TransitionConfig era) (LM.ListMap (Addr (EraCrypto era)) Coin)
tcInitialFundsL =
  protectMainnetLens "InitialFunds" null $
    tcShelleyGenesisL . sgInitialFundsL

-- | Get the initial staking from the `TransitionConfig`. This value must be non-empty
-- only during testing and benchmarking, it must never contain anything on a real system.
--
-- /Warning/ - Should only be used in testing and benchmarking. Will result in an error
-- when NetworkId is set to Mainnet
tcInitialStakingL ::
  (HasCallStack, EraTransition era) =>
  Lens' (TransitionConfig era) (ShelleyGenesisStaking (EraCrypto era))
tcInitialStakingL =
  protectMainnetLens "InitialStaking" (== mempty) $
    tcShelleyGenesisL . sgStakingL

-- | Constructor for the base Shelley `TransitionConfig`
mkShelleyTransitionConfig :: ShelleyGenesis c -> TransitionConfig (ShelleyEra c)
mkShelleyTransitionConfig = ShelleyTransitionConfig

protectMainnetLens ::
  (HasCallStack, EraTransition era) =>
  String ->
  (a -> Bool) ->
  Lens' (TransitionConfig era) a ->
  Lens' (TransitionConfig era) a
protectMainnetLens name isMainnetSafe l =
  lens
    (\g -> protectMainnet name g isMainnetSafe $ g ^. l)
    (\g x -> g & l .~ x)

protectMainnet ::
  (HasCallStack, EraTransition era) =>
  String ->
  TransitionConfig era ->
  (a -> Bool) ->
  a ->
  a
protectMainnet name g isMainnetSafe m =
  if g ^. tcNetworkIDG == Mainnet && not (isMainnetSafe m)
    then error $ "Injection of " ++ name ++ " is not possible on Mainnet"
    else m

deriving instance Crypto c => NoThunks (TransitionConfig (ShelleyEra c))

instance Crypto c => ToJSON (TransitionConfig (ShelleyEra c)) where
  toJSON = object . toShelleyTransitionConfigPairs
  toEncoding = pairs . mconcat . toShelleyTransitionConfigPairs

instance Crypto c => FromJSON (TransitionConfig (ShelleyEra c)) where
  parseJSON = withObject "ShelleyTransitionConfig" $ \o -> do
    sg <- o .: "shelley"
    pure $ ShelleyTransitionConfig {stcShelleyGenesis = sg}

toShelleyTransitionConfigPairs ::
  (KeyValue e a, Crypto c) =>
  TransitionConfig (ShelleyEra c) ->
  [a]
toShelleyTransitionConfigPairs stc@(ShelleyTransitionConfig _) =
  ["shelley" .= object (toShelleyGenesisPairs (stcShelleyGenesis stc))]

-- | Helper function for constructing the initial state for any era
--
-- /Warning/ - Should only be used in testing and benchmarking. Will result in an error
-- when NetworkId is set to Mainnet
--
-- This function does not register any initial funds or delegates.
createInitialState ::
  forall era.
  (EraTransition era, HasCallStack) =>
  TransitionConfig era ->
  NewEpochState era
createInitialState tc =
  protectMainnet
    "InitialState"
    tc
    (const False)
    NewEpochState
      { nesEL = initialEpochNo
      , nesBprev = BlocksMade Map.empty
      , nesBcur = BlocksMade Map.empty
      , nesEs =
          EpochState
            { esAccountState = AccountState zero reserves
            , esSnapshots = emptySnapShots
            , esLState =
                LedgerState
                  { lsUTxOState =
                      smartUTxOState pp initialUtxo zero zero govState zero
                  , lsCertState =
                      CertState
                        { certDState = dState {dsGenDelegs = GenDelegs (sgGenDelegs sg)}
                        , certPState = def
                        , certVState = def
                        }
                  }
            , esNonMyopic = def
            }
      , nesRu = SNothing
      , nesPd = PoolDistr Map.empty mempty
      , stashedAVVMAddresses = def
      }
  where
    dState :: DState era
    dState = def
    govState :: GovState era
    govState =
      emptyGovState
        & curPParamsGovStateL .~ pp
        & prevPParamsGovStateL .~ pp
    pp :: PParams era
    pp = tc ^. tcInitialPParamsG
    sg :: ShelleyGenesis (EraCrypto era)
    sg = tc ^. tcShelleyGenesisL
    initialEpochNo :: EpochNo
    initialEpochNo = EpochNo 0
    initialUtxo :: UTxO era
    initialUtxo = mempty
    reserves :: Coin
    reserves = word64ToCoin (sgMaxLovelaceSupply sg) <-> coinBalance initialUtxo

-- | Register the initial staking information in the 'NewEpochState'.
--
-- HERE BE DRAGONS! This function is intended to help in testing.
--
-- In production, the genesis should /not/ contain any initial staking.
--
-- Any existing staking information is overridden, but the UTxO is left
-- untouched.
--
-- /Warning/ - Should only be used in testing and benchmarking. Will result in an error
-- when NetworkId is set to Mainnet
registerInitialStaking ::
  forall era.
  (HasCallStack, EraTransition era) =>
  TransitionConfig era ->
  NewEpochState era ->
  NewEpochState era
registerInitialStaking tc nes =
  nes
    { nesEs =
        epochState
          { esLState =
              ledgerState
                { lsCertState =
                    dpState
                      { certDState = dState'
                      , certPState = pState'
                      }
                }
          , esSnapshots =
              (esSnapshots epochState)
                { ssStakeMark = initSnapShot
                , ssStakeMarkPoolDistr = calculatePoolDistr initSnapShot
                }
          }
    , -- Note that this is only applicable in the initial configuration where
      -- there is no existing stake distribution, since it would completely
      -- overwrite any such thing.
      nesPd = calculatePoolDistr initSnapShot
    }
  where
    ShelleyGenesisStaking {sgsPools, sgsStake} = tc ^. tcInitialStakingL
    NewEpochState {nesEs = epochState} = nes
    ledgerState = esLState epochState
    dpState = lsCertState ledgerState

    -- New delegation state. Since we're using base addresses, we only care
    -- about updating the '_delegations' field.
    --
    -- See STS DELEG for details
    dState' :: DState era
    dState' =
      (certDState dpState)
        { dsUnified =
            UM.unify
              ( Map.map (const $ UM.RDPair (CompactCoin 0) (CompactCoin 0))
                  . Map.mapKeys KeyHashObj
                  $ sgsStakeMap
              )
              mempty
              (Map.mapKeys KeyHashObj sgsStakeMap)
              mempty
        }
      where
        sgsStakeMap = ListMap.toMap sgsStake

    -- We consider pools as having been registered in slot 0
    -- See STS POOL for details
    pState' :: PState era
    pState' =
      (certPState dpState)
        { psStakePoolParams = ListMap.toMap sgsPools
        }

    pp = nes ^. nesEsL . curPParamsEpochStateL

    -- The new stake distribution is made on the basis of a snapshot taken
    -- during the previous epoch. We create a "fake" snapshot in order to
    -- establish an initial stake distribution.
    initSnapShot :: SnapShot (EraCrypto era)
    initSnapShot =
      -- Since we build a stake from nothing, we first initialise an
      -- 'IncrementalStake' as empty, and then:
      --
      -- 1. Add the initial UTxO, whilst deleting nothing.
      -- 2. Update the stake map given the initial delegation.
      incrementalStakeDistr
        pp
        -- Note that 'updateStakeDistribution' takes first the set of UTxO to
        -- delete, and then the set to add. In our case, there is nothing to
        -- delete, since this is an initial UTxO set.
        (updateStakeDistribution pp mempty mempty (utxosUtxo (lsUTxOState ledgerState)))
        dState'
        pState'

-- | Register the initial funds in the 'NewEpochState'.
--
-- HERE BE DRAGONS! This function is intended to help in testing.
--
-- In production, the genesis should /not/ contain any initial funds.
--
-- The given funds are /added/ to the existing UTxO.
--
-- PRECONDITION: the given funds must not be part of the existing UTxO.
-- > forall (addr, _) in initialFunds.
-- >    Map.notElem (initialFundsPseudoTxIn addr) existingUTxO
--
-- PROPERTY:
-- >    genesisUTxO genesis
-- > == <genesisUTxO'> (sgInitialFunds genesis)
-- > == <extractUTxO> (registerInitialFunds (sgInitialFunds genesis)
-- >                                        <empty NewEpochState>)
--
-- /Warning/ - Should only be used in testing and benchmarking. Will result in an error
-- when NetworkId is set to Mainnet
registerInitialFunds ::
  forall era.
  ( EraTransition era
  , HasCallStack
  ) =>
  TransitionConfig era ->
  NewEpochState era ->
  NewEpochState era
registerInitialFunds tc nes =
  nes
    { nesEs =
        epochState
          { esAccountState = accountState'
          , esLState = ledgerState'
          }
    }
  where
    epochState = nesEs nes
    accountState = esAccountState epochState
    ledgerState = esLState epochState
    utxoState = lsUTxOState ledgerState
    utxo = utxosUtxo utxoState
    reserves = asReserves accountState

    initialFundsUtxo :: UTxO era
    initialFundsUtxo =
      UTxO $
        Map.fromList
          [ (txIn, txOut)
          | (addr, amount) <- ListMap.toList (tc ^. tcInitialFundsL)
          , let txIn = initialFundsPseudoTxIn addr
                txOut = mkBasicTxOut addr (inject amount)
          ]

    utxo' = mergeUtxoNoOverlap utxo initialFundsUtxo

    -- Update the reserves
    accountState' =
      accountState
        { asReserves = reserves <-> coin (balance initialFundsUtxo)
        }

    -- Since we only add entries to our UTxO, rather than spending them, there
    -- is nothing to delete in the incremental update.
    utxoToDel = UTxO mempty
    ledgerState' =
      ledgerState
        { lsUTxOState =
            utxoState
              { utxosUtxo = utxo'
              , -- Normally we would incrementally update here. But since we pass
                -- the full UTxO as "toAdd" rather than a delta, we simply
                -- reinitialise the full incremental stake.
                utxosStakeDistr =
                  updateStakeDistribution
                    (nes ^. nesEsL . curPParamsEpochStateL)
                    mempty
                    utxoToDel
                    utxo'
              }
        }

    -- Merge two UTxOs, throw an 'error' in case of overlap
    mergeUtxoNoOverlap ::
      HasCallStack =>
      UTxO era ->
      UTxO era ->
      UTxO era
    mergeUtxoNoOverlap (UTxO m1) (UTxO m2) =
      UTxO $
        Map.unionWithKey
          (\k _ _ -> error $ "initial fund part of UTxO: " <> show k)
          m1
          m2
