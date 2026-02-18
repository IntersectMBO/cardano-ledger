{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- | Besides capturing all configuration that is necessary to progress to a specific era,
-- this interface also provides fast forward functionality that is used in testing and
-- benchmarking in order to initilize a chain in a particular era without going through
-- the trouble of generating all the history for preceeding eras.
module Cardano.Ledger.Shelley.Transition (
  EraTransition (
    TransitionConfig,
    mkTransitionConfig,
    injectIntoTestState,
    tcPreviousEraConfigL,
    tcTranslationContextL,
    tcShelleyGenesisL,
    tcInitialPParamsG
  ),
  pattern ShelleyTransitionConfig,
  tcInitialFundsL,
  tcInitialStakingL,
  mkShelleyTransitionConfig,
  createInitialState,
  shelleyRegisterInitialFundsThenStaking,
  shelleyRegisterInitialAccounts,
  registerInitialStakePools,
  registerInitialFunds,
  resetStakeDistribution,
  protectMainnet,
  protectMainnetLens,
) where

import Cardano.Ledger.Address
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin
import Cardano.Ledger.Core
import Cardano.Ledger.Credential
import Cardano.Ledger.Genesis
import Cardano.Ledger.Keys
import Cardano.Ledger.Shelley.Era
import Cardano.Ledger.Shelley.Genesis
import Cardano.Ledger.Shelley.Governance
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Shelley.State
import Cardano.Ledger.Shelley.Translation (
  FromByronTranslationContext (..),
  toFromByronTranslationContext,
 )
import Cardano.Ledger.Val
import Data.Aeson (FromJSON (..), KeyValue (..), ToJSON (..), object, withObject, (.:))
import qualified Data.Aeson as Aeson (Value (..))
import Data.Aeson.Key (Key, fromString)
import Data.Aeson.Types (Parser)
import Data.Char (toLower)
import Data.Default
import Data.Kind
import qualified Data.ListMap as ListMap
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Typeable
import Data.Void (Void)
import GHC.Generics (Generic)
import GHC.Stack
import Lens.Micro
import NoThunks.Class (NoThunks (..))

-- | Register the initial information in the 'NewEpochState'.
--
-- HERE BE DRAGONS! This interface is intended to help in testing and benchmarking.
--
-- In production, the genesis should /not/ contain any initial information about accounts, stake
-- pools or dreps.
--
-- /Warning/ - Should only be used in testing and benchmarking. Will result in an error
-- when NetworkId is set to Mainnet
class
  ( EraTxOut era
  , EraGov era
  , EraStake era
  , EraGenesis era
  , EraCertState era
  , Eq (TransitionConfig era)
  , Show (TransitionConfig era)
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
  tcShelleyGenesisL :: Lens' (TransitionConfig era) ShelleyGenesis
  default tcShelleyGenesisL ::
    EraTransition (PreviousEra era) =>
    Lens' (TransitionConfig era) ShelleyGenesis
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

  toTransitionConfigKeyValuePairs ::
    KeyValue e a =>
    TransitionConfig era ->
    [a]
  default toTransitionConfigKeyValuePairs ::
    ( EraTransition (PreviousEra era)
    , ToKeyValuePairs (TranslationContext era)
    , ToKeyValuePairs (TransitionConfig (PreviousEra era))
    , Typeable (TranslationContext era)
    , KeyValue e a
    ) =>
    TransitionConfig era ->
    [a]
  toTransitionConfigKeyValuePairs config =
    toKeyValuePairs (config ^. tcPreviousEraConfigL) ++ translationContextPairs
    where
      translationContextPairs =
        case eqT :: Maybe (TranslationContext era :~: NoGenesis era) of
          Nothing ->
            [ eraNameKey @era .= object (toKeyValuePairs (config ^. tcTranslationContextL))
            ]
          Just Refl -> []

  parseTransitionConfigJSON :: Aeson.Value -> Parser (TransitionConfig era)
  default parseTransitionConfigJSON ::
    ( Typeable (TranslationContext era)
    , FromJSON (TranslationContext era)
    , FromJSON (TransitionConfig (PreviousEra era))
    ) =>
    Aeson.Value ->
    Parser (TransitionConfig era)
  parseTransitionConfigJSON = withObject (eraName @era <> "TransitionConfig") $ \o -> do
    prevTransitionConfig :: TransitionConfig (PreviousEra era) <- parseJSON (Aeson.Object o)
    genesis <- mkGenesisWith @era (o .: eraNameKey @era)
    pure $ mkTransitionConfig genesis prevTransitionConfig

eraNameKey :: forall era. Era era => Key
eraNameKey = fromString (map toLower (eraName @era))

instance EraTransition era => ToKeyValuePairs (TransitionConfig era) where
  toKeyValuePairs = toTransitionConfigKeyValuePairs

deriving via
  KeyValuePairs (TransitionConfig era)
  instance
    ToKeyValuePairs (TransitionConfig era) => ToJSON (TransitionConfig era)

instance EraTransition era => FromJSON (TransitionConfig era) where
  parseJSON = parseTransitionConfigJSON

tcNetworkIDG :: EraTransition era => SimpleGetter (TransitionConfig era) Network
tcNetworkIDG = tcShelleyGenesisL . to sgNetworkId

shelleyRegisterInitialFundsThenStaking ::
  (EraTransition era, ShelleyEraAccounts era) =>
  TransitionConfig era ->
  NewEpochState era ->
  NewEpochState era
shelleyRegisterInitialFundsThenStaking cfg =
  -- We must first register the initial funds, because the stake
  -- information depends on it.
  resetStakeDistribution
    . shelleyRegisterInitialAccounts (cfg ^. tcInitialStakingL)
    . registerInitialStakePools (cfg ^. tcInitialStakingL)
    . registerInitialFunds cfg

instance EraTransition ShelleyEra where
  newtype TransitionConfig ShelleyEra = ShelleyTransitionConfig
    { stcShelleyGenesis :: ShelleyGenesis
    }
    deriving (Eq, Show, Generic)

  mkTransitionConfig =
    error "Impossible: There is no EraTransition instance for ByronEra"

  injectIntoTestState = shelleyRegisterInitialFundsThenStaking

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

  toTransitionConfigKeyValuePairs stc@(ShelleyTransitionConfig _) =
    ["shelley" .= object (toKeyValuePairs (stcShelleyGenesis stc))]

  parseTransitionConfigJSON = withObject "ShelleyTransitionConfig" $ \o -> do
    sg <- o .: "shelley"
    pure $ ShelleyTransitionConfig {stcShelleyGenesis = sg}

-- | Get the initial funds from the `TransitionConfig`. This value must be non-empty
-- only during testing and benchmarking, it must never contain anything on a real system.
--
-- /Warning/ - Should only be used in testing and benchmarking. Will result in an error
-- when NetworkId is set to Mainnet
tcInitialFundsL ::
  (HasCallStack, EraTransition era) =>
  Lens' (TransitionConfig era) (ListMap.ListMap Addr Coin)
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
  Lens' (TransitionConfig era) ShelleyGenesisStaking
tcInitialStakingL =
  protectMainnetLens "InitialStaking" (== mempty) $
    tcShelleyGenesisL . sgStakingL

-- | Constructor for the base Shelley `TransitionConfig`
mkShelleyTransitionConfig :: ShelleyGenesis -> TransitionConfig ShelleyEra
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

deriving instance NoThunks (TransitionConfig ShelleyEra)

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
            { esChainAccountState =
                ChainAccountState
                  { casTreasury = zero
                  , casReserves = reserves
                  }
            , esSnapshots = emptySnapShots
            , esLState =
                LedgerState
                  { lsUTxOState =
                      smartUTxOState pp initialUtxo zero zero govState zero
                  , lsCertState =
                      def & certDStateL . dsGenDelegsL .~ GenDelegs (sgGenDelegs sg)
                  }
            , esNonMyopic = def
            }
      , nesRu = SNothing
      , nesPd = def
      , stashedAVVMAddresses = def
      }
  where
    govState :: GovState era
    govState =
      emptyGovState
        & curPParamsGovStateL .~ pp
        & prevPParamsGovStateL .~ pp
    pp :: PParams era
    pp = tc ^. tcInitialPParamsG
    sg :: ShelleyGenesis
    sg = tc ^. tcShelleyGenesisL
    initialEpochNo :: EpochNo
    initialEpochNo = EpochNo 0
    initialUtxo :: UTxO era
    initialUtxo = mempty
    reserves :: Coin
    reserves = word64ToCoin (sgMaxLovelaceSupply sg) <-> sumCoinUTxO initialUtxo

-- | Register initial stake pools from the `ShelleyGenesisStaking`
registerInitialStakePools ::
  forall era.
  (EraCertState era, EraGov era) =>
  ShelleyGenesisStaking ->
  NewEpochState era ->
  NewEpochState era
registerInitialStakePools ShelleyGenesisStaking {sgsPools} nes =
  nes
    & nesEsL . esLStateL . lsCertStateL . certPStateL . psStakePoolsL
      .~ ListMap.toMap (mkStakePoolState deposit mempty <$> sgsPools)
  where
    deposit = nes ^. nesEsL . curPParamsEpochStateL . ppPoolDepositCompactL

-- | Register all staking credentials and apply delegations. Make sure StakePools that are being
-- delegated to are already registered, which can be done with `registerInitialStakePools`.
shelleyRegisterInitialAccounts ::
  forall era.
  (HasCallStack, ShelleyEraAccounts era, EraCertState era, EraGov era) =>
  ShelleyGenesisStaking ->
  NewEpochState era ->
  NewEpochState era
shelleyRegisterInitialAccounts ShelleyGenesisStaking {sgsStake} nes =
  nes
    & nesEsL . esLStateL . lsCertStateL . certDStateL . accountsL .~ updatedAccounts
    & nesEsL . esLStateL . lsCertStateL . certPStateL . psStakePoolsL .~ updatedStakePoolStates
  where
    stakePools = nes ^. nesEsL . esLStateL . lsCertStateL . certPStateL . psStakePoolsL
    initialAccounts = nes ^. nesEsL . esLStateL . lsCertStateL . certDStateL . accountsL
    deposit = compactCoinOrError $ nes ^. nesEsL . curPParamsEpochStateL . ppKeyDepositL

    !(!updatedAccounts, !updatedStakePoolStates) =
      foldr registerAndDelegate (initialAccounts, stakePools) (zip (ListMap.toList sgsStake) ptrs)
    registerAndDelegate ((stakeKeyHash, stakePool), ptr) (!accounts, !stakePoolMap)
      | stakePool `Map.member` stakePools =
          ( registerShelleyAccount (KeyHashObj stakeKeyHash) ptr deposit (Just stakePool) accounts
          , Map.adjust (spsDelegatorsL %~ Set.insert (KeyHashObj stakeKeyHash)) stakePool stakePoolMap
          )
      | otherwise =
          error $
            "Invariant of a delegation of "
              ++ show stakeKeyHash
              ++ " to an unregistered stake pool "
              ++ show stakePool
              ++ " is being violated."
    ptrs =
      [ Ptr minBound txIx certIx | txIx <- [minBound .. maxBound], certIx <- [minBound .. maxBound]
      ]

-- NOTE: it seems like this is only used for testing, so hardcoding `Testnet` as a Network for now

-- | Having initial funds, stake pools and accounts with delegations, we need to reset the stake
-- distribution, otherwise those initial stake pools will not be able to produce blocks
resetStakeDistribution ::
  (EraCertState era, EraStake era) =>
  NewEpochState era ->
  NewEpochState era
resetStakeDistribution nes =
  nes
    & nesEsL . esSnapshotsL . ssStakeMarkL .~ initSnapShot
    & nesEsL . esSnapshotsL . ssStakeMarkPoolDistrL .~ poolDistr
    & nesPdL .~ poolDistr
  where
    dState = nes ^. nesEsL . esLStateL . lsCertStateL . certDStateL
    pState = nes ^. nesEsL . esLStateL . lsCertStateL . certPStateL
    poolDistr = calculatePoolDistr initSnapShot
    -- The new stake distribution is made on the basis of a snapshot taken
    -- during the previous epoch. We create a "fake" snapshot in order to
    -- establish an initial stake distribution.
    initSnapShot :: SnapShot
    initSnapShot =
      snapShotFromInstantStake (addInstantStake (nes ^. utxoL) mempty) dState pState

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
          { esChainAccountState = accountState'
          , esLState = ledgerState'
          }
    }
  where
    epochState = nesEs nes
    accountState = esChainAccountState epochState
    ledgerState = esLState epochState
    utxoState = lsUTxOState ledgerState
    utxo = utxosUtxo utxoState

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
        { casReserves = casReserves accountState <-> sumCoinUTxO initialFundsUtxo
        }

    ledgerState' =
      ledgerState
        { lsUTxOState =
            utxoState
              { utxosUtxo = utxo'
              , -- Normally we would incrementally update here. But since we pass
                -- the full UTxO as "toAdd" rather than a delta, we simply
                -- reinitialise the full instant stake.
                utxosInstantStake = addInstantStake utxo' mempty
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
