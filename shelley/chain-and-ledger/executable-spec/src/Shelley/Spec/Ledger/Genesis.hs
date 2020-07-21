{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Shelley.Spec.Ledger.Genesis
  ( ShelleyGenesisStaking (..),
    ShelleyGenesis (..),
    ValidationErr (..),
    emptyGenesisStaking,
    sgActiveSlotCoeff,
    genesisUtxO,
    initialFundsPseudoTxIn,
    validateGenesis,
    describeValidationErr,
  )
where

import qualified Cardano.Crypto.Hash.Class as Crypto
import Cardano.Crypto.KES.Class (totalPeriodsKES)
import Cardano.Prelude (NoUnexpectedThunks)
import Cardano.Slotting.Slot (EpochSize (..))
import Data.Aeson (FromJSON (..), ToJSON (..), (.!=), (.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import Data.Proxy (Proxy (..))
import Data.Scientific (Scientific)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (NominalDiffTime, UTCTime)
import Data.Word (Word32, Word64)
import GHC.Generics (Generic)
import Shelley.Spec.Ledger.Address
import Shelley.Spec.Ledger.BaseTypes
import Shelley.Spec.Ledger.Coin
import Shelley.Spec.Ledger.Crypto (Crypto, HASH, KES)
import Shelley.Spec.Ledger.Keys
import Shelley.Spec.Ledger.PParams
import Shelley.Spec.Ledger.TxData
import Shelley.Spec.Ledger.UTxO

-- | Genesis Shelley staking configuration.
--
-- This allows us to configure some initial stake pools and delegation to them,
-- in order to test Praos in a static configuration, without requiring on-chain
-- registration and delegation.
--
-- For simplicity, pools defined in the genesis staking do not pay deposits for
-- their registration.
data ShelleyGenesisStaking c = ShelleyGenesisStaking
  { -- | Pools to register
    --
    --   The key in this map is the hash of the public key of the _pool_. This
    --   need not correspond to any payment or staking key, but must correspond
    --   to the cold key held by 'TPraosIsCoreNode'.
    sgsPools :: !(Map (KeyHash 'StakePool c) (PoolParams c)),
    -- | Stake-holding key hash credentials and the pools to delegate that stake
    -- to. We require the raw staking key hash in order to:
    --
    -- - Avoid pointer addresses, which would be tricky when there's no slot or
    --   transaction to point to.
    -- - Avoid script credentials.
    sgsStake :: !(Map (KeyHash 'Staking c) (KeyHash 'StakePool c))
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NoUnexpectedThunks)

-- | Empty genesis staking
emptyGenesisStaking :: ShelleyGenesisStaking c
emptyGenesisStaking =
  ShelleyGenesisStaking
    { sgsPools = Map.empty,
      sgsStake = Map.empty
    }

-- | Shelley genesis information
--
-- Note that this is needed only for a pure Shelley network, hence it being
-- defined here rather than in its own module. In mainnet, Shelley will
-- transition naturally from Byron, and thus will never have its own genesis
-- information.
data ShelleyGenesis c = ShelleyGenesis
  { sgSystemStart :: !UTCTime,
    sgNetworkMagic :: !Word32,
    sgNetworkId :: !Network,
    sgActiveSlotsCoeff :: !Rational,
    sgSecurityParam :: !Word64,
    sgEpochLength :: !EpochSize,
    sgSlotsPerKESPeriod :: !Word64,
    sgMaxKESEvolutions :: !Word64,
    sgSlotLength :: !NominalDiffTime,
    sgUpdateQuorum :: !Word64,
    sgMaxLovelaceSupply :: !Word64,
    sgProtocolParams :: !PParams,
    sgGenDelegs :: !(Map (KeyHash 'Genesis c) (GenDelegPair c)),
    sgInitialFunds :: !(Map (Addr c) Coin),
    sgStaking :: !(ShelleyGenesisStaking c)
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NoUnexpectedThunks)

sgActiveSlotCoeff :: ShelleyGenesis c -> ActiveSlotCoeff
sgActiveSlotCoeff =
  mkActiveSlotCoeff
    . unitIntervalFromRational
    . sgActiveSlotsCoeff

instance Crypto crypto => ToJSON (ShelleyGenesis crypto) where
  toJSON sg =
    Aeson.object
      [ "systemStart" .= sgSystemStart sg,
        "networkMagic" .= sgNetworkMagic sg,
        "networkId" .= sgNetworkId sg,
        "activeSlotsCoeff" .= (fromRational (sgActiveSlotsCoeff sg) :: Scientific),
        "securityParam" .= sgSecurityParam sg,
        "epochLength" .= sgEpochLength sg,
        "slotsPerKESPeriod" .= sgSlotsPerKESPeriod sg,
        "maxKESEvolutions" .= sgMaxKESEvolutions sg,
        "slotLength" .= sgSlotLength sg,
        "updateQuorum" .= sgUpdateQuorum sg,
        "maxLovelaceSupply" .= sgMaxLovelaceSupply sg,
        "protocolParams" .= sgProtocolParams sg,
        "genDelegs" .= sgGenDelegs sg,
        "initialFunds" .= sgInitialFunds sg,
        "staking" .= sgStaking sg
      ]

instance Crypto crypto => FromJSON (ShelleyGenesis crypto) where
  parseJSON =
    Aeson.withObject "ShelleyGenesis" $ \obj ->
      ShelleyGenesis
        <$> obj .: "systemStart"
        <*> obj .: "networkMagic"
        <*> obj .: "networkId"
        <*> ( (toRational :: Scientific -> Rational)
                <$> obj .: "activeSlotsCoeff"
            )
        <*> obj .: "securityParam"
        <*> obj .: "epochLength"
        <*> obj .: "slotsPerKESPeriod"
        <*> obj .: "maxKESEvolutions"
        <*> obj .: "slotLength"
        <*> obj .: "updateQuorum"
        <*> obj .: "maxLovelaceSupply"
        <*> obj .: "protocolParams"
        <*> obj .: "genDelegs"
        <*> obj .: "initialFunds"
        <*> obj .:? "staking" .!= emptyGenesisStaking

instance Crypto c => ToJSON (ShelleyGenesisStaking c) where
  toJSON sgs =
    Aeson.object
      [ "pools" .= sgsPools sgs,
        "stake" .= sgsStake sgs
      ]

instance Crypto c => FromJSON (ShelleyGenesisStaking c) where
  parseJSON =
    Aeson.withObject "ShelleyGenesisStaking" $ \obj ->
      ShelleyGenesisStaking
        <$> obj .: "pools"
        <*> obj .: "stake"

{-------------------------------------------------------------------------------
  Genesis UTxO
-------------------------------------------------------------------------------}

genesisUtxO :: Crypto c => ShelleyGenesis c -> UTxO c
genesisUtxO genesis =
  UTxO $
    Map.fromList
      [ (txIn, txOut)
        | (addr, amount) <- Map.toList (sgInitialFunds genesis),
          let txIn = initialFundsPseudoTxIn addr
              txOut = TxOut addr amount
      ]

-- | Compute the 'TxIn' of the initial UTxO pseudo-transaction corresponding
-- to the given address in the genesis initial funds.
--
-- The Shelley initial UTxO is constructed from the 'sgInitialFunds' which
-- is not a full UTxO but just a map from addresses to coin values.
--
-- This gets turned into a UTxO by making a pseudo-transaction for each address,
-- with the 0th output being the coin value. So to spend from the initial UTxO
-- we need this same 'TxIn' to use as an input to the spending transaction.
initialFundsPseudoTxIn :: forall c. Crypto c => Addr c -> TxIn c
initialFundsPseudoTxIn addr =
  TxIn (pseudoTxId addr) 0
  where
    pseudoTxId =
      TxId
        . ( Crypto.castHash ::
              Crypto.Hash (HASH c) (Addr c) ->
              Crypto.Hash (HASH c) (TxBody c)
          )
        . Crypto.hashWith serialiseAddr

{-------------------------------------------------------------------------------
  Genesis validation
-------------------------------------------------------------------------------}

data ValidationErr
  = EpochNotLongEnough EpochSize Word64 Rational EpochSize
  | MaxKESEvolutionsUnsupported Word64 Word
  | QuorumTooSmall Word64 Word64 Word64
  deriving (Eq, Show)

describeValidationErr :: ValidationErr -> Text
describeValidationErr (EpochNotLongEnough es secParam asc minEpochSize) =
  mconcat
    [ "Epoch length is too low. Your epoch length of ",
      Text.pack (show es),
      " does not meet the minimum epoch length of ",
      Text.pack (show minEpochSize),
      " required by your choice of parameters for k and f: ",
      Text.pack (show secParam),
      " and ",
      Text.pack (show asc),
      ". Epochs should be at least 10k/f slots long."
    ]
describeValidationErr (MaxKESEvolutionsUnsupported reqKES supportedKES) =
  mconcat
    [ "You have specified a 'maxKESEvolutions' higher",
      " than that supported by the underlying algorithm.",
      " You requested ",
      Text.pack (show reqKES),
      " but the algorithm supports a maximum of ",
      Text.pack (show supportedKES)
    ]
describeValidationErr (QuorumTooSmall q maxTooSmal nodes) =
  mconcat
    [ "You have specified an 'updateQuorum' which is",
      " too small compared to the number of genesis nodes.",
      " You requested ",
      Text.pack (show q),
      ", but given ",
      Text.pack (show nodes),
      " genesis nodes 'updateQuorum' must be greater than ",
      Text.pack (show maxTooSmal)
    ]

-- | Do some basic sanity checking on the Shelley genesis file.
validateGenesis ::
  forall c.
  Crypto c =>
  ShelleyGenesis c ->
  Either [ValidationErr] ()
validateGenesis
  ShelleyGenesis
    { sgEpochLength,
      sgActiveSlotsCoeff,
      sgMaxKESEvolutions,
      sgSecurityParam,
      sgUpdateQuorum,
      sgGenDelegs
    } =
    case catMaybes errors of
      [] -> Right ()
      xs -> Left xs
    where
      errors =
        [ checkEpochLength,
          checkKesEvolutions,
          checkQuorumSize
        ]
      checkEpochLength =
        let minLength =
              EpochSize . ceiling $
                fromIntegral @_ @Double (3 * sgSecurityParam)
                  / fromRational sgActiveSlotsCoeff
         in if minLength > sgEpochLength
              then
                Just $
                  EpochNotLongEnough
                    sgEpochLength
                    sgSecurityParam
                    sgActiveSlotsCoeff
                    minLength
              else Nothing
      checkKesEvolutions =
        if sgMaxKESEvolutions <= fromIntegral (totalPeriodsKES (Proxy @(KES c)))
          then Nothing
          else
            Just $
              MaxKESEvolutionsUnsupported
                sgMaxKESEvolutions
                (totalPeriodsKES (Proxy @(KES c)))
      checkQuorumSize =
        let numGenesisNodes = fromIntegral $ length sgGenDelegs
            maxTooSmal = numGenesisNodes `div` 2
         in if numGenesisNodes == 0 || sgUpdateQuorum > maxTooSmal
              then Nothing
              else Just $ QuorumTooSmall sgUpdateQuorum maxTooSmal numGenesisNodes
