{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Shelley.Genesis
  ( ShelleyGenesisStaking (..),
    ShelleyGenesis (..),
    ValidationErr (..),
    emptyGenesisStaking,
    sgActiveSlotCoeff,
    genesisUTxO,
    initialFundsPseudoTxIn,
    validateGenesis,
    describeValidationErr,
    mkShelleyGlobals,
  )
where

import qualified Cardano.Crypto.Hash.Class as Crypto
import Cardano.Crypto.KES.Class (totalPeriodsKES)
import Cardano.Ledger.Address
import Cardano.Ledger.BaseTypes
  ( ActiveSlotCoeff,
    BoundedRational (boundRational, unboundRational),
    EpochSize (..),
    Globals (..),
    Network,
    PositiveUnitInterval,
    Version,
    mkActiveSlotCoeff,
  )
import Cardano.Ledger.Binary
  ( Decoder,
    DecoderError (..),
    Encoding,
    FromCBOR (..),
    ToCBOR (..),
    cborError,
    decodeRational,
    decodeRecordNamed,
    encodeListLen,
    enforceDecoderVersion,
    enforceEncodingVersion,
    shelleyProtVer,
  )
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Core
import Cardano.Ledger.Crypto (HASH, KES)
import qualified Cardano.Ledger.Crypto as CC (Crypto)
import Cardano.Ledger.Keys
import Cardano.Ledger.SafeHash (unsafeMakeSafeHash)
import Cardano.Ledger.Shelley.PParams
import Cardano.Ledger.Shelley.StabilityWindow
import Cardano.Ledger.Shelley.TxBody (PoolParams (..))
import Cardano.Ledger.TxIn (TxId (..), TxIn (..))
import Cardano.Ledger.UTxO (UTxO (UTxO))
import qualified Cardano.Ledger.Val as Val
import Cardano.Slotting.EpochInfo (EpochInfo)
import Cardano.Slotting.Time (SystemStart (SystemStart))
import Data.Aeson (FromJSON (..), ToJSON (..), (.!=), (.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.ListMap as LM
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (NominalDiffTime, UTCTime (..))
import Data.Unit.Strict (forceElemsToWHNF)
import Data.Word (Word32, Word64)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))

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
    sgsPools :: LM.ListMap (KeyHash 'StakePool c) (PoolParams c),
    -- | Stake-holding key hash credentials and the pools to delegate that stake
    -- to. We require the raw staking key hash in order to:
    --
    -- - Avoid pointer addresses, which would be tricky when there's no slot or
    --   transaction to point to.
    -- - Avoid script credentials.
    sgsStake :: LM.ListMap (KeyHash 'Staking c) (KeyHash 'StakePool c)
  }
  deriving stock (Eq, Show, Generic)

instance NoThunks (ShelleyGenesisStaking c)

instance CC.Crypto c => ToCBOR (ShelleyGenesisStaking c) where
  toCBOR (ShelleyGenesisStaking pools stake) =
    encodeListLen 2 <> toCBOR pools <> toCBOR stake

instance CC.Crypto c => FromCBOR (ShelleyGenesisStaking c) where
  fromCBOR = do
    decodeRecordNamed "ShelleyGenesisStaking" (const 2) $ do
      pools <- fromCBOR
      stake <- fromCBOR
      pure $ ShelleyGenesisStaking pools stake

-- | Empty genesis staking
emptyGenesisStaking :: ShelleyGenesisStaking c
emptyGenesisStaking =
  ShelleyGenesisStaking
    { sgsPools = mempty,
      sgsStake = mempty
    }

-- | Shelley genesis information
--
-- Note that this is needed only for a pure Shelley network, hence it being
-- defined here rather than in its own module. In mainnet, Shelley will
-- transition naturally from Byron, and thus will never have its own genesis
-- information.
data ShelleyGenesis era = ShelleyGenesis
  { sgSystemStart :: !UTCTime,
    sgNetworkMagic :: !Word32,
    sgNetworkId :: !Network,
    sgActiveSlotsCoeff :: !PositiveUnitInterval,
    sgSecurityParam :: !Word64,
    sgEpochLength :: !EpochSize,
    sgSlotsPerKESPeriod :: !Word64,
    sgMaxKESEvolutions :: !Word64,
    sgSlotLength :: !NominalDiffTime,
    sgUpdateQuorum :: !Word64,
    sgMaxLovelaceSupply :: !Word64,
    sgProtocolParams :: !(ShelleyPParams era),
    sgGenDelegs :: !(Map (KeyHash 'Genesis (EraCrypto era)) (GenDelegPair (EraCrypto era))),
    sgInitialFunds :: LM.ListMap (Addr (EraCrypto era)) Coin,
    sgStaking :: ShelleyGenesisStaking (EraCrypto era)
  }
  deriving stock (Eq, Show, Generic)

deriving instance Era era => NoThunks (ShelleyGenesis era)

sgActiveSlotCoeff :: ShelleyGenesis era -> ActiveSlotCoeff
sgActiveSlotCoeff = mkActiveSlotCoeff . sgActiveSlotsCoeff

instance Era era => ToJSON (ShelleyGenesis era) where
  toJSON = Aeson.object . toShelleyGenesisPairs
  toEncoding = Aeson.pairs . mconcat . toShelleyGenesisPairs

toShelleyGenesisPairs ::
  (Aeson.KeyValue a, CC.Crypto (EraCrypto era)) =>
  ShelleyGenesis era ->
  [a]
toShelleyGenesisPairs
  ShelleyGenesis
    { sgSystemStart,
      sgNetworkMagic,
      sgNetworkId,
      sgActiveSlotsCoeff,
      sgSecurityParam,
      sgEpochLength,
      sgSlotsPerKESPeriod,
      sgMaxKESEvolutions,
      sgSlotLength,
      sgUpdateQuorum,
      sgMaxLovelaceSupply,
      sgProtocolParams,
      sgGenDelegs,
      sgInitialFunds,
      sgStaking
    } =
    let !strictSgInitialFunds = sgInitialFunds
        !strictSgStaking = sgStaking
     in [ "systemStart" .= sgSystemStart,
          "networkMagic" .= sgNetworkMagic,
          "networkId" .= sgNetworkId,
          "activeSlotsCoeff" .= sgActiveSlotsCoeff,
          "securityParam" .= sgSecurityParam,
          "epochLength" .= sgEpochLength,
          "slotsPerKESPeriod" .= sgSlotsPerKESPeriod,
          "maxKESEvolutions" .= sgMaxKESEvolutions,
          "slotLength" .= sgSlotLength,
          "updateQuorum" .= sgUpdateQuorum,
          "maxLovelaceSupply" .= sgMaxLovelaceSupply,
          "protocolParams" .= sgProtocolParams,
          "genDelegs" .= sgGenDelegs,
          "initialFunds" .= strictSgInitialFunds,
          "staking" .= strictSgStaking
        ]

instance Era era => FromJSON (ShelleyGenesis era) where
  parseJSON =
    Aeson.withObject "ShelleyGenesis" $ \obj ->
      ShelleyGenesis
        <$> (forceUTCTime <$> obj .: "systemStart")
        <*> obj .: "networkMagic"
        <*> obj .: "networkId"
        <*> obj .: "activeSlotsCoeff"
        <*> obj .: "securityParam"
        <*> obj .: "epochLength"
        <*> obj .: "slotsPerKESPeriod"
        <*> obj .: "maxKESEvolutions"
        <*> obj .: "slotLength"
        <*> obj .: "updateQuorum"
        <*> obj .: "maxLovelaceSupply"
        <*> obj .: "protocolParams"
        <*> (forceElemsToWHNF <$> obj .: "genDelegs")
        <*> (forceElemsToWHNF <$> obj .: "initialFunds")
        <*> obj .:? "staking" .!= emptyGenesisStaking
    where
      forceUTCTime date =
        let !day = utctDay date
            !time = utctDayTime date
         in UTCTime day time

instance CC.Crypto c => ToJSON (ShelleyGenesisStaking c) where
  toJSON = Aeson.object . toShelleyGenesisStakingPairs
  toEncoding = Aeson.pairs . mconcat . toShelleyGenesisStakingPairs

toShelleyGenesisStakingPairs ::
  (Aeson.KeyValue a, CC.Crypto c) =>
  ShelleyGenesisStaking c ->
  [a]
toShelleyGenesisStakingPairs ShelleyGenesisStaking {sgsPools, sgsStake} =
  [ "pools" .= sgsPools,
    "stake" .= sgsStake
  ]

instance CC.Crypto c => FromJSON (ShelleyGenesisStaking c) where
  parseJSON =
    Aeson.withObject "ShelleyGenesisStaking" $ \obj ->
      ShelleyGenesisStaking
        <$> (forceElemsToWHNF <$> obj .: "pools")
        <*> (forceElemsToWHNF <$> obj .: "stake")

instance Era era => ToCBOR (ShelleyGenesis era) where
  toCBOR
    ShelleyGenesis
      { sgSystemStart,
        sgNetworkMagic,
        sgNetworkId,
        sgActiveSlotsCoeff,
        sgSecurityParam,
        sgEpochLength,
        sgSlotsPerKESPeriod,
        sgMaxKESEvolutions,
        sgSlotLength,
        sgUpdateQuorum,
        sgMaxLovelaceSupply,
        sgProtocolParams,
        sgGenDelegs,
        sgInitialFunds,
        sgStaking
      } =
      encodeListLen 15
        <> toCBOR sgSystemStart
        <> toCBOR sgNetworkMagic
        <> toCBOR sgNetworkId
        <> activeSlotsCoeffToCBOR sgActiveSlotsCoeff
        <> toCBOR sgSecurityParam
        <> toCBOR (unEpochSize sgEpochLength)
        <> toCBOR sgSlotsPerKESPeriod
        <> toCBOR sgMaxKESEvolutions
        <> toCBOR sgSlotLength
        <> toCBOR sgUpdateQuorum
        <> toCBOR sgMaxLovelaceSupply
        <> toCBOR sgProtocolParams
        <> toCBOR sgGenDelegs
        <> toCBOR sgInitialFunds
        <> toCBOR sgStaking

instance Era era => FromCBOR (ShelleyGenesis era) where
  fromCBOR = do
    decodeRecordNamed "ShelleyGenesis" (const 15) $ do
      sgSystemStart <- fromCBOR
      sgNetworkMagic <- fromCBOR
      sgNetworkId <- fromCBOR
      sgActiveSlotsCoeff <- activeSlotsCoeffFromCBOR
      sgSecurityParam <- fromCBOR
      sgEpochLength <- fromCBOR
      sgSlotsPerKESPeriod <- fromCBOR
      sgMaxKESEvolutions <- fromCBOR
      sgSlotLength <- fromCBOR
      sgUpdateQuorum <- fromCBOR
      sgMaxLovelaceSupply <- fromCBOR
      sgProtocolParams <- fromCBOR
      sgGenDelegs <- fromCBOR
      sgInitialFunds <- fromCBOR
      sgStaking <- fromCBOR
      pure $
        ShelleyGenesis
          sgSystemStart
          sgNetworkMagic
          sgNetworkId
          sgActiveSlotsCoeff
          sgSecurityParam
          (EpochSize sgEpochLength)
          sgSlotsPerKESPeriod
          sgMaxKESEvolutions
          sgSlotLength
          sgUpdateQuorum
          sgMaxLovelaceSupply
          sgProtocolParams
          sgGenDelegs
          sgInitialFunds
          sgStaking

-- | Serialize `PositiveUnitInterval` type in the same way `Rational` is serialized,
-- however ensure there is no usage of tag 30 by enforcing Shelley protocol version.
activeSlotsCoeffToCBOR :: PositiveUnitInterval -> Encoding
activeSlotsCoeffToCBOR = enforceEncodingVersion shelleyProtVer . toCBOR . unboundRational

-- | Deserialize `PositiveUnitInterval` type using `Rational` deserialization and fail
-- when bounds are violated. Also, ensure there is no usage of tag 30 by enforcing Shelley
-- protocol version.
activeSlotsCoeffFromCBOR :: Decoder s PositiveUnitInterval
activeSlotsCoeffFromCBOR = do
  r <- enforceDecoderVersion shelleyProtVer $ decodeRational
  case boundRational r of
    Nothing ->
      cborError $ DecoderErrorCustom "ActiveSlotsCoeff (PositiveUnitInterval)" (Text.pack $ show r)
    Just u -> pure u

{-------------------------------------------------------------------------------
  Genesis UTxO
-------------------------------------------------------------------------------}

genesisUTxO ::
  forall era.
  EraTxOut era =>
  ShelleyGenesis era ->
  UTxO era
genesisUTxO genesis =
  UTxO $
    Map.fromList
      [ (txIn, txOut)
        | (addr, amount) <- LM.unListMap (sgInitialFunds genesis),
          let txIn = initialFundsPseudoTxIn addr
              txOut = mkBasicTxOut addr (Val.inject amount)
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
initialFundsPseudoTxIn :: forall c. CC.Crypto c => Addr c -> TxIn c
initialFundsPseudoTxIn addr =
  TxIn (pseudoTxId addr) minBound
  where
    pseudoTxId =
      TxId
        . unsafeMakeSafeHash
        . ( Crypto.castHash ::
              Crypto.Hash (HASH c) (Addr c) ->
              Crypto.Hash (HASH c) EraIndependentTxBody
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
  forall era.
  Era era =>
  ShelleyGenesis era ->
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
        let activeSlotsCoeff = unboundRational sgActiveSlotsCoeff
            minLength =
              EpochSize . ceiling $
                fromIntegral @_ @Double (3 * sgSecurityParam)
                  / fromRational activeSlotsCoeff
         in if minLength > sgEpochLength
              then
                Just $
                  EpochNotLongEnough
                    sgEpochLength
                    sgSecurityParam
                    activeSlotsCoeff
                    minLength
              else Nothing
      checkKesEvolutions =
        if sgMaxKESEvolutions
          <= fromIntegral (totalPeriodsKES (Proxy @(KES (EraCrypto era))))
          then Nothing
          else
            Just $
              MaxKESEvolutionsUnsupported
                sgMaxKESEvolutions
                (totalPeriodsKES (Proxy @(KES (EraCrypto era))))
      checkQuorumSize =
        let numGenesisNodes = fromIntegral $ length sgGenDelegs
            maxTooSmal = numGenesisNodes `div` 2
         in if numGenesisNodes == 0 || sgUpdateQuorum > maxTooSmal
              then Nothing
              else Just $ QuorumTooSmall sgUpdateQuorum maxTooSmal numGenesisNodes

{-------------------------------------------------------------------------------
  Construct 'Globals' using 'ShelleyGenesis'
-------------------------------------------------------------------------------}

mkShelleyGlobals ::
  ShelleyGenesis era ->
  EpochInfo (Either Text) ->
  Version ->
  Globals
mkShelleyGlobals genesis epochInfoAc maxMajorPV =
  Globals
    { activeSlotCoeff = sgActiveSlotCoeff genesis,
      epochInfo = epochInfoAc,
      maxKESEvo = sgMaxKESEvolutions genesis,
      maxLovelaceSupply = sgMaxLovelaceSupply genesis,
      maxMajorPV = maxMajorPV,
      networkId = sgNetworkId genesis,
      quorum = sgUpdateQuorum genesis,
      randomnessStabilisationWindow,
      securityParameter = k,
      slotsPerKESPeriod = sgSlotsPerKESPeriod genesis,
      stabilityWindow,
      systemStart
    }
  where
    systemStart = SystemStart $ sgSystemStart genesis
    k = sgSecurityParam genesis
    stabilityWindow =
      computeStabilityWindow k (sgActiveSlotCoeff genesis)
    randomnessStabilisationWindow =
      computeRandomnessStabilisationWindow k (sgActiveSlotCoeff genesis)
