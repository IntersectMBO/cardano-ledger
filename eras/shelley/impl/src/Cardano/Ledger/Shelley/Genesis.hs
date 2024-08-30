{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Shelley.Genesis (
  ShelleyGenesisStaking (..),
  ShelleyGenesis (..),
  toShelleyGenesisPairs,
  ValidationErr (..),
  NominalDiffTimeMicro (..),
  emptyGenesisStaking,
  sgActiveSlotCoeff,
  genesisUTxO,
  initialFundsPseudoTxIn,
  validateGenesis,
  describeValidationErr,
  mkShelleyGlobals,
  nominalDiffTimeMicroToMicroseconds,
  nominalDiffTimeMicroToSeconds,
  toNominalDiffTimeMicro,
  toNominalDiffTimeMicroWithRounding,
  fromNominalDiffTimeMicro,
  secondsToNominalDiffTimeMicro,
  sgInitialFundsL,
  sgStakingL,
)
where

import qualified Cardano.Crypto.Hash.Class as Crypto
import Cardano.Crypto.KES.Class (totalPeriodsKES)
import Cardano.Ledger.Address (Addr, serialiseAddr)
import Cardano.Ledger.BaseTypes (
  ActiveSlotCoeff,
  BoundedRational (boundRational, unboundRational),
  EpochSize (..),
  Globals (..),
  Network,
  Nonce (..),
  PositiveUnitInterval,
  mkActiveSlotCoeff,
 )
import Cardano.Ledger.Binary (
  DecCBOR (..),
  Decoder,
  DecoderError (..),
  EncCBOR (..),
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
  toPlainDecoder,
  toPlainEncoding,
 )
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Core
import Cardano.Ledger.Crypto (Crypto, HASH, KES)
import Cardano.Ledger.Genesis (EraGenesis (..))
import Cardano.Ledger.Keys
import Cardano.Ledger.PoolParams (PoolParams (..))
import Cardano.Ledger.SafeHash (unsafeMakeSafeHash)
import Cardano.Ledger.Shelley.Era (ShelleyEra)
import Cardano.Ledger.Shelley.PParams (ShelleyPParams (..))
import Cardano.Ledger.Shelley.StabilityWindow
import Cardano.Ledger.TxIn (TxId (..), TxIn (..))
import Cardano.Ledger.UTxO (UTxO (UTxO))
import qualified Cardano.Ledger.Val as Val
import Cardano.Slotting.EpochInfo (EpochInfo)
import Cardano.Slotting.Time (SystemStart (SystemStart))
import Control.Monad.Identity (Identity)
import Data.Aeson (FromJSON (..), ToJSON (..), object, (.!=), (.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (Parser, Value (..), typeMismatch)
import Data.Fixed (Fixed (..), Micro, Pico)
import qualified Data.ListMap as LM
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock (
  NominalDiffTime,
  UTCTime (..),
  nominalDiffTimeToSeconds,
  secondsToNominalDiffTime,
 )
import Data.Unit.Strict (forceElemsToWHNF)
import Data.Word (Word32, Word64)
import GHC.Generics (Generic)
import Lens.Micro (Lens', lens)
import NoThunks.Class (AllowThunksIn (..), NoThunks (..))

-- | Genesis Shelley staking configuration.
--
-- This allows us to configure some initial stake pools and delegation to them,
-- in order to test Praos in a static configuration, without requiring on-chain
-- registration and delegation.
--
-- For simplicity, pools defined in the genesis staking do not pay deposits for
-- their registration.
data ShelleyGenesisStaking c = ShelleyGenesisStaking
  { sgsPools :: LM.ListMap (KeyHash 'StakePool c) (PoolParams c)
  -- ^ Pools to register
  --
  --   The key in this map is the hash of the public key of the _pool_. This
  --   need not correspond to any payment or staking key, but must correspond
  --   to the cold key held by 'TPraosIsCoreNode'.
  , sgsStake :: LM.ListMap (KeyHash 'Staking c) (KeyHash 'StakePool c)
  -- ^ Stake-holding key hash credentials and the pools to delegate that stake
  -- to. We require the raw staking key hash in order to:
  --
  -- - Avoid pointer addresses, which would be tricky when there's no slot or
  --   transaction to point to.
  -- - Avoid script credentials.
  }
  deriving stock (Eq, Show, Generic)

instance NoThunks (ShelleyGenesisStaking c)

instance Semigroup (ShelleyGenesisStaking c) where
  (<>) (ShelleyGenesisStaking p1 s1) (ShelleyGenesisStaking p2 s2) =
    ShelleyGenesisStaking (p1 <> p2) (s1 <> s2)

instance Monoid (ShelleyGenesisStaking c) where
  mempty = ShelleyGenesisStaking mempty mempty

instance Crypto c => EncCBOR (ShelleyGenesisStaking c) where
  encCBOR (ShelleyGenesisStaking pools stake) =
    encodeListLen 2 <> encCBOR pools <> encCBOR stake

instance Crypto c => DecCBOR (ShelleyGenesisStaking c) where
  decCBOR = do
    decodeRecordNamed "ShelleyGenesisStaking" (const 2) $ do
      pools <- decCBOR
      stake <- decCBOR
      pure $ ShelleyGenesisStaking pools stake

-- | Empty genesis staking
emptyGenesisStaking :: ShelleyGenesisStaking c
emptyGenesisStaking = mempty

-- | Unlike @'NominalDiffTime'@ that supports @'Pico'@ precision, this type
-- only supports @'Micro'@ precision.
newtype NominalDiffTimeMicro = NominalDiffTimeMicro Micro
  deriving (Show, Eq, Generic)
  deriving anyclass (NoThunks)
  deriving newtype (Ord, Num, Fractional, Real, ToJSON, FromJSON, EncCBOR, DecCBOR)

-- | There is no loss of resolution in this conversion
microToPico :: Micro -> Pico
microToPico micro = fromRational @Pico $ toRational micro

-- | Loss of resolution occurs in this conversion
picoToMicro :: Pico -> Micro
picoToMicro pico = fromRational @Micro $ toRational pico

fromNominalDiffTimeMicro :: NominalDiffTimeMicro -> NominalDiffTime
fromNominalDiffTimeMicro =
  secondsToNominalDiffTime . microToPico . nominalDiffTimeMicroToMicroseconds

toNominalDiffTimeMicroWithRounding :: NominalDiffTime -> NominalDiffTimeMicro
toNominalDiffTimeMicroWithRounding =
  secondsToNominalDiffTimeMicro . picoToMicro . nominalDiffTimeToSeconds

toNominalDiffTimeMicro :: NominalDiffTime -> Maybe NominalDiffTimeMicro
toNominalDiffTimeMicro ndt
  | fromNominalDiffTimeMicro ndtm == ndt = Just ndtm
  | otherwise = Nothing
  where
    ndtm = toNominalDiffTimeMicroWithRounding ndt

secondsToNominalDiffTimeMicro :: Micro -> NominalDiffTimeMicro
secondsToNominalDiffTimeMicro = NominalDiffTimeMicro

nominalDiffTimeMicroToMicroseconds :: NominalDiffTimeMicro -> Micro
nominalDiffTimeMicroToMicroseconds (NominalDiffTimeMicro microseconds) = microseconds

nominalDiffTimeMicroToSeconds :: NominalDiffTimeMicro -> Pico
nominalDiffTimeMicroToSeconds (NominalDiffTimeMicro microseconds) = microToPico microseconds

-- | Shelley genesis information
--
-- Note that this is needed only for a pure Shelley network, hence it being
-- defined here rather than in its own module. In mainnet, Shelley will
-- transition naturally from Byron, and thus will never have its own genesis
-- information.
data ShelleyGenesis c = ShelleyGenesis
  { sgSystemStart :: !UTCTime
  , sgNetworkMagic :: !Word32
  , sgNetworkId :: !Network
  , sgActiveSlotsCoeff :: !PositiveUnitInterval
  , sgSecurityParam :: !Word64
  , sgEpochLength :: !EpochSize
  , sgSlotsPerKESPeriod :: !Word64
  , sgMaxKESEvolutions :: !Word64
  , sgSlotLength :: !NominalDiffTimeMicro
  , sgUpdateQuorum :: !Word64
  , sgMaxLovelaceSupply :: !Word64
  , sgProtocolParams :: !(PParams (ShelleyEra c))
  , sgGenDelegs :: !(Map (KeyHash 'Genesis c) (GenDelegPair c))
  , sgInitialFunds :: LM.ListMap (Addr c) Coin
  -- ^ 'sgInitialFunds' is intentionally kept lazy, as it can otherwise cause
  --   out-of-memory problems in testing and benchmarking.
  , sgStaking :: ShelleyGenesisStaking c
  -- ^ 'sgStaking' is intentionally kept lazy, as it can otherwise cause
  --   out-of-memory problems in testing and benchmarking.
  }
  deriving stock (Generic)

sgInitialFundsL :: Lens' (ShelleyGenesis c) (LM.ListMap (Addr c) Coin)
sgInitialFundsL = lens sgInitialFunds (\sg x -> sg {sgInitialFunds = x})

sgStakingL :: Lens' (ShelleyGenesis c) (ShelleyGenesisStaking c)
sgStakingL = lens sgStaking (\sg x -> sg {sgStaking = x})

deriving instance Crypto c => Show (ShelleyGenesis c)

deriving instance Crypto c => Eq (ShelleyGenesis c)

deriving via
  AllowThunksIn '["sgInitialFunds", "sgStaking"] (ShelleyGenesis c)
  instance
    Crypto c => NoThunks (ShelleyGenesis c)

sgActiveSlotCoeff :: ShelleyGenesis c -> ActiveSlotCoeff
sgActiveSlotCoeff = mkActiveSlotCoeff . sgActiveSlotsCoeff

instance Crypto c => ToJSON (ShelleyGenesis c) where
  toJSON = Aeson.object . toShelleyGenesisPairs
  toEncoding = Aeson.pairs . mconcat . toShelleyGenesisPairs

instance Crypto c => EraGenesis (ShelleyEra c) where
  type Genesis (ShelleyEra c) = ShelleyGenesis c

--------------------------------------------------
-- Legacy JSON representation of ShelleyGenesis --
--------------------------------------------------
newtype LegacyJSONPParams c = LegacyJSONPParams (PParamsHKD Identity (ShelleyEra c))

legacyFromJSONPParams :: LegacyJSONPParams c -> PParams (ShelleyEra c)
legacyFromJSONPParams (LegacyJSONPParams x) = PParams x

instance FromJSON (LegacyJSONPParams c) where
  parseJSON =
    Aeson.withObject "ShelleyPParams" $ \obj -> do
      LegacyJSONPParams
        <$> ( ShelleyPParams
                <$> obj .: "minFeeA"
                <*> obj .: "minFeeB"
                <*> obj .: "maxBlockBodySize"
                <*> obj .: "maxTxSize"
                <*> obj .: "maxBlockHeaderSize"
                <*> obj .: "keyDeposit"
                <*> obj .: "poolDeposit"
                <*> obj .: "eMax"
                <*> obj .: "nOpt"
                <*> obj .: "a0"
                <*> obj .: "rho"
                <*> obj .: "tau"
                <*> obj .: "decentralisationParam"
                <*> (parseNonce =<< (obj .: "extraEntropy"))
                <*> obj .: "protocolVersion"
                <*> obj .:? "minUTxOValue" .!= mempty
                <*> obj .:? "minPoolCost" .!= mempty
            )
    where
      parseNonce :: Aeson.Value -> Parser Nonce
      parseNonce =
        Aeson.withObject
          "Nonce"
          ( \obj -> do
              tag <- (obj .: "tag" :: Parser Text)
              case tag of
                "Nonce" -> Nonce <$> obj .: "contents"
                "NeutralNonce" -> return NeutralNonce
                _ -> typeMismatch "Nonce" (Object obj)
          )

legacyToJSONPParams :: PParams (ShelleyEra c) -> LegacyJSONPParams c
legacyToJSONPParams (PParams x) = LegacyJSONPParams x

instance ToJSON (LegacyJSONPParams c) where
  toJSON
    ( LegacyJSONPParams
        ( ShelleyPParams
            { sppMinFeeA
            , sppMinFeeB
            , sppMaxBBSize
            , sppMaxTxSize
            , sppMaxBHSize
            , sppKeyDeposit
            , sppPoolDeposit
            , sppEMax
            , sppNOpt
            , sppA0
            , sppRho
            , sppTau
            , sppD
            , sppExtraEntropy
            , sppProtocolVersion
            , sppMinUTxOValue
            , sppMinPoolCost
            }
          )
      ) =
      Aeson.object
        [ "minFeeA" .= sppMinFeeA
        , "minFeeB" .= sppMinFeeB
        , "maxBlockBodySize" .= sppMaxBBSize
        , "maxTxSize" .= sppMaxTxSize
        , "maxBlockHeaderSize" .= sppMaxBHSize
        , "keyDeposit" .= sppKeyDeposit
        , "poolDeposit" .= sppPoolDeposit
        , "eMax" .= sppEMax
        , "nOpt" .= sppNOpt
        , "a0" .= sppA0
        , "rho" .= sppRho
        , "tau" .= sppTau
        , "decentralisationParam" .= sppD
        , "extraEntropy"
            .= object
              ( case sppExtraEntropy of
                  Nonce hash ->
                    [ "tag" .= ("Nonce" :: Text)
                    , "contents" .= hash
                    ]
                  NeutralNonce -> ["tag" .= ("NeutralNonce" :: Text)]
              )
        , "protocolVersion" .= sppProtocolVersion
        , "minUTxOValue" .= sppMinUTxOValue
        , "minPoolCost" .= sppMinPoolCost
        ]

toShelleyGenesisPairs :: (Aeson.KeyValue e a, Crypto c) => ShelleyGenesis c -> [a]
toShelleyGenesisPairs
  ShelleyGenesis
    { sgSystemStart
    , sgNetworkMagic
    , sgNetworkId
    , sgActiveSlotsCoeff
    , sgSecurityParam
    , sgEpochLength
    , sgSlotsPerKESPeriod
    , sgMaxKESEvolutions
    , sgSlotLength
    , sgUpdateQuorum
    , sgMaxLovelaceSupply
    , sgProtocolParams
    , sgGenDelegs
    , sgInitialFunds
    , sgStaking
    } =
    let !strictSgInitialFunds = sgInitialFunds
        !strictSgStaking = sgStaking
     in [ "systemStart" .= sgSystemStart
        , "networkMagic" .= sgNetworkMagic
        , "networkId" .= sgNetworkId
        , "activeSlotsCoeff" .= sgActiveSlotsCoeff
        , "securityParam" .= sgSecurityParam
        , "epochLength" .= sgEpochLength
        , "slotsPerKESPeriod" .= sgSlotsPerKESPeriod
        , "maxKESEvolutions" .= sgMaxKESEvolutions
        , "slotLength" .= sgSlotLength
        , "updateQuorum" .= sgUpdateQuorum
        , "maxLovelaceSupply" .= sgMaxLovelaceSupply
        , "protocolParams" .= legacyToJSONPParams sgProtocolParams
        , "genDelegs" .= sgGenDelegs
        , "initialFunds" .= strictSgInitialFunds
        , "staking" .= strictSgStaking
        ]

instance Crypto c => FromJSON (ShelleyGenesis c) where
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
        <*> (legacyFromJSONPParams <$> obj .: "protocolParams")
        <*> (forceElemsToWHNF <$> obj .: "genDelegs")
        <*> (forceElemsToWHNF <$> obj .: "initialFunds") -- TODO: disable. Move to EraTransition
        <*> obj .:? "staking" .!= emptyGenesisStaking -- TODO: remove. Move to EraTransition
    where
      forceUTCTime date =
        let !day = utctDay date
            !time = utctDayTime date
         in UTCTime day time

instance Crypto c => ToJSON (ShelleyGenesisStaking c) where
  toJSON = Aeson.object . toShelleyGenesisStakingPairs
  toEncoding = Aeson.pairs . mconcat . toShelleyGenesisStakingPairs

toShelleyGenesisStakingPairs ::
  (Aeson.KeyValue e a, Crypto c) =>
  ShelleyGenesisStaking c ->
  [a]
toShelleyGenesisStakingPairs ShelleyGenesisStaking {sgsPools, sgsStake} =
  [ "pools" .= sgsPools
  , "stake" .= sgsStake
  ]

instance Crypto c => FromJSON (ShelleyGenesisStaking c) where
  parseJSON =
    Aeson.withObject "ShelleyGenesisStaking" $ \obj ->
      ShelleyGenesisStaking
        <$> (forceElemsToWHNF <$> obj .: "pools")
        <*> (forceElemsToWHNF <$> obj .: "stake")

-- | Genesis are always encoded with the version of era they are defined in.
instance Crypto c => DecCBOR (ShelleyGenesis c)

instance Crypto c => EncCBOR (ShelleyGenesis c)

instance Crypto c => ToCBOR (ShelleyGenesis c) where
  toCBOR
    ShelleyGenesis
      { sgSystemStart
      , sgNetworkMagic
      , sgNetworkId
      , sgActiveSlotsCoeff
      , sgSecurityParam
      , sgEpochLength
      , sgSlotsPerKESPeriod
      , sgMaxKESEvolutions
      , sgSlotLength
      , sgUpdateQuorum
      , sgMaxLovelaceSupply
      , sgProtocolParams
      , sgGenDelegs
      , sgInitialFunds
      , sgStaking
      } =
      toPlainEncoding shelleyProtVer $
        encodeListLen 15
          <> encCBOR sgSystemStart
          <> encCBOR sgNetworkMagic
          <> encCBOR sgNetworkId
          <> activeSlotsCoeffEncCBOR sgActiveSlotsCoeff
          <> encCBOR sgSecurityParam
          <> encCBOR (unEpochSize sgEpochLength)
          <> encCBOR sgSlotsPerKESPeriod
          <> encCBOR sgMaxKESEvolutions
          <> encCBOR sgSlotLength
          <> encCBOR sgUpdateQuorum
          <> encCBOR sgMaxLovelaceSupply
          <> encCBOR sgProtocolParams
          <> encCBOR sgGenDelegs
          <> encCBOR sgInitialFunds
          <> encCBOR sgStaking

instance Crypto c => FromCBOR (ShelleyGenesis c) where
  fromCBOR = toPlainDecoder shelleyProtVer $ do
    decodeRecordNamed "ShelleyGenesis" (const 15) $ do
      sgSystemStart <- decCBOR
      sgNetworkMagic <- decCBOR
      sgNetworkId <- decCBOR
      sgActiveSlotsCoeff <- activeSlotsCoeffDecCBOR
      sgSecurityParam <- decCBOR
      sgEpochLength <- decCBOR
      sgSlotsPerKESPeriod <- decCBOR
      sgMaxKESEvolutions <- decCBOR
      sgSlotLength <- decCBOR
      sgUpdateQuorum <- decCBOR
      sgMaxLovelaceSupply <- decCBOR
      sgProtocolParams <- decCBOR
      sgGenDelegs <- decCBOR
      sgInitialFunds <- decCBOR
      sgStaking <- decCBOR
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
activeSlotsCoeffEncCBOR :: PositiveUnitInterval -> Encoding
activeSlotsCoeffEncCBOR = enforceEncodingVersion shelleyProtVer . encCBOR . unboundRational

-- | Deserialize `PositiveUnitInterval` type using `Rational` deserialization and fail
-- when bounds are violated. Also, ensure there is no usage of tag 30 by enforcing Shelley
-- protocol version.
activeSlotsCoeffDecCBOR :: Decoder s PositiveUnitInterval
activeSlotsCoeffDecCBOR = do
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
  ShelleyGenesis (EraCrypto era) ->
  UTxO era
genesisUTxO genesis =
  UTxO $
    Map.fromList
      [ (txIn, txOut)
      | (addr, amount) <- LM.unListMap (sgInitialFunds genesis)
      , let txIn = initialFundsPseudoTxIn addr
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
initialFundsPseudoTxIn :: forall c. Crypto c => Addr c -> TxIn c
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
    [ "Epoch length is too low. Your epoch length of "
    , Text.pack (show es)
    , " does not meet the minimum epoch length of "
    , Text.pack (show minEpochSize)
    , " required by your choice of parameters for k and f: "
    , Text.pack (show secParam)
    , " and "
    , Text.pack (show asc)
    , ". Epochs should be at least 10k/f slots long."
    ]
describeValidationErr (MaxKESEvolutionsUnsupported reqKES supportedKES) =
  mconcat
    [ "You have specified a 'maxKESEvolutions' higher"
    , " than that supported by the underlying algorithm."
    , " You requested "
    , Text.pack (show reqKES)
    , " but the algorithm supports a maximum of "
    , Text.pack (show supportedKES)
    ]
describeValidationErr (QuorumTooSmall q maxTooSmal nodes) =
  mconcat
    [ "You have specified an 'updateQuorum' which is"
    , " too small compared to the number of genesis nodes."
    , " You requested "
    , Text.pack (show q)
    , ", but given "
    , Text.pack (show nodes)
    , " genesis nodes 'updateQuorum' must be greater than "
    , Text.pack (show maxTooSmal)
    ]

-- | Do some basic sanity checking on the Shelley genesis file.
validateGenesis ::
  forall c.
  Crypto c =>
  ShelleyGenesis c ->
  Either [ValidationErr] ()
validateGenesis
  ShelleyGenesis
    { sgEpochLength
    , sgActiveSlotsCoeff
    , sgMaxKESEvolutions
    , sgSecurityParam
    , sgUpdateQuorum
    , sgGenDelegs
    } =
    case catMaybes errors of
      [] -> Right ()
      xs -> Left xs
    where
      errors =
        [ checkEpochLength
        , checkKesEvolutions
        , checkQuorumSize
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
          <= fromIntegral (totalPeriodsKES (Proxy @(KES c)))
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

{-------------------------------------------------------------------------------
  Construct 'Globals' using 'ShelleyGenesis'
-------------------------------------------------------------------------------}

mkShelleyGlobals ::
  ShelleyGenesis c ->
  EpochInfo (Either Text) ->
  Globals
mkShelleyGlobals genesis epochInfoAc =
  Globals
    { activeSlotCoeff = sgActiveSlotCoeff genesis
    , epochInfo = epochInfoAc
    , maxKESEvo = sgMaxKESEvolutions genesis
    , maxLovelaceSupply = sgMaxLovelaceSupply genesis
    , networkId = sgNetworkId genesis
    , quorum = sgUpdateQuorum genesis
    , randomnessStabilisationWindow
    , securityParameter = k
    , slotsPerKESPeriod = sgSlotsPerKESPeriod genesis
    , stabilityWindow
    , systemStart
    }
  where
    systemStart = SystemStart $ sgSystemStart genesis
    k = sgSecurityParam genesis
    stabilityWindow =
      computeStabilityWindow k (sgActiveSlotCoeff genesis)
    randomnessStabilisationWindow =
      computeRandomnessStabilisationWindow k (sgActiveSlotCoeff genesis)
