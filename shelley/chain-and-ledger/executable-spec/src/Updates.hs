{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Updates
  ( Ppm(..)
  , PPUpdateEnv(..)
  , PPUpdate(..)
  , PParamsUpdate(..)
  , ApName
  , apName
  , ApVer(..)
  , Mdt(..)
  , SystemTag
  , systemTag
  , InstallerHash(..)
  , Applications(..)
  , AVUpdate(..)
  , Update(..)
  , UpdateState(..)
  , Favs
  , allNames
  , maxVer
  , newAVs
  , votedValue
  , emptyUpdateState
  , emptyUpdate
  , updateNull
  , updatePParams
  , svCanFollow
  )
where

import           Cardano.Binary (DecoderError (..), Encoding, FromCBOR (..), ToCBOR (..),
                     decodeMapLenOrIndef, encodeListLen, encodeMapLen, enforceSize)
import           Cardano.Crypto.Hash (Hash)
import           Cardano.Ledger.Shelley.Crypto
import           Cardano.Prelude (NoUnexpectedThunks (..), cborError)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.List as List (group)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)
import           Data.Word (Word8)
import           GHC.Generics (Generic)

import           BaseTypes (Nonce, UnitInterval)
import           Coin (Coin)
import           Keys (GenDelegs, GenKeyHash)
import           PParams (PParams (..))
import           Serialization (CBORMap (..), decodeCollection)
import           Slot (EpochNo (..), SlotNo)

import           Numeric.Natural (Natural)

import           Ledger.Core (dom, range, (◁))
import           Value

newtype ApVer = ApVer Natural
  deriving (Show, Ord, Eq, FromCBOR, ToCBOR, NoUnexpectedThunks)

newtype ApName = ApName T.Text
  deriving (Show, Ord, Eq, ToCBOR, NoUnexpectedThunks)

textSize :: T.Text -> Int
textSize = BS.length . encodeUtf8

apName :: T.Text -> ApName
apName t =
  let numBytes = textSize t
  in
    if numBytes <= 64
      then ApName t
      else error $ "apName received too many bytes (expected 64): " <> show numBytes

instance FromCBOR ApName
 where
  fromCBOR = do
    t <- fromCBOR
    if textSize t > 64
      then cborError $ DecoderErrorCustom "ApName has too many bytes:" t
      else pure $ ApName t

newtype SystemTag = SystemTag T.Text
  deriving (Show, Ord, Eq, ToCBOR, NoUnexpectedThunks)

systemTag :: T.Text -> SystemTag
systemTag t =
  let numBytes = (BS.length . encodeUtf8) t
  in
    if numBytes <= 64
      then SystemTag t
      else error $ "systemTag received too many bytes (expected 64): " <> show numBytes

instance FromCBOR SystemTag
 where
  fromCBOR = do
    t <- fromCBOR
    if textSize t > 64
      then cborError $ DecoderErrorCustom "SystemTag has too many bytes:" t
      else pure $ SystemTag t

newtype InstallerHash crypto = InstallerHash (Hash (HASH crypto) ByteString)
  deriving (Show, Ord, Eq, NoUnexpectedThunks)

deriving instance Crypto crypto => ToCBOR (InstallerHash crypto)
deriving instance Crypto crypto => FromCBOR (InstallerHash crypto)

newtype Mdt crypto = Mdt (Map SystemTag (InstallerHash crypto))
  deriving (Show, Ord, Eq, NoUnexpectedThunks)

instance Crypto crypto => ToCBOR (Mdt crypto) where
  toCBOR (Mdt m) = toCBOR (CBORMap m)

instance Crypto crypto => FromCBOR (Mdt crypto) where
  fromCBOR = Mdt .  unwrapCBORMap <$> fromCBOR


-- | List of applications on the blockchain and their versions.
newtype Applications crypto = Applications {
  apps :: Map ApName (ApVer, Mdt crypto)
  } deriving (Show, Ord, Eq, NoUnexpectedThunks)

instance Crypto crypto => ToCBOR (Applications crypto) where
  toCBOR (Applications m) = toCBOR (CBORMap m)

instance Crypto crypto => FromCBOR (Applications crypto) where
  fromCBOR = Applications .  unwrapCBORMap <$> fromCBOR

-- | A single update of the @Applications list.
newtype AVUpdate crypto = AVUpdate {
  aup :: Map (GenKeyHash crypto) (Applications crypto)
  } deriving (Show, Eq, NoUnexpectedThunks)

instance Crypto crypto => ToCBOR (AVUpdate crypto) where
  toCBOR (AVUpdate m) = toCBOR (CBORMap m)

instance Crypto crypto => FromCBOR (AVUpdate crypto) where
  fromCBOR = AVUpdate .  unwrapCBORMap <$> fromCBOR

-- | Update Proposal
data Update crypto
  = Update (PPUpdate crypto) (AVUpdate crypto) (Maybe EpochNo)
  deriving (Show, Eq, Generic)

updateNull :: Update crypto -> Bool
updateNull (Update (PPUpdate ppup) (AVUpdate avup) _e) = null ppup && null avup

instance NoUnexpectedThunks (Update crypto)

instance Crypto crypto => ToCBOR (Update crypto) where
  toCBOR (Update ppUpdate avUpdate e) =
    encodeListLen 3 <> toCBOR ppUpdate <> toCBOR avUpdate <> toCBOR e

instance Crypto crypto => FromCBOR (Update crypto) where
  fromCBOR =
    Update <$ enforceSize "Update" 3
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR

data PPUpdateEnv crypto = PPUpdateEnv {
    slotNo :: SlotNo
  , genDelegs  :: GenDelegs crypto
  } deriving (Show, Eq, Generic)

instance NoUnexpectedThunks (PPUpdateEnv crypto)

-- | Protocol parameter selector, contains just a single field to be updated
--   within @PPUpdate
data Ppm crypto = MinFeeA Integer
  | MinFeeB Natural
  | MaxBBSize Natural
  | MaxTxSize Natural
  | MaxBHSize Natural
  | KeyDeposit (Value crypto)
  | KeyMinRefund UnitInterval
  | KeyDecayRate Rational
  | PoolDeposit (Value crypto)
  | PoolMinRefund UnitInterval
  | PoolDecayRate Rational
  | EMax EpochNo
  | Nopt Natural
  | A0 Rational
  | Rho UnitInterval
  | Tau UnitInterval
  | ActiveSlotCoefficient UnitInterval
  | D UnitInterval
  | ExtraEntropy Nonce
  | ProtocolVersion (Natural, Natural)
  deriving (Show, Ord, Eq, Generic)

instance NoUnexpectedThunks (Ppm crypto)

newtype PParamsUpdate crypto = PParamsUpdate { ppmSet :: Set (Ppm crypto) }
  deriving (Show, Eq, Generic, Ord)

instance NoUnexpectedThunks (PParamsUpdate crypto)

instance (Crypto crypto) => ToCBOR (PParamsUpdate crypto) where
  toCBOR (PParamsUpdate ppms) = encodeMapLen l <> foldMap f ppms
    where
      l = fromIntegral $ Set.size ppms
      word :: Word8 -> Encoding
      word = toCBOR
      f = \case
        MinFeeA x               -> word  0 <> toCBOR x
        MinFeeB x               -> word  1 <> toCBOR x
        MaxBBSize x             -> word  2 <> toCBOR x
        MaxTxSize x             -> word  3 <> toCBOR x
        MaxBHSize x             -> word  4 <> toCBOR x
        KeyDeposit x            -> word  5 <> toCBOR x
        KeyMinRefund x          -> word  6 <> toCBOR x
        KeyDecayRate x          -> word  7 <> toCBOR x
        PoolDeposit x           -> word  8 <> toCBOR x
        PoolMinRefund x         -> word  9 <> toCBOR x
        PoolDecayRate x         -> word 10 <> toCBOR x
        EMax x                  -> word 11 <> toCBOR x
        Nopt x                  -> word 12 <> toCBOR x
        A0   x                  -> word 13 <> toCBOR x
        Rho  x                  -> word 14 <> toCBOR x
        Tau  x                  -> word 15 <> toCBOR x
        ActiveSlotCoefficient x -> word 16 <> toCBOR x
        D x                     -> word 17 <> toCBOR x
        ExtraEntropy x          -> word 18 <> toCBOR x
        ProtocolVersion x       -> word 19 <> toCBOR x

instance FromCBOR PParamsUpdate where
  fromCBOR = fmap (PParamsUpdate . Set.fromList)
    $ decodeCollection decodeMapLenOrIndef
    $ fromCBOR @Word8 >>= \case
         0  -> MinFeeA <$> fromCBOR
         1  -> MinFeeB <$> fromCBOR
         2  -> MaxBBSize <$> fromCBOR
         3  -> MaxTxSize <$> fromCBOR
         4  -> MaxBHSize <$> fromCBOR
         5  -> KeyDeposit <$> fromCBOR
         6  -> KeyMinRefund <$> fromCBOR
         7  -> KeyDecayRate <$> fromCBOR
         8  -> PoolDeposit <$> fromCBOR
         9  -> PoolMinRefund <$> fromCBOR
         10 -> PoolDecayRate <$> fromCBOR
         11 -> (EMax . EpochNo) <$> fromCBOR
         12 -> Nopt <$> fromCBOR
         13 -> A0   <$> fromCBOR
         14 -> Rho  <$> fromCBOR
         15 -> Tau  <$> fromCBOR
         16 -> ActiveSlotCoefficient <$> fromCBOR
         17 -> D <$> fromCBOR
         18 -> ExtraEntropy <$> fromCBOR
         19 -> ProtocolVersion <$> fromCBOR
         k -> fail $ "not a valid key: " ++ show k

-- | Update operation for protocol parameters structure @PParams
newtype PPUpdate crypto
  = PPUpdate (Map (GenKeyHash crypto) PParamsUpdate)
  deriving (Show, Eq, NoUnexpectedThunks)

-- | This is just an example and not neccessarily how we will actually validate names
apNameValid :: ApName -> Bool
apNameValid (ApName an) = all isAscii cs && length cs <= 12
  where cs = unpack an

instance Crypto crypto => ToCBOR (PPUpdate crypto) where
  toCBOR (PPUpdate m) = toCBOR (CBORMap m)

instance Crypto crypto => FromCBOR (PPUpdate crypto) where
  fromCBOR = PPUpdate .  unwrapCBORMap <$> fromCBOR

type Favs crypto = Map SlotNo (Applications crypto)

maxVer :: ApName -> Applications crypto -> Favs crypto -> (ApVer, Mdt crypto)
maxVer an avs favs =
  maximum $ vs an avs : fmap (vs an) (Set.toList (range favs))
    where
      vs n (Applications as) =
        Map.foldr max (ApVer 0, Mdt Map.empty) (Set.singleton n ◁ as)

svCanFollow :: Applications crypto -> Favs crypto -> (ApName, (ApVer, Mdt crypto)) -> Bool
svCanFollow avs favs (an, (ApVer v, _)) = v == 1 + m
  where (ApVer m) = fst $ maxVer an avs favs

allNames :: Applications crypto -> Favs crypto -> Set ApName
allNames (Applications avs) favs =
  Prelude.foldr
    (\(Applications fav) acc -> acc `Set.union` Map.keysSet fav)
    (Map.keysSet avs)
    (Map.elems favs)

newAVs :: Applications crypto -> Favs crypto -> Applications crypto
newAVs avs favs = Applications $
                    Map.fromList [(an, maxVer an avs favs) | an <- Set.toList $ allNames avs favs]

votedValue
  :: Eq a => Map (GenKeyHash crypto) a -> Int -> Maybe a
votedValue vs quorum | null elemLists = Nothing
                     | otherwise      = Just $ (head . head) elemLists  -- elemLists contains an element
                                               -- and that list contains at
                                               -- least 5 elements


 where
  elemLists =
    filter (\l -> length l >= quorum) $ List.group $ map snd $ Map.toList vs

emptyUpdateState :: UpdateState crypto
emptyUpdateState = UpdateState
                     (PPUpdate Map.empty)
                     (AVUpdate Map.empty)
                     Map.empty
                     (Applications Map.empty)

emptyUpdate :: Update crypto
emptyUpdate = Update (PPUpdate Map.empty) (AVUpdate Map.empty) Nothing

updatePParams :: PParams crypto -> (PParamsUpdate crypto) -> PParams crypto
updatePParams ppms (PParamsUpdate up) = Set.foldr updatePParams' ppms up
 where
  updatePParams' (MinFeeA               p) pps = pps { _minfeeA = p }
  updatePParams' (MinFeeB               p) pps = pps { _minfeeB = p }
  updatePParams' (MaxBBSize             p) pps = pps { _maxBBSize = p }
  updatePParams' (MaxTxSize             p) pps = pps { _maxTxSize = p }
  updatePParams' (MaxBHSize             p) pps = pps { _maxBHSize = p }
  updatePParams' (KeyDeposit            p) pps = pps { _keyDeposit = p }
  updatePParams' (KeyMinRefund          p) pps = pps { _keyMinRefund = p }
  updatePParams' (KeyDecayRate          p) pps = pps { _keyDecayRate = p }
  updatePParams' (PoolDeposit           p) pps = pps { _poolDeposit = p }
  updatePParams' (PoolMinRefund         p) pps = pps { _poolMinRefund = p }
  updatePParams' (PoolDecayRate         p) pps = pps { _poolDecayRate = p }
  updatePParams' (EMax                  p) pps = pps { _eMax = p }
  updatePParams' (Nopt                  p) pps = pps { _nOpt = p }
  updatePParams' (A0                    p) pps = pps { _a0 = p }
  updatePParams' (Rho                   p) pps = pps { _rho = p }
  updatePParams' (Tau                   p) pps = pps { _tau = p }
  updatePParams' (ActiveSlotCoefficient p) pps = pps { _activeSlotCoeff = p }
  updatePParams' (D                     p) pps = pps { _d = p }
  updatePParams' (ExtraEntropy          p) pps = pps { _extraEntropy = p }
  updatePParams' (ProtocolVersion       p) pps = pps { _protocolVersion = p }

data UpdateState crypto
  = UpdateState
      (PPUpdate crypto)
      (AVUpdate crypto)
      (Map SlotNo (Applications crypto))
      (Applications crypto)
  deriving (Show, Eq, Generic)

instance NoUnexpectedThunks (UpdateState crypto)

instance Crypto crypto => ToCBOR (UpdateState crypto)
 where
  toCBOR (UpdateState a b c d) =
    encodeListLen 4 <> toCBOR a <> toCBOR b <> toCBOR c <> toCBOR d

instance Crypto crypto => FromCBOR (UpdateState crypto)
 where
  fromCBOR = do
    enforceSize "UpdateState" 4
    a <- fromCBOR
    b <- fromCBOR
    c <- fromCBOR
    d <- fromCBOR
    pure $ UpdateState a b c d
