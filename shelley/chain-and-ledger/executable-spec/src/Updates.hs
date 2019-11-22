{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}

module Updates
  ( Ppm(..)
  , PPUpdateEnv(..)
  , PPUpdate(..)
  , ApName(..)
  , ApVer(..)
  , Mdt(..)
  , SystemTag(..)
  , InstallerHash(..)
  , Applications(..)
  , AVUpdate(..)
  , Update(..)
  , UpdateState(..)
  , Favs
  , apNameValid
  , allNames
  , newAVs
  , votedValue
  , emptyUpdateState
  , emptyUpdate
  , updateNull
  , updatePParams
  , svCanFollow
  , sTagsValid
  )
where

import           Cardano.Binary (ToCBOR (toCBOR), encodeListLen)
import           Cardano.Crypto.Hash (Hash)
import           Cardano.Ledger.Shelley.Crypto
import           Cardano.Prelude (NoUnexpectedThunks (..))
import           Data.ByteString (ByteString)
import           Data.ByteString.Char8 (unpack)
import           Data.Char (isAscii)
import qualified Data.List as List (group)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Word (Word8)
import           GHC.Generics (Generic)

import           BaseTypes (Nonce, UnitInterval)
import           Coin (Coin)
import           Keys (GenDelegs, GenKeyHash)
import           PParams (PParams (..))
import           Slot (Epoch, Slot)

import           Numeric.Natural (Natural)

import           Ledger.Core (dom, range, (◁))

newtype ApVer = ApVer Natural
  deriving (Show, Ord, Eq, NoUnexpectedThunks, ToCBOR)

newtype ApName = ApName ByteString
  deriving (Show, Ord, Eq, ToCBOR, NoUnexpectedThunks)

newtype SystemTag = SystemTag ByteString
  deriving (Show, Ord, Eq, ToCBOR, NoUnexpectedThunks)

newtype InstallerHash crypto = InstallerHash (Hash (HASH crypto) ByteString)
  deriving (Show, Ord, Eq, NoUnexpectedThunks)

deriving instance Crypto crypto => ToCBOR (InstallerHash crypto)

newtype Mdt crypto = Mdt (Map SystemTag (InstallerHash crypto))
  deriving (Show, Ord, Eq, NoUnexpectedThunks)

deriving instance Crypto crypto => ToCBOR (Mdt crypto)

-- | List of applications on the blockchain and their versions.
newtype Applications crypto = Applications {
  apps :: Map ApName (ApVer, Mdt crypto)
  } deriving (Show, Ord, Eq, ToCBOR, NoUnexpectedThunks)

-- | A single update of the @Applications list.
newtype AVUpdate crypto = AVUpdate {
  aup :: Map (GenKeyHash crypto) (Applications crypto)
  } deriving (Show, Eq, ToCBOR, NoUnexpectedThunks)

-- | Update Proposal
data Update crypto
  = Update (PPUpdate crypto) (AVUpdate crypto)
  deriving (Show, Eq, Generic)

updateNull :: Update crypto -> Bool
updateNull (Update (PPUpdate ppup) (AVUpdate avup)) = null ppup && null avup

instance NoUnexpectedThunks (Update crypto)

instance Crypto crypto => ToCBOR (Update crypto) where
  toCBOR (Update ppUpdate avUpdate) =
    encodeListLen 2 <> toCBOR ppUpdate <> toCBOR avUpdate

data PPUpdateEnv crypto = PPUpdateEnv {
    slot :: Slot
  , genDelegs  :: GenDelegs crypto
  } deriving (Show, Eq, Generic)

instance NoUnexpectedThunks (PPUpdateEnv crypto)

-- | Protocol parameter selector, contains just a single field to be updated
--   within @PPUpdate
data Ppm = MinFeeA Integer
  | MinFeeB Natural
  | MaxBBSize Natural
  | MaxTxSize Natural
  | KeyDeposit Coin
  | KeyMinRefund UnitInterval
  | KeyDecayRate Rational
  | PoolDeposit Coin
  | PoolMinRefund UnitInterval
  | PoolDecayRate Rational
  | EMax Epoch
  | Nopt Natural
  | A0 Rational
  | Rho UnitInterval
  | Tau UnitInterval
  | ActiveSlotCoefficient UnitInterval
  | D UnitInterval
  | ExtraEntropy Nonce
  | ProtocolVersion (Natural, Natural, Natural)
  deriving (Show, Ord, Eq, Generic)

instance NoUnexpectedThunks Ppm

instance ToCBOR Ppm where
  toCBOR = \case
    MinFeeA a -> encodeListLen 2 <> toCBOR (0 :: Word8) <> toCBOR a

    MinFeeB b -> encodeListLen 2 <> toCBOR (1 :: Word8) <> toCBOR b

    MaxBBSize maxBBSize ->
      encodeListLen 2 <> toCBOR (2 :: Word8) <> toCBOR maxBBSize

    MaxTxSize maxTxSize ->
      encodeListLen 2 <> toCBOR (3 :: Word8) <> toCBOR maxTxSize

    KeyDeposit keyDeposit ->
      encodeListLen 2 <> toCBOR (4 :: Word8) <> toCBOR keyDeposit

    KeyMinRefund keyMinRefund ->
      encodeListLen 2 <> toCBOR (5 :: Word8) <> toCBOR keyMinRefund

    KeyDecayRate keyDecayRate ->
      encodeListLen 2 <> toCBOR (6 :: Word8) <> toCBOR keyDecayRate

    PoolDeposit poolDeposit ->
      encodeListLen 2 <> toCBOR (7 :: Word8) <> toCBOR poolDeposit

    PoolMinRefund poolMinRefund ->
      encodeListLen 2 <> toCBOR (8 :: Word8) <> toCBOR poolMinRefund

    PoolDecayRate poolDecayRate ->
      encodeListLen 2 <> toCBOR (9 :: Word8) <> toCBOR poolDecayRate

    EMax eMax -> encodeListLen 2 <> toCBOR (10 :: Word8) <> toCBOR eMax

    Nopt nopt -> encodeListLen 2 <> toCBOR (11 :: Word8) <> toCBOR nopt

    A0   a0   -> encodeListLen 2 <> toCBOR (12 :: Word8) <> toCBOR a0

    Rho  rho  -> encodeListLen 2 <> toCBOR (13 :: Word8) <> toCBOR rho

    Tau  tau  -> encodeListLen 2 <> toCBOR (14 :: Word8) <> toCBOR tau

    ActiveSlotCoefficient activeSlotCoeff ->
      encodeListLen 2 <> toCBOR (15 :: Word8) <> toCBOR activeSlotCoeff

    D d -> encodeListLen 2 <> toCBOR (16 :: Word8) <> toCBOR d

    ExtraEntropy extraEntropy ->
      encodeListLen 2 <> toCBOR (17 :: Word8) <> toCBOR extraEntropy

    ProtocolVersion protocolVersion ->
      encodeListLen 2 <> toCBOR (18 :: Word8) <> toCBOR protocolVersion

-- | Update operation for protocol parameters structure @PParams
newtype PPUpdate crypto
  = PPUpdate (Map (GenKeyHash crypto) (Set Ppm))
  deriving (Show, Eq, ToCBOR, NoUnexpectedThunks)

-- | This is just an example and not neccessarily how we will actually validate names
apNameValid :: ApName -> Bool
apNameValid (ApName an) = all isAscii cs && length cs <= 12
  where cs = unpack an

-- | This is just an example and not neccessarily how we will actually validate system tags
sTagValid :: SystemTag -> Bool
sTagValid (SystemTag st) = all isAscii cs && length cs <= 10
  where cs = unpack st

sTagsValid :: Mdt crypto -> Bool
sTagsValid (Mdt md) = all sTagValid (dom md)

type Favs crypto = Map Slot (Applications crypto)

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
  :: Eq a => Map (GenKeyHash crypto) a -> Maybe a
votedValue vs | null elemLists = Nothing
              | otherwise      = Just $ (head . head) elemLists  -- elemLists contains an element
                                               -- and that list contains at
                                               -- least 5 elements


 where
  elemLists =
    filter (\l -> length l >= 5) $ List.group $ map snd $ Map.toList vs

emptyUpdateState :: UpdateState crypto
emptyUpdateState = UpdateState
                     (PPUpdate Map.empty)
                     (AVUpdate Map.empty)
                     Map.empty
                     (Applications Map.empty)

emptyUpdate :: Update crypto
emptyUpdate = Update (PPUpdate Map.empty) (AVUpdate Map.empty)

updatePParams :: PParams -> Set Ppm -> PParams
updatePParams = Set.foldr updatePParams'
 where
  updatePParams' (MinFeeA               p) pps = pps { _minfeeA = p }
  updatePParams' (MinFeeB               p) pps = pps { _minfeeB = p }
  updatePParams' (MaxBBSize             p) pps = pps { _maxBBSize = p }
  updatePParams' (MaxTxSize             p) pps = pps { _maxTxSize = p }
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
      (Map Slot (Applications crypto))
      (Applications crypto)
  deriving (Show, Eq, Generic)

instance NoUnexpectedThunks (UpdateState crypto)
