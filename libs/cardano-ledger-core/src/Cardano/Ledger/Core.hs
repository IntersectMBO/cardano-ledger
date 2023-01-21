{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-redundant-constraints #-}

-- | This module defines core type families which we know to vary from era to
-- era.
--
-- Families in this module should be indexed on era.
--
-- It is intended for qualified import:
-- > import qualified Cardano.Ledger.Core as Core
module Cardano.Ledger.Core (
  -- * Era-changing types
  EraTx (..),
  EraTxOut (..),
  bootAddrTxOutF,
  coinTxOutL,
  compactCoinTxOutL,
  isAdaOnlyTxOutF,
  EraTxBody (..),
  EraTxAuxData (..),
  EraTxWits (..),
  EraScript (..),
  Value,
  EraPParams (..),

  -- * Era STS
  EraRule,
  Era (..),
  -- $segWit
  EraSegWits (..),

  -- * Phases
  Phase (..),
  PhaseRep (..),
  SomeScript,
  PhasedScript (..),
  getPhase1,
  getPhase2,

  -- * Rewards
  RewardType (..),
  Reward (..),

  -- * Re-exports
  module Cardano.Ledger.Hashes,
  module Cardano.Ledger.Core.Era,
  module Cardano.Ledger.Core.PParams,
  module Cardano.Ledger.Core.Translation,

  -- * Deprecations
  hashAuxiliaryData,
  validateAuxiliaryData,
)
where

import qualified Cardano.Crypto.Hash as Hash
import Cardano.Ledger.Address (
  Addr (..),
  BootstrapAddress,
  CompactAddr,
  Withdrawals,
  compactAddr,
  decompactAddr,
  isBootstrapCompactAddr,
 )
import Cardano.Ledger.AuxiliaryData (AuxiliaryDataHash)
import Cardano.Ledger.BaseTypes (ProtVer (..))
import Cardano.Ledger.Binary (
  Annotator,
  EncCBOR,
  FromCBOR,
  FromSharedCBOR (Share),
  Interns,
  Sized (sizedValue),
  ToCBOR,
  ToCBORGroup,
  mkSized,
 )
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Compactible (Compactible (..))
import Cardano.Ledger.Core.Era
import Cardano.Ledger.Core.PParams
import Cardano.Ledger.Core.Translation
import Cardano.Ledger.Credential (Credential)
import qualified Cardano.Ledger.Crypto as CC
import Cardano.Ledger.Hashes
import Cardano.Ledger.Keys (KeyRole (Staking, Witness))
import Cardano.Ledger.Keys.Bootstrap (BootstrapWitness)
import Cardano.Ledger.Keys.WitVKey (WitVKey)
import Cardano.Ledger.Language (Language)
import Cardano.Ledger.Rewards (Reward (..), RewardType (..))
import Cardano.Ledger.SafeHash (HashAnnotated (..), SafeToHash (..))
import Cardano.Ledger.TxIn (TxIn (..))
import Cardano.Ledger.Val (DecodeNonNegative, Val (..))
import Control.DeepSeq (NFData)
import qualified Data.ByteString as BS
import Data.ByteString.Short (ShortByteString)
import Data.Kind (Type)
import Data.Map (Map, mapMaybe)
import Data.Maybe (fromMaybe)
import Data.Maybe.Strict (StrictMaybe)
import Data.Sequence.Strict (StrictSeq)
import Data.Set (Set)
import Data.Word (Word64)
import GHC.TypeLits (Symbol)
import Lens.Micro
import NoThunks.Class (NoThunks)

-- | A transaction.
class
  ( EraTxBody era
  , EraTxWits era
  , EraTxAuxData era
  , EraPParams era
  , -- NFData (Tx era), TODO: Add NFData constraints to Crypto class
    NoThunks (Tx era)
  , FromCBOR (Annotator (Tx era))
  , ToCBOR (Tx era)
  , Show (Tx era)
  , Eq (Tx era)
  ) =>
  EraTx era
  where
  type Tx era = (r :: Type) | r -> era

  mkBasicTx :: TxBody era -> Tx era

  bodyTxL :: Lens' (Tx era) (TxBody era)

  witsTxL :: Lens' (Tx era) (TxWits era)

  auxDataTxL :: Lens' (Tx era) (StrictMaybe (AuxiliaryData era))

  sizeTxF :: SimpleGetter (Tx era) Integer

  validateScript :: PhasedScript 'PhaseOne era -> Tx era -> Bool

  getMinFeeTx :: PParams era -> Tx era -> Coin

class
  ( EraTxOut era
  , EraPParams era
  , HashAnnotated (TxBody era) EraIndependentTxBody (EraCrypto era)
  , FromCBOR (Annotator (TxBody era))
  , EncCBOR (TxBody era)
  , NoThunks (TxBody era)
  , NFData (TxBody era)
  , Show (TxBody era)
  , Eq (TxBody era)
  ) =>
  EraTxBody era
  where
  -- | The body of a transaction.
  type TxBody era = (r :: Type) | r -> era

  mkBasicTxBody :: TxBody era

  inputsTxBodyL :: Lens' (TxBody era) (Set (TxIn (EraCrypto era)))

  outputsTxBodyL :: Lens' (TxBody era) (StrictSeq (TxOut era))

  feeTxBodyL :: Lens' (TxBody era) Coin

  withdrawalsTxBodyL :: Lens' (TxBody era) (Withdrawals (EraCrypto era))

  auxDataHashTxBodyL :: Lens' (TxBody era) (StrictMaybe (AuxiliaryDataHash (EraCrypto era)))

  allInputsTxBodyF :: SimpleGetter (TxBody era) (Set (TxIn (EraCrypto era)))

-- | Abstract interface into specific fields of a `TxOut`
class
  ( DecodeNonNegative (Value era)
  , Compactible (Value era)
  , NFData (Value era)
  , Show (Value era)
  , Val (Value era)
  , Eq (Value era)
  , FromCBOR (Value era)
  , ToCBOR (Value era)
  , FromCBOR (TxOut era)
  , FromSharedCBOR (TxOut era)
  , Share (TxOut era) ~ Interns (Credential 'Staking (EraCrypto era))
  , ToCBOR (TxOut era)
  , NoThunks (TxOut era)
  , NFData (TxOut era)
  , Show (TxOut era)
  , Eq (TxOut era)
  , EraPParams era
  ) =>
  EraTxOut era
  where
  -- | The output of a UTxO for a particular era
  type TxOut era = (r :: Type) | r -> era

  {-# MINIMAL
    mkBasicTxOut
    , valueEitherTxOutL
    , addrEitherTxOutL
    , (getMinCoinSizedTxOut | getMinCoinTxOut)
    #-}

  mkBasicTxOut :: Addr (EraCrypto era) -> Value era -> TxOut era

  valueTxOutL :: Lens' (TxOut era) (Value era)
  valueTxOutL =
    lens
      ( \txOut -> case txOut ^. valueEitherTxOutL of
          Left value -> value
          Right cValue -> fromCompact cValue
      )
      (\txOut value -> txOut & valueEitherTxOutL .~ Left value)
  {-# INLINE valueTxOutL #-}

  compactValueTxOutL :: Lens' (TxOut era) (CompactForm (Value era))
  compactValueTxOutL =
    lens
      ( \txOut -> case txOut ^. valueEitherTxOutL of
          Left value -> toCompactPartial value
          Right cValue -> cValue
      )
      (\txOut cValue -> txOut & valueEitherTxOutL .~ Right cValue)
  {-# INLINE compactValueTxOutL #-}

  -- | Lens for getting and setting in TxOut either an address or its compact
  -- version by doing the least amount of work.
  valueEitherTxOutL :: Lens' (TxOut era) (Either (Value era) (CompactForm (Value era)))

  addrTxOutL :: Lens' (TxOut era) (Addr (EraCrypto era))
  addrTxOutL =
    lens
      ( \txOut -> case txOut ^. addrEitherTxOutL of
          Left addr -> addr
          Right cAddr -> decompactAddr cAddr
      )
      (\txOut addr -> txOut & addrEitherTxOutL .~ Left addr)
  {-# INLINE addrTxOutL #-}

  compactAddrTxOutL :: Lens' (TxOut era) (CompactAddr (EraCrypto era))
  compactAddrTxOutL =
    lens
      ( \txOut -> case txOut ^. addrEitherTxOutL of
          Left addr -> compactAddr addr
          Right cAddr -> cAddr
      )
      (\txOut cAddr -> txOut & addrEitherTxOutL .~ Right cAddr)
  {-# INLINE compactAddrTxOutL #-}

  -- | Lens for getting and setting in TxOut either an address or its compact
  -- version by doing the least amount of work.
  --
  -- The utility of this function comes from the fact that TxOut usually stores
  -- the address in either one of two forms: compacted or unpacked. In order to
  -- avoid extroneous conversions in `getTxOutAddr` and `getTxOutCompactAddr` we
  -- can define just this functionality. Also sometimes it crutial to know at
  -- the callsite which form of address we have readily available without any
  -- conversions (eg. searching millions of TxOuts for a particular address)
  addrEitherTxOutL :: Lens' (TxOut era) (Either (Addr (EraCrypto era)) (CompactAddr (EraCrypto era)))

  -- | Produce the minimum lovelace that a given transaction output must
  -- contain. Information about the size of the TxOut is required in some eras.
  -- Use `getMinCoinTxOut` if you don't have the size readily available to you.
  getMinCoinSizedTxOut :: PParams era -> Sized (TxOut era) -> Coin
  getMinCoinSizedTxOut pp = getMinCoinTxOut pp . sizedValue

  -- | Same as `getMinCoinSizedTxOut`, except information about the size of
  -- TxOut will be computed by serializing the TxOut. If the size turns out to
  -- be not needed, then serialization will have no overhead, since it is
  -- computed lazily.
  getMinCoinTxOut :: PParams era -> TxOut era -> Coin
  getMinCoinTxOut pp txOut =
    let ProtVer version _ = pp ^. ppProtocolVersionL
     in getMinCoinSizedTxOut pp (mkSized version txOut)

bootAddrTxOutF :: EraTxOut era => SimpleGetter (TxOut era) (Maybe (BootstrapAddress (EraCrypto era)))
bootAddrTxOutF = to $ \txOut ->
  case txOut ^. addrEitherTxOutL of
    Left (AddrBootstrap bootstrapAddr) -> Just bootstrapAddr
    Right cAddr
      | isBootstrapCompactAddr cAddr -> do
          AddrBootstrap bootstrapAddr <- Just (decompactAddr cAddr)
          Just bootstrapAddr
    _ -> Nothing
{-# INLINE bootAddrTxOutF #-}

coinTxOutL :: EraTxOut era => Lens' (TxOut era) Coin
coinTxOutL =
  lens
    ( \txOut ->
        case txOut ^. valueEitherTxOutL of
          Left val -> coin val
          Right cVal -> fromCompact (coinCompact cVal)
    )
    ( \txOut c ->
        case txOut ^. valueEitherTxOutL of
          Left val -> txOut & valueTxOutL .~ modifyCoin (const c) val
          Right cVal ->
            txOut & compactValueTxOutL .~ modifyCompactCoin (const (toCompactPartial c)) cVal
    )
{-# INLINE coinTxOutL #-}

compactCoinTxOutL :: EraTxOut era => Lens' (TxOut era) (CompactForm Coin)
compactCoinTxOutL =
  lens
    ( \txOut ->
        case txOut ^. valueEitherTxOutL of
          Left val -> toCompactPartial (coin val)
          Right cVal -> coinCompact cVal
    )
    ( \txOut cCoin ->
        case txOut ^. valueEitherTxOutL of
          Left val -> txOut & valueTxOutL .~ modifyCoin (const (fromCompact cCoin)) val
          Right cVal ->
            txOut & compactValueTxOutL .~ modifyCompactCoin (const cCoin) cVal
    )
{-# INLINE compactCoinTxOutL #-}

-- | This is a getter that implements an efficient way to check whether 'TxOut'
-- contains ADA only.
isAdaOnlyTxOutF :: EraTxOut era => SimpleGetter (TxOut era) Bool
isAdaOnlyTxOutF = to $ \txOut ->
  case txOut ^. valueEitherTxOutL of
    Left val -> isAdaOnly val
    Right cVal -> isAdaOnlyCompact cVal

toCompactPartial :: (Val a, Show a) => a -> CompactForm a
toCompactPartial v =
  fromMaybe (error $ "Illegal value in TxOut: " <> show v) $ toCompact v

-- | A value is something which quantifies a transaction output.
type family Value era :: Type

-- | TxAuxData which may be attached to a transaction
class
  ( Era era
  , Eq (TxAuxData era)
  , Show (TxAuxData era)
  , NoThunks (TxAuxData era)
  , EncCBOR (TxAuxData era)
  , FromCBOR (Annotator (TxAuxData era))
  , HashAnnotated (TxAuxData era) EraIndependentTxAuxData (EraCrypto era)
  ) =>
  EraTxAuxData era
  where
  type TxAuxData era = (r :: Type) | r -> era
  hashTxAuxData :: TxAuxData era -> AuxiliaryDataHash (EraCrypto era)
  validateTxAuxData :: ProtVer -> TxAuxData era -> Bool

type AuxiliaryData era = TxAuxData era

{-# DEPRECATED AuxiliaryData "Use `TxAuxData` instead" #-}

hashAuxiliaryData :: EraTxAuxData era => TxAuxData era -> AuxiliaryDataHash (EraCrypto era)
hashAuxiliaryData = hashTxAuxData
{-# DEPRECATED hashAuxiliaryData "Use `hashTxAuxData` instead" #-}

validateAuxiliaryData :: EraTxAuxData era => ProtVer -> TxAuxData era -> Bool
validateAuxiliaryData = validateTxAuxData
{-# DEPRECATED validateAuxiliaryData "Use `validateTxAuxData` instead" #-}

-- | A collection of witnesses in a Tx
class
  ( EraScript era
  , Eq (TxWits era)
  , Show (TxWits era)
  , Monoid (TxWits era)
  , NoThunks (TxWits era)
  , EncCBOR (TxWits era)
  , FromCBOR (Annotator (TxWits era))
  ) =>
  EraTxWits era
  where
  type TxWits era = (r :: Type) | r -> era

  mkBasicTxWits :: TxWits era
  mkBasicTxWits = mempty

  addrTxWitsL :: Lens' (TxWits era) (Set (WitVKey 'Witness (EraCrypto era)))

  bootAddrTxWitsL :: Lens' (TxWits era) (Set (BootstrapWitness (EraCrypto era)))

  scriptTxWitsL :: Lens' (TxWits era) (Map (ScriptHash (EraCrypto era)) (Script era))

-- | Era STS map
type family EraRule (k :: Symbol) era :: Type

-----------------------------------------------------------------------------
-- Script Validation
-----------------------------------------------------------------------------

-- | Typeclass for script data types. Allows for script validation and hashing.
--   You must understand the role of SafeToHash and scriptPrefixTag to make new
--   instances. 'scriptPrefixTag' is a magic number representing the tag of the
--   script language. For each new script language defined, a new tag is chosen
--   and the tag is included in the script hash for a script. The safeToHash
--   constraint ensures that Scripts are never reserialised.
class
  ( Era era
  , Show (Script era)
  , Eq (Script era)
  , EncCBOR (Script era)
  , FromCBOR (Annotator (Script era))
  , NoThunks (Script era)
  , SafeToHash (Script era)
  ) =>
  EraScript era
  where
  -- | Scripts which may lock transaction outputs in this era
  type Script era = (r :: Type) | r -> era

  scriptPrefixTag :: Script era -> BS.ByteString
  hashScript :: Script era -> ScriptHash (EraCrypto era)
  -- ONE SHOULD NOT OVERIDE THE hashScript DEFAULT METHOD
  -- UNLESS YOU UNDERSTAND THE SafeToHash class, AND THE ROLE OF THE scriptPrefixTag
  hashScript =
    ScriptHash
      . Hash.castHash
      . Hash.hashWith
        (\x -> scriptPrefixTag @era x <> originalBytes x)
  phaseScript :: PhaseRep phase -> Script era -> Maybe (PhasedScript phase era)
  isNativeScript :: Script era -> Bool
  isNativeScript s =
    case phaseScript @era PhaseOneRep s of
      Nothing -> False
      Just _ -> True

--------------------------------------------------------------------------------
-- Segregated Witness
--------------------------------------------------------------------------------

-- $segWit
-- * Segregated Witness
--
-- The idea of segregated witnessing is to alter the encoding of transactions in
-- a block such that the witnesses (the information needed to verify the
-- validity of the transactions) can be stored separately from the body (the
-- information needed to update the ledger state). In this way, a node which
-- only cares about replaying transactions need not even decode the witness
-- information.
--
-- In order to do this, we introduce two concepts:
-- - A 'TxSeq`, which represents the decoded structure of a sequence of
--   transactions as represented in the encoded block; that is, with witnessing,
--   metadata and other non-body parts split separately.

-- | Indicates that an era supports segregated witnessing.
--
--   This class embodies an isomorphism between 'TxSeq era' and 'StrictSeq
--   (Tx era)', witnessed by 'fromTxSeq' and 'toTxSeq'.
class
  ( EraTx era
  , Eq (TxSeq era)
  , Show (TxSeq era)
  , ToCBORGroup (TxSeq era)
  , FromCBOR (Annotator (TxSeq era))
  ) =>
  EraSegWits era
  where
  type TxSeq era = (r :: Type) | r -> era

  fromTxSeq :: TxSeq era -> StrictSeq (Tx era)
  toTxSeq :: StrictSeq (Tx era) -> TxSeq era

  -- | Get the block body hash from the TxSeq. Note that this is not a regular
  -- "hash the stored bytes" function since the block body hash forms a small
  -- Merkle tree.
  hashTxSeq ::
    TxSeq era ->
    Hash.Hash (CC.HASH (EraCrypto era)) EraIndependentBlockBody

  -- | The number of segregated components
  numSegComponents :: Word64

-- ====================================================
-- Reflecting the phase of scripts into types
-- ====================================================

-- | There are only two Phases
data Phase
  = PhaseOne -- simple scripts run in phase 1
  | PhaseTwo -- smart scripts (Plutus) run in phase 2

-- | A GADT that witnesses the two Phases
data PhaseRep t where
  PhaseOneRep :: PhaseRep 'PhaseOne
  PhaseTwoRep :: PhaseRep 'PhaseTwo

-- | Type family that defines a script type given a Phase and an Era.
type family SomeScript (phase :: Phase) (era :: Type) :: Type

{- Instance like these appear in modules Cardano.Ledger.{Shelley,ShelleyMA,Alonzo,Babbage}
type instance SomeScript PhaseOne (BabbageEra c) = Timelock c
type instance SomeScript PhaseOne (AlonzoEra c) = Timelock c
--  We need the Language for Phase2 scripts because Plutus comes in several variants
type instance SomeScript PhaseTwo (BabbageEra c) = (Language, ShortByteString)
type instance SomeScript PhaseTwo (AlonzoEra c) = (Language, ShortByteString)
type instance SomeScript PhaseOne (ShelleyMAEra ma c) = Timelock c
type instance SomeScript PhaseOne (ShelleyEra c) = MultiSig c
-}

-- | A concrete type that witnesses the Phase of SomeScript
data PhasedScript phase era where
  Phase1Script :: SomeScript 'PhaseOne era -> PhasedScript 'PhaseOne era
  Phase2Script :: Language -> ShortByteString -> PhasedScript 'PhaseTwo era

getPhase1 :: EraScript era => Map k (Script era) -> Map k (Script era, PhasedScript 'PhaseOne era)
getPhase1 = mapMaybe phase1
  where
    phase1 s = case phaseScript PhaseOneRep s of
      Just ps -> Just (s, ps)
      Nothing -> Nothing

getPhase2 :: EraScript era => Map k (Script era) -> Map k (PhasedScript 'PhaseTwo era)
getPhase2 = mapMaybe (phaseScript PhaseTwoRep)
