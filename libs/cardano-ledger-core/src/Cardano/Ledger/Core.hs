{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module defines core type families which we know to vary from era to
-- era.
--
-- Families in this module should be indexed on era.
--
-- It is intended for qualified import:
-- > import qualified Cardano.Ledger.Core as Core
module Cardano.Ledger.Core (
  -- * Transaction types
  module Cardano.Ledger.Core.TxLevel,

  -- * Era-changing types
  EraTx (..),
  txIdTx,
  EraTxOut (..),
  bootAddrTxOutF,
  coinTxOutL,
  compactCoinTxOutL,
  isAdaOnlyTxOutF,
  EraTxBody (..),
  txIdTxBody,
  EraTxAuxData (..),
  hashTxAuxData,
  EraTxWits (..),
  EraScript (..),
  hashScript,
  isNativeScript,
  hashScriptTxWitsL,
  keyHashWitnessesTxWits,
  Value,
  EraPParams (..),
  mkCoinTxOut,
  wireSizeTxF,
  binaryUpgradeTx,
  binaryUpgradeTxBody,
  binaryUpgradeTxWits,
  binaryUpgradeTxAuxData,
  fromStrictMaybeL,
  toStrictMaybeL,

  -- * Era
  module Cardano.Ledger.Core.Era,
  -- $erablockbody
  EraBlockBody (..),
  bBodySize,

  -- * Re-exports
  Addr (..),
  AccountId (..),
  AccountAddress (..),
  Withdrawals (..),
  module Cardano.Ledger.Hashes,
  module Cardano.Ledger.Core.TxCert,
  module Cardano.Ledger.Core.PParams,
  module Cardano.Ledger.Core.Translation,
) where

import qualified Cardano.Crypto.Hash as Hash
import Cardano.Ledger.Address (
  AccountAddress (..),
  AccountId (..),
  Addr (..),
  BootstrapAddress,
  CompactAddr,
  Withdrawals (..),
  compactAddr,
  decompactAddr,
  isBootstrapCompactAddr,
 )
import Cardano.Ledger.BaseTypes (ProtVer (..), integralToBounded)
import Cardano.Ledger.Binary (
  Annotator,
  DecCBOR,
  DecShareCBOR (Share),
  DecoderError,
  EncCBOR (..),
  EncCBORGroup,
  Interns,
  Sized (sizedValue),
  ToCBOR,
  encCBORGroup,
  mkSized,
  serialize,
  serialize',
  translateViaCBORAnnotator,
 )
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Compactible (Compactible (..))
import Cardano.Ledger.Core.Era
import Cardano.Ledger.Core.PParams
import Cardano.Ledger.Core.Translation
import Cardano.Ledger.Core.TxCert
import Cardano.Ledger.Core.TxLevel
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Hashes hiding (GenDelegPair (..), GenDelegs (..), unsafeMakeSafeHash)
import Cardano.Ledger.Keys.Bootstrap (BootstrapWitness, bootstrapWitKeyHash)
import Cardano.Ledger.Keys.WitVKey (WitVKey, witVKeyHash)
import Cardano.Ledger.MemoBytes
import Cardano.Ledger.Metadata
import Cardano.Ledger.TxIn (TxId (..), TxIn (..))
import Cardano.Ledger.Val (Val (..), inject)
import Control.DeepSeq (NFData)
import Control.Monad.Except (Except)
import Control.Monad.Trans.Fail.String (errorFail)
import Data.Aeson (ToJSON)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Kind (Type)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Maybe.Strict (StrictMaybe, maybeToStrictMaybe, strictMaybe, strictMaybeToMaybe)
import Data.MemPack
import Data.OMap.Strict (HasOKey (..))
import Data.Sequence.Strict (StrictSeq)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable
import Data.Word (Word32, Word64)
import GHC.Stack (HasCallStack)
import Lens.Micro
import NoThunks.Class (NoThunks)

-- | A transaction.
class
  ( EraTxBody era
  , EraTxWits era
  , EraTxAuxData era
  , EraPParams era
  , HasEraTxLevel Tx era
  , forall l. Typeable l => NoThunks (Tx l era)
  , forall l. Typeable l => DecCBOR (Annotator (Tx l era))
  , forall l. Typeable l => ToCBOR (Tx l era)
  , forall l. EncCBOR (Tx l era)
  , forall l. NFData (Tx l era)
  , forall l. Show (Tx l era)
  , forall l. Eq (Tx l era)
  ) =>
  EraTx era
  where
  data Tx (l :: TxLevel) era

  mkBasicTx :: TxBody l era -> Tx l era

  bodyTxL :: Lens' (Tx l era) (TxBody l era)

  witsTxL :: Lens' (Tx l era) (TxWits era)

  auxDataTxL :: Lens' (Tx l era) (StrictMaybe (TxAuxData era))

  -- | For fee calculation and estimations of impact on block space
  sizeTxF :: HasCallStack => SimpleGetter (Tx l era) Word32

  -- | For fee calculation and estimations of impact on block space
  -- To replace `sizeTxF` after it has been proved equivalent to it .
  sizeTxForFeeCalculation :: (HasCallStack, SafeToHash (TxWits era), Typeable l) => Tx l era -> Word32
  sizeTxForFeeCalculation tx =
    errorFail $
      integralToBounded @Int @Word32 $
        originalBytesSize (tx ^. bodyTxL)
          + originalBytesSize (tx ^. witsTxL)
          + strictMaybe 1 originalBytesSize (tx ^. auxDataTxL)
          + 1 -- account for the top-level CBOR encoding tag

  -- | Using information from the transaction validate the supplied native script.
  validateNativeScript :: Tx l era -> NativeScript era -> Bool

  -- | Minimum fee calculation excluding witnesses
  getMinFeeTx ::
    PParams era ->
    Tx l era ->
    -- | Size in bytes of reference scripts present in this transaction
    Int ->
    Coin

class
  ( EraTxOut era
  , EraTxCert era
  , EraPParams era
  , HasEraTxLevel TxBody era
  , forall l. HashAnnotated (TxBody l era) EraIndependentTxBody
  , forall l. EncCBOR (TxBody l era)
  , forall l. Typeable l => DecCBOR (Annotator (TxBody l era))
  , forall l. Typeable l => ToCBOR (TxBody l era)
  , forall l. Typeable l => NoThunks (TxBody l era)
  , forall l. NFData (TxBody l era)
  , forall l. Show (TxBody l era)
  , forall l. Eq (TxBody l era)
  , forall l. EqRaw (TxBody l era)
  ) =>
  EraTxBody era
  where
  -- | The body of a transaction.
  data TxBody (l :: TxLevel) era

  mkBasicTxBody :: Typeable l => TxBody l era

  inputsTxBodyL :: Lens' (TxBody l era) (Set TxIn)

  outputsTxBodyL :: Lens' (TxBody l era) (StrictSeq (TxOut era))

  feeTxBodyL :: Lens' (TxBody TopTx era) Coin

  withdrawalsTxBodyL :: Lens' (TxBody l era) Withdrawals

  auxDataHashTxBodyL :: Lens' (TxBody l era) (StrictMaybe TxAuxDataHash)

  -- | This getter will produce all inputs from the UTxO map that this transaction might
  -- spend, which ones will depend on the validity of the transaction itself. Starting in
  -- Alonzo this will include collateral inputs.
  spendableInputsTxBodyF :: SimpleGetter (TxBody l era) (Set TxIn)

  -- | This getter will produce all inputs from the UTxO map that this transaction is
  -- referencing, even if some of them cannot be spent by the transaction. For example
  -- starting with Babbage era it will also include reference inputs.
  allInputsTxBodyF :: SimpleGetter (TxBody TopTx era) (Set TxIn)

  certsTxBodyL :: Lens' (TxBody l era) (StrictSeq (TxCert era))

  -- | Compute the total deposits from the certificates in a TxBody.
  --
  -- This is the contribution of a TxBody towards the consumed amount by the transaction
  getTotalDepositsTxBody ::
    PParams era ->
    -- | Check whether stake pool is registered or not
    (KeyHash StakePool -> Bool) ->
    TxBody l era ->
    Coin
  getTotalDepositsTxBody pp isPoolRegisted txBody =
    getTotalDepositsTxCerts pp isPoolRegisted (txBody ^. certsTxBodyL)

  -- | Compute the total refunds from the Certs of a TxBody.
  --
  -- This is the contribution of a TxBody towards produced amount by the transaction
  getTotalRefundsTxBody ::
    PParams era ->
    -- | Lookup current deposit for Staking credential if one is registered
    (Credential Staking -> Maybe Coin) ->
    -- | Lookup current deposit for DRep credential if one is registered
    (Credential DRepRole -> Maybe Coin) ->
    TxBody l era ->
    Coin
  getTotalRefundsTxBody pp lookupStakingDeposit lookupDRepDeposit txBody =
    getTotalRefundsTxCerts pp lookupStakingDeposit lookupDRepDeposit (txBody ^. certsTxBodyL)

  -- | This function is not used in the ledger rules. It is only used by the downstream
  -- tooling to figure out how many witnesses should be supplied for Genesis keys.
  getGenesisKeyHashCountTxBody :: TxBody TopTx era -> Int
  getGenesisKeyHashCountTxBody _ = 0

-- | Abstract interface into specific fields of a `TxOut`
class
  ( Val (Value era)
  , ToJSON (TxOut era)
  , DecCBOR (Value era)
  , DecCBOR (CompactForm (Value era))
  , MemPack (CompactForm (Value era))
  , EncCBOR (Value era)
  , ToCBOR (TxOut era)
  , EncCBOR (TxOut era)
  , DecCBOR (TxOut era)
  , DecShareCBOR (TxOut era)
  , Share (TxOut era) ~ Interns (Credential Staking)
  , NoThunks (TxOut era)
  , NFData (TxOut era)
  , Show (TxOut era)
  , Eq (TxOut era)
  , MemPack (TxOut era)
  , EraPParams era
  ) =>
  EraTxOut era
  where
  -- | The output of a UTxO for a particular era
  type TxOut era = (r :: Type) | r -> era

  {-# MINIMAL
    mkBasicTxOut
    , upgradeTxOut
    , valueEitherTxOutL
    , addrEitherTxOutL
    , (getMinCoinSizedTxOut | getMinCoinTxOut)
    #-}

  mkBasicTxOut :: HasCallStack => Addr -> Value era -> TxOut era

  -- | Every era, except Shelley, must be able to upgrade a `TxOut` from a previous era.
  upgradeTxOut :: EraTxOut (PreviousEra era) => TxOut (PreviousEra era) -> TxOut era

  valueTxOutL :: Lens' (TxOut era) (Value era)
  valueTxOutL =
    lens
      ( \txOut -> case txOut ^. valueEitherTxOutL of
          Left value -> value
          Right cValue -> fromCompact cValue
      )
      (\txOut value -> txOut & valueEitherTxOutL .~ Left value)
  {-# INLINE valueTxOutL #-}

  compactValueTxOutL :: HasCallStack => Lens' (TxOut era) (CompactForm (Value era))
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

  addrTxOutL :: Lens' (TxOut era) Addr
  addrTxOutL =
    lens
      ( \txOut -> case txOut ^. addrEitherTxOutL of
          Left addr -> addr
          Right cAddr -> decompactAddr cAddr
      )
      (\txOut addr -> txOut & addrEitherTxOutL .~ Left addr)
  {-# INLINE addrTxOutL #-}

  compactAddrTxOutL :: Lens' (TxOut era) CompactAddr
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
  -- can define just this functionality. Also sometimes it is crucial to know at
  -- the callsite which form of address we have readily available without any
  -- conversions (eg. searching millions of TxOuts for a particular address)
  addrEitherTxOutL :: Lens' (TxOut era) (Either Addr CompactAddr)

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

bootAddrTxOutF ::
  EraTxOut era => SimpleGetter (TxOut era) (Maybe BootstrapAddress)
bootAddrTxOutF = to $ \txOut ->
  case txOut ^. addrEitherTxOutL of
    Left (AddrBootstrap bootstrapAddr) -> Just bootstrapAddr
    Right cAddr
      | isBootstrapCompactAddr cAddr -> do
          AddrBootstrap bootstrapAddr <- Just (decompactAddr cAddr)
          Just bootstrapAddr
    _ -> Nothing
{-# INLINE bootAddrTxOutF #-}

coinTxOutL :: (HasCallStack, EraTxOut era) => Lens' (TxOut era) Coin
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

compactCoinTxOutL :: (HasCallStack, EraTxOut era) => Lens' (TxOut era) (CompactForm Coin)
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

toCompactPartial :: (HasCallStack, Val a) => a -> CompactForm a
toCompactPartial v =
  fromMaybe (error $ "Illegal value in TxOut: " <> show v) $ toCompact v

-- A version of mkBasicTxOut, which has only a Coin (no multiAssets) for every EraTxOut era.
mkCoinTxOut :: EraTxOut era => Addr -> Coin -> TxOut era
mkCoinTxOut addr = mkBasicTxOut addr . inject

-- | A value is something which quantifies a transaction output.
type family Value era :: Type

-- | TxAuxData which may be attached to a transaction
class
  ( Era era
  , Eq (TxAuxData era)
  , EqRaw (TxAuxData era)
  , Show (TxAuxData era)
  , NoThunks (TxAuxData era)
  , ToCBOR (TxAuxData era)
  , EncCBOR (TxAuxData era)
  , DecCBOR (Annotator (TxAuxData era))
  , HashAnnotated (TxAuxData era) EraIndependentTxAuxData
  ) =>
  EraTxAuxData era
  where
  type TxAuxData era = (r :: Type) | r -> era

  mkBasicTxAuxData :: TxAuxData era

  metadataTxAuxDataL :: Lens' (TxAuxData era) (Map Word64 Metadatum)

  validateTxAuxData :: ProtVer -> TxAuxData era -> Bool

-- | Compute a hash of `TxAuxData`
hashTxAuxData :: EraTxAuxData era => TxAuxData era -> TxAuxDataHash
hashTxAuxData = TxAuxDataHash . hashAnnotated

-- | A collection of witnesses in a Tx
class
  ( EraScript era
  , Eq (TxWits era)
  , EqRaw (TxWits era)
  , Show (TxWits era)
  , Monoid (TxWits era)
  , NoThunks (TxWits era)
  , ToCBOR (TxWits era)
  , EncCBOR (TxWits era)
  , DecCBOR (Annotator (TxWits era))
  ) =>
  EraTxWits era
  where
  type TxWits era = (r :: Type) | r -> era

  mkBasicTxWits :: TxWits era
  mkBasicTxWits = mempty

  addrTxWitsL :: Lens' (TxWits era) (Set (WitVKey Witness))

  bootAddrTxWitsL :: Lens' (TxWits era) (Set BootstrapWitness)

  scriptTxWitsL :: Lens' (TxWits era) (Map ScriptHash (Script era))

-- | This is a helper lens that will hash the scripts when adding as witnesses.
hashScriptTxWitsL ::
  EraTxWits era =>
  Lens (TxWits era) (TxWits era) (Map ScriptHash (Script era)) [Script era]
hashScriptTxWitsL =
  lens
    (^. scriptTxWitsL)
    (\wits ss -> wits & scriptTxWitsL .~ Map.fromList [(hashScript s, s) | s <- ss])
{-# INLINEABLE hashScriptTxWitsL #-}

-- | Extract all of the `KeyHash` witnesses provided in the `TxWits`
keyHashWitnessesTxWits ::
  EraTxWits era =>
  TxWits era ->
  Set (KeyHash Witness)
keyHashWitnessesTxWits txWits =
  Set.map witVKeyHash (txWits ^. addrTxWitsL)
    `Set.union` Set.map bootstrapWitKeyHash (txWits ^. bootAddrTxWitsL)
{-# INLINEABLE keyHashWitnessesTxWits #-}

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
  , EqRaw (Script era)
  , ToCBOR (Script era)
  , EncCBOR (Script era)
  , DecCBOR (Annotator (Script era))
  , NoThunks (Script era)
  , SafeToHash (Script era)
  , Eq (NativeScript era)
  , Show (NativeScript era)
  , NFData (NativeScript era)
  , NoThunks (NativeScript era)
  , ToCBOR (NativeScript era)
  , EncCBOR (NativeScript era)
  , DecCBOR (Annotator (NativeScript era))
  ) =>
  EraScript era
  where
  -- | Scripts which may lock transaction outputs in this era
  type Script era = (r :: Type) | r -> era

  type NativeScript era = (r :: Type) | r -> era

  -- | Every era, except Shelley, must be able to upgrade a `Script` from a previous era.
  --
  -- /Warning/ - Important to note that any memoized binary representation will not be
  -- preserved. If you need to retain underlying bytes then you can use `translateEraThroughCBOR`
  upgradeScript :: EraScript (PreviousEra era) => Script (PreviousEra era) -> Script era

  scriptPrefixTag :: Script era -> BS.ByteString

  getNativeScript :: Script era -> Maybe (NativeScript era)

  fromNativeScript :: NativeScript era -> Script era

isNativeScript :: EraScript era => Script era -> Bool
isNativeScript = isJust . getNativeScript

-- | Compute `ScriptHash` of a `Script` for a particular era.
hashScript :: forall era. EraScript era => Script era -> ScriptHash
hashScript =
  ScriptHash
    . Hash.castHash
    . Hash.hashWith
      (\x -> scriptPrefixTag @era x <> originalBytes x)

--------------------------------------------------------------------------------
-- EraBlockBody - Segregated Witness
--------------------------------------------------------------------------------

-- $erablockbody
-- * EraBlockBody
--
-- The idea of EraBlockBody is to alter the encoding of transactions in
-- a block such that the witnesses (the information needed to verify the
-- validity of the transactions) can be stored separately from the body (the
-- information needed to update the ledger state). In this way, a node which
-- only cares about replaying transactions need not even decode the witness
-- information.
--
-- In order to do this, we introduce two concepts:
-- - A 'BlockBody`, which represents the decoded structure of a sequence of
--   transactions as represented in the encoded block; that is, with witnessing,
--   metadata and other non-body parts split separately.

-- | Indicates that an era supports segregated witnessing.
--
--   This class embodies an isomorphism between 'BlockBody era' and 'StrictSeq
--   (Tx l era)', witnessed by the `txSeqBlockBodyL` lens.
class
  ( EraTx era
  , Eq (BlockBody era)
  , Show (BlockBody era)
  , Typeable (BlockBody era)
  , EncCBORGroup (BlockBody era)
  , DecCBOR (Annotator (BlockBody era))
  ) =>
  EraBlockBody era
  where
  type BlockBody era = (r :: Type) | r -> era

  mkBasicBlockBody :: BlockBody era

  txSeqBlockBodyL :: Lens' (BlockBody era) (StrictSeq (Tx TopTx era))

  -- | Get the block body hash from the BlockBody. Note that this is not a regular
  -- "hash the stored bytes" function since the block body hash forms a small
  -- Merkle tree.
  hashBlockBody :: BlockBody era -> Hash.Hash HASH EraIndependentBlockBody

  -- | The number of segregated components
  numSegComponents :: Word64

bBodySize :: forall era. EraBlockBody era => ProtVer -> BlockBody era -> Int
bBodySize (ProtVer v _) = BS.length . serialize' v . encCBORGroup

txIdTx :: EraTx era => Tx l era -> TxId
txIdTx tx = txIdTxBody (tx ^. bodyTxL)

txIdTxBody :: EraTxBody era => TxBody l era -> TxId
txIdTxBody = TxId . hashAnnotated

-- | txsize computes the length of the serialised bytes (actual size)
wireSizeTxF :: forall era l. EraTx era => SimpleGetter (Tx l era) Word32
wireSizeTxF =
  to $
    checkedFromIntegral
      . LBS.length
      . serialize (eraProtVerLow @era)
      . encCBOR
  where
    checkedFromIntegral n =
      if n <= fromIntegral (maxBound :: Word32)
        then fromIntegral n
        else error $ "Impossible: Size of the transaction is too big: " ++ show n
{-# INLINEABLE wireSizeTxF #-}

-- | Translate a transaction through its binary representation from previous to current era.
binaryUpgradeTx ::
  forall era l.
  (Era era, ToCBOR (Tx l (PreviousEra era)), DecCBOR (Annotator (Tx l era))) =>
  Tx l (PreviousEra era) ->
  Except DecoderError (Tx l era)
binaryUpgradeTx = translateViaCBORAnnotator (eraProtVerLow @era) (withEraName @era "Tx")

-- | Translate a tx body through its binary representation from previous to current era.
binaryUpgradeTxBody ::
  forall era l.
  (Era era, ToCBOR (TxBody l (PreviousEra era)), DecCBOR (Annotator (TxBody l era))) =>
  TxBody l (PreviousEra era) ->
  Except DecoderError (TxBody l era)
binaryUpgradeTxBody = translateViaCBORAnnotator (eraProtVerLow @era) (withEraName @era "TxBody")

-- | Translate tx witnesses through its binary representation from previous to current era.
binaryUpgradeTxWits ::
  forall era.
  (Era era, ToCBOR (TxWits (PreviousEra era)), DecCBOR (Annotator (TxWits era))) =>
  TxWits (PreviousEra era) ->
  Except DecoderError (TxWits era)
binaryUpgradeTxWits = translateViaCBORAnnotator (eraProtVerLow @era) (withEraName @era "TxWits")

-- | Translate tx auxData through its binary representation from previous to current era.
binaryUpgradeTxAuxData ::
  forall era.
  (Era era, ToCBOR (TxAuxData (PreviousEra era)), DecCBOR (Annotator (TxAuxData era))) =>
  TxAuxData (PreviousEra era) ->
  Except DecoderError (TxAuxData era)
binaryUpgradeTxAuxData = translateViaCBORAnnotator (eraProtVerLow @era) (withEraName @era "TxAuxData")

withEraName :: forall era. Era era => Text -> Text
withEraName t = t <> " " <> T.pack (eraName @era) <> "Era"

toStrictMaybeL :: Lens' (Maybe a) (StrictMaybe a)
toStrictMaybeL = lens maybeToStrictMaybe (const strictMaybeToMaybe)

fromStrictMaybeL :: Lens' (StrictMaybe a) (Maybe a)
fromStrictMaybeL = lens strictMaybeToMaybe (const maybeToStrictMaybe)

instance EraTx era => HasOKey TxId (Tx l era) where
  toOKey = txIdTx

instance EraTxBody era => HasOKey TxId (TxBody l era) where
  toOKey = txIdTxBody
