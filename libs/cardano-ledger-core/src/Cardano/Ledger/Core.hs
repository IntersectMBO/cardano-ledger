{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
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
  EraTxWits (..),
  EraScript (..),
  hashScript,
  isNativeScript,
  hashScriptTxWitsL,
  Value,
  EraPParams (..),
  mkCoinTxOut,

  -- * Era
  module Cardano.Ledger.Core.Era,
  -- $segWit
  EraSegWits (..),
  bBodySize,

  -- * Rewards
  RewardType (..),
  Reward (..),

  -- * Re-exports
  module Cardano.Ledger.Hashes,
  module Cardano.Ledger.Core.TxCert,
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
  DecCBOR,
  DecShareCBOR (Share),
  EncCBOR,
  EncCBORGroup,
  FromCBOR,
  Interns,
  Sized (sizedValue),
  ToCBOR,
  encCBORGroup,
  mkSized,
  serialize',
 )
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Compactible (Compactible (..))
import Cardano.Ledger.Core.Era
import Cardano.Ledger.Core.PParams
import Cardano.Ledger.Core.Translation
import Cardano.Ledger.Core.TxCert
import Cardano.Ledger.Credential (Credential)
import qualified Cardano.Ledger.Crypto as CC
import Cardano.Ledger.Hashes
import Cardano.Ledger.Keys (KeyHash, KeyRole (..))
import Cardano.Ledger.Keys.Bootstrap (BootstrapWitness)
import Cardano.Ledger.Keys.WitVKey (WitVKey)
import Cardano.Ledger.MemoBytes
import Cardano.Ledger.Metadata
import Cardano.Ledger.Rewards (Reward (..), RewardType (..))
import Cardano.Ledger.SafeHash (HashAnnotated (..), SafeToHash (..))
import Cardano.Ledger.TxIn (TxId (..), TxIn (..))
import Cardano.Ledger.Val (Val (..), inject)
import Control.DeepSeq (NFData)
import Data.Aeson (ToJSON)
import qualified Data.ByteString as BS
import Data.Kind (Type)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Maybe.Strict (StrictMaybe)
import Data.Sequence.Strict (StrictSeq)
import Data.Set (Set)
import Data.Void (Void)
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
  , -- NFData (Tx era), TODO: Add NFData constraints to Crypto class
    NoThunks (Tx era)
  , DecCBOR (Annotator (Tx era))
  , EncCBOR (Tx era)
  , ToCBOR (Tx era)
  , Show (Tx era)
  , Eq (Tx era)
  , EqRaw (Tx era)
  ) =>
  EraTx era
  where
  type Tx era = (r :: Type) | r -> era

  type TxUpgradeError era :: Type
  type TxUpgradeError era = Void

  mkBasicTx :: TxBody era -> Tx era

  bodyTxL :: Lens' (Tx era) (TxBody era)

  witsTxL :: Lens' (Tx era) (TxWits era)

  auxDataTxL :: Lens' (Tx era) (StrictMaybe (AuxiliaryData era))

  -- | For fee calculation and estimations of impact on block space
  sizeTxF :: SimpleGetter (Tx era) Integer

  -- | For end use by eg. diffusion layer in transaction submission protocol
  wireSizeTxF :: SimpleGetter (Tx era) Word32

  -- | Using information from the transaction validate the supplied native script.
  validateNativeScript :: Tx era -> NativeScript era -> Bool

  -- | Minimum fee calculation excluding witnesses
  getMinFeeTx ::
    PParams era ->
    Tx era ->
    -- | Size in bytes of reference scripts present in this transaction
    Int ->
    Coin

  upgradeTx ::
    EraTx (PreviousEra era) =>
    Tx (PreviousEra era) ->
    Either (TxUpgradeError era) (Tx era)

class
  ( EraTxOut era
  , EraTxCert era
  , EraPParams era
  , HashAnnotated (TxBody era) EraIndependentTxBody (EraCrypto era)
  , DecCBOR (Annotator (TxBody era))
  , EncCBOR (TxBody era)
  , ToCBOR (TxBody era)
  , NoThunks (TxBody era)
  , NFData (TxBody era)
  , Show (TxBody era)
  , Eq (TxBody era)
  , EqRaw (TxBody era)
  ) =>
  EraTxBody era
  where
  -- | The body of a transaction.
  type TxBody era = (r :: Type) | r -> era

  type TxBodyUpgradeError era :: Type
  type TxBodyUpgradeError era = Void

  mkBasicTxBody :: TxBody era

  inputsTxBodyL :: Lens' (TxBody era) (Set (TxIn (EraCrypto era)))

  outputsTxBodyL :: Lens' (TxBody era) (StrictSeq (TxOut era))

  feeTxBodyL :: Lens' (TxBody era) Coin

  withdrawalsTxBodyL :: Lens' (TxBody era) (Withdrawals (EraCrypto era))

  auxDataHashTxBodyL :: Lens' (TxBody era) (StrictMaybe (AuxiliaryDataHash (EraCrypto era)))

  -- | This getter will produce all inputs from the UTxO map that this transaction might
  -- spend, which ones will depend on the validity of the transaction itself. Starting in
  -- Alonzo this will include collateral inputs.
  spendableInputsTxBodyF :: SimpleGetter (TxBody era) (Set (TxIn (EraCrypto era)))

  -- | This getter will produce all inputs from the UTxO map that this transaction is
  -- referencing, even if some of them cannot be spent by the transaction. For example
  -- starting with Babbage era it will also include reference inputs.
  allInputsTxBodyF :: SimpleGetter (TxBody era) (Set (TxIn (EraCrypto era)))

  certsTxBodyL :: Lens' (TxBody era) (StrictSeq (TxCert era))

  -- | Compute the total deposits from the certificates in a TxBody.
  --
  -- This is the contribution of a TxBody towards the consumed amount by the transaction
  getTotalDepositsTxBody ::
    PParams era ->
    -- | Check whether stake pool is registered or not
    (KeyHash 'StakePool (EraCrypto era) -> Bool) ->
    TxBody era ->
    Coin
  getTotalDepositsTxBody pp isPoolRegisted txBody =
    getTotalDepositsTxCerts pp isPoolRegisted (txBody ^. certsTxBodyL)

  -- | Compute the total refunds from the Certs of a TxBody.
  --
  -- This is the contribution of a TxBody towards produced amount by the transaction
  getTotalRefundsTxBody ::
    PParams era ->
    -- | Lookup current deposit for Staking credential if one is registered
    (Credential 'Staking (EraCrypto era) -> Maybe Coin) ->
    -- | Lookup current deposit for DRep credential if one is registered
    (Credential 'DRepRole (EraCrypto era) -> Maybe Coin) ->
    TxBody era ->
    Coin
  getTotalRefundsTxBody pp lookupStakingDeposit lookupDRepDeposit txBody =
    getTotalRefundsTxCerts pp lookupStakingDeposit lookupDRepDeposit (txBody ^. certsTxBodyL)

  -- | This function is not used in the ledger rules. It is only used by the downstream
  -- tooling to figure out how many witnesses should be supplied for Genesis keys.
  getGenesisKeyHashCountTxBody :: TxBody era -> Int
  getGenesisKeyHashCountTxBody _ = 0

  -- | Upgrade the transaction body from the previous era.
  --
  -- This can fail where elements of the transaction body are deprecated.
  -- Compare this to `translateEraThroughCBOR`:
  -- - `upgradeTxBody` will use the Haskell representation, but will not
  --   preserve the serialised form. However, it will be suitable for iterated
  --   translation through eras.
  -- - `translateEraThroughCBOR` will preserve the binary representation, but is
  --   not guaranteed to work through multiple eras - that is, the serialised
  --   representation from era n is guaranteed valid in era n + 1, but not
  --   necessarily in era n + 2.
  upgradeTxBody ::
    EraTxBody (PreviousEra era) =>
    TxBody (PreviousEra era) ->
    Either (TxBodyUpgradeError era) (TxBody era)

-- | Abstract interface into specific fields of a `TxOut`
class
  ( Val (Value era)
  , ToJSON (TxOut era)
  , DecCBOR (Value era)
  , DecCBOR (CompactForm (Value era))
  , EncCBOR (Value era)
  , ToCBOR (TxOut era)
  , FromCBOR (TxOut era)
  , EncCBOR (TxOut era)
  , DecCBOR (TxOut era)
  , DecShareCBOR (TxOut era)
  , Share (TxOut era) ~ Interns (Credential 'Staking (EraCrypto era))
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
    , upgradeTxOut
    , valueEitherTxOutL
    , addrEitherTxOutL
    , (getMinCoinSizedTxOut | getMinCoinTxOut)
    #-}

  mkBasicTxOut :: HasCallStack => Addr (EraCrypto era) -> Value era -> TxOut era

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
  -- can define just this functionality. Also sometimes it is crucial to know at
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

bootAddrTxOutF ::
  EraTxOut era => SimpleGetter (TxOut era) (Maybe (BootstrapAddress (EraCrypto era)))
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
mkCoinTxOut :: EraTxOut era => Addr (EraCrypto era) -> Coin -> TxOut era
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
  , HashAnnotated (TxAuxData era) EraIndependentTxAuxData (EraCrypto era)
  ) =>
  EraTxAuxData era
  where
  type TxAuxData era = (r :: Type) | r -> era

  mkBasicTxAuxData :: TxAuxData era

  metadataTxAuxDataL :: Lens' (TxAuxData era) (Map Word64 Metadatum)

  -- | Every era, except Shelley, must be able to upgrade a `TxAuxData` from a previous
  -- era.
  --
  -- /Warning/ - Important to note that any memoized binary representation will not be
  -- preserved. If you need to retain underlying bytes you can use `translateEraThroughCBOR`
  upgradeTxAuxData :: EraTxAuxData (PreviousEra era) => TxAuxData (PreviousEra era) -> TxAuxData era

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

  addrTxWitsL :: Lens' (TxWits era) (Set (WitVKey 'Witness (EraCrypto era)))

  bootAddrTxWitsL :: Lens' (TxWits era) (Set (BootstrapWitness (EraCrypto era)))

  scriptTxWitsL :: Lens' (TxWits era) (Map (ScriptHash (EraCrypto era)) (Script era))

  upgradeTxWits :: EraTxWits (PreviousEra era) => TxWits (PreviousEra era) -> TxWits era

-- | This is a helper lens that will hash the scripts when adding as witnesses.
hashScriptTxWitsL ::
  EraTxWits era =>
  Lens (TxWits era) (TxWits era) (Map (ScriptHash (EraCrypto era)) (Script era)) [Script era]
hashScriptTxWitsL =
  lens
    (\wits -> wits ^. scriptTxWitsL)
    (\wits ss -> wits & scriptTxWitsL .~ Map.fromList [(hashScript s, s) | s <- ss])
{-# INLINEABLE hashScriptTxWitsL #-}

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
  -- preserved, you need to retain underlying bytes you can use `translateEraThroughCBOR`
  upgradeScript :: EraScript (PreviousEra era) => Script (PreviousEra era) -> Script era

  scriptPrefixTag :: Script era -> BS.ByteString

  getNativeScript :: Script era -> Maybe (NativeScript era)

  fromNativeScript :: NativeScript era -> Script era

isNativeScript :: EraScript era => Script era -> Bool
isNativeScript = isJust . getNativeScript

-- | Compute `ScriptHash` of a `Script` for a particular era.
hashScript :: forall era. EraScript era => Script era -> ScriptHash (EraCrypto era)
hashScript =
  ScriptHash
    . Hash.castHash
    . Hash.hashWith
      (\x -> scriptPrefixTag @era x <> originalBytes x)

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
  , EncCBORGroup (TxSeq era)
  , DecCBOR (Annotator (TxSeq era))
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

bBodySize :: forall era. EraSegWits era => ProtVer -> TxSeq era -> Int
bBodySize (ProtVer v _) = BS.length . serialize' v . encCBORGroup

txIdTx :: EraTx era => Tx era -> TxId (EraCrypto era)
txIdTx tx = txIdTxBody (tx ^. bodyTxL)

txIdTxBody :: EraTxBody era => TxBody era -> TxId (EraCrypto era)
txIdTxBody = TxId . hashAnnotated
