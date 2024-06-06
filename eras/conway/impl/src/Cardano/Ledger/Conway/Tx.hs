{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Cardano.Ledger.Conway.Tx (
  module BabbageTxReExport,
  ConwayTxZones,
  pattern ConwayTxZones,
)
where

import Cardano.Crypto.Hash ()
import qualified Cardano.Crypto.Hash as Hash hiding (Hash)
import Cardano.Ledger.Allegra.Tx (validateTimelock)
import Cardano.Ledger.Alonzo.Core (AlonzoEraTxWits)
import Cardano.Ledger.Alonzo.Tx (
  IsValid (IsValid),
  alonzoMinFeeTx,
  alonzoSegwitTx,
  auxDataAlonzoTxL,
  bodyAlonzoTxL,
  isValidAlonzoTxL,
  mkBasicAlonzoTx,
  sizeAlonzoTxF,
  witsAlonzoTxL,
 )
import Cardano.Ledger.Babbage.Tx as BabbageTxReExport (
  AlonzoEraTx (..),
  AlonzoTx (..),
 )
import Cardano.Ledger.BaseTypes (StrictMaybe (SNothing), strictMaybeToMaybe, unboundRational)
import Cardano.Ledger.Binary (
  Annotator,
  DecCBOR (decCBOR),
  EncCBOR (encCBOR, encodedSizeExpr),
  EncCBORGroup (encodedGroupSizeExpr),
  encCBOR,
  encCBORGroup,
  encodeFoldableEncoder,
  encodeFoldableMapEncoder,
  encodePreEncoded,
  listLenBound,
  serialize,
  withSlice,
 )
import Cardano.Ledger.Binary.Group (EncCBORGroup (listLen))
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Era (ConwayEra)
import Cardano.Ledger.Conway.PParams (ConwayEraPParams, ppMinFeeRefScriptCostPerByteL)
import Cardano.Ledger.Conway.TxAuxData ()
import Cardano.Ledger.Conway.TxBody ()
import Cardano.Ledger.Conway.TxWits ()
import Cardano.Ledger.Core (
  Era,
  EraTx (auxDataTxL),
  Tx,
  bodyTxL,
  eraProtVerLow,
  upgradeTxAuxData,
  upgradeTxBody,
  upgradeTxWits,
  witsTxL,
 )
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Crypto
import Cardano.Ledger.Keys (Hash)
import Cardano.Ledger.SafeHash (SafeToHash (..))
import Cardano.Ledger.Shelley.BlockChain (constructMetadata)
import Cardano.Ledger.Shelley.Tx (ShelleyRequiredTx)
import Cardano.Ledger.Val (Val (..))
import Control.Monad (unless)
import Data.ByteString (ByteString)
import Data.ByteString.Builder (shortByteString, toLazyByteString)
import qualified Data.ByteString.Lazy as BSL
import Data.Coerce (coerce)
import Data.Data (Proxy)
import qualified Data.Map as Map
import Data.Proxy (Proxy (..))
import qualified Data.Sequence as Seq
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as StrictSeq
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Lens.Micro hiding (set)
import Lens.Micro.Extras (view)
import NoThunks.Class (AllowThunksIn (AllowThunksIn), NoThunks)

instance Crypto c => Core.EraTx (ConwayEra c) where
  {-# SPECIALIZE instance Core.EraTx (ConwayEra StandardCrypto) #-}

  type Tx (ConwayEra c) = AlonzoTx (ConwayEra c)
  type TxUpgradeError (ConwayEra c) = Core.TxBodyUpgradeError (ConwayEra c)

  mkBasicTx = mkBasicAlonzoTx

  bodyTxL = bodyAlonzoTxL
  {-# INLINE bodyTxL #-}

  witsTxL = witsAlonzoTxL
  {-# INLINE witsTxL #-}

  auxDataTxL = auxDataAlonzoTxL
  {-# INLINE auxDataTxL #-}

  requiredTxsTxL = lens (const mempty) const
  {-# INLINE requiredTxsTxL #-}

  sizeTxF = sizeAlonzoTxF
  {-# INLINE sizeTxF #-}

  validateNativeScript = validateTimelock
  {-# INLINE validateNativeScript #-}

  getMinFeeTx = getConwayMinFeeTx

  upgradeTx (AlonzoTx b w valid aux _) =
    AlonzoTx
      <$> upgradeTxBody b
      <*> pure (upgradeTxWits w)
      <*> pure valid
      <*> pure (fmap upgradeTxAuxData aux)
      <*> pure SNothing -- TODO WG

getConwayMinFeeTx ::
  ( Core.EraTx era
  , AlonzoEraTxWits era
  , ConwayEraPParams era
  ) =>
  Core.PParams era ->
  Core.Tx era ->
  Int ->
  Coin
getConwayMinFeeTx pp tx refScriptsSize =
  alonzoMinFeeTx pp tx <+> refScriptsFee
  where
    refScriptCostPerByte = unboundRational (pp ^. ppMinFeeRefScriptCostPerByteL)
    refScriptsFee = Coin (floor (fromIntegral @Int @Rational refScriptsSize * refScriptCostPerByte))

instance Crypto c => AlonzoEraTx (ConwayEra c) where
  {-# SPECIALIZE instance AlonzoEraTx (ConwayEra StandardCrypto) #-}

  isValidTxL = isValidAlonzoTxL
  {-# INLINE isValidTxL #-}

instance Crypto c => Core.EraRequiredTxsData (ConwayEra c) where
  type RequiredTxs (ConwayEra c) = ShelleyRequiredTx (ConwayEra c)

instance Crypto c => Core.EraSegWits (ConwayEra c) where
  type TxZones (ConwayEra c) = ConwayTxZones (ConwayEra c)
  fromTxZones = txZonesTxns
  toTxZones = ConwayTxZones
  hashTxZones = hashConwayTxZones
  numSegComponents = 4

-- hashAlonzoTxSeq :: forall era.
-- Era era =>
-- AlonzoTxSeq era -> Hash (EraCrypto era) EraIndependentBlockBody
-- Defined at /home/will/git/cardano-ledger/eras/alonzo/impl/src/Cardano/Ledger/Alonzo/TxSeq.hs:177:1

-- _ :: TxZones (AlonzoEra c)
-- -> Hash (HASH (EraCrypto (AlonzoEra c))) EraIndependentBlockBody
-- _ :: forall era.
-- Era era =>
-- AlonzoTxSeq era -> Hash (EraCrypto era) EraIndependentBlockBody

--------------------------------------------------------------------------------
-- Serialisation and hashing
--------------------------------------------------------------------------------

instance Era era => EncCBORGroup (TxZones era) where
  encCBORGroup (ConwayTxZonesRaw _ bodyBytes witsBytes metadataBytes invalidBytes) =
    encodePreEncoded $
      BSL.toStrict $
        bodyBytes <> witsBytes <> metadataBytes <> invalidBytes
  encodedGroupSizeExpr size _proxy =
    encodedSizeExpr size (Proxy :: Proxy BSL.ByteString)
      + encodedSizeExpr size (Proxy :: Proxy BSL.ByteString)
      + encodedSizeExpr size (Proxy :: Proxy BSL.ByteString)
      + encodedSizeExpr size (Proxy :: Proxy BSL.ByteString)
  listLen _ = 4
  listLenBound _ = 4

instance AlonzoEraTx era => DecCBOR (Annotator (TxZones era)) where
  decCBOR = do
    (bodies, bodiesAnn) <- withSlice decCBOR
    (ws, witsAnn) <- withSlice decCBOR
    let b = length bodies
        inRange x = (0 <= x) && (x <= (b - 1))
        w = length ws
    (auxData :: Seq.Seq (Maybe (Annotator (Core.TxAuxData era))), auxDataAnn) <- withSlice $
      do
        m <- decCBOR
        unless
          (all inRange (Map.keysSet m))
          ( fail
              ( "Some Auxiliarydata index is not in the range: 0 .. "
                  ++ show (b - 1)
              )
          )
        pure (constructMetadata b m)
    (isValIdxs, isValAnn) <- withSlice decCBOR
    let vs = alignedValidFlags b isValIdxs
    unless
      (b == w)
      ( fail $
          "different number of transaction bodies ("
            <> show b
            <> ") and witness sets ("
            <> show w
            <> ")"
      )
    unless
      (all inRange isValIdxs)
      ( fail
          ( "Some IsValid index is not in the range: 0 .. "
              ++ show (b - 1)
              ++ ", "
              ++ show isValIdxs
          )
      )
    let
      -- TODO WG: This might not actually make sense. Think about it.
      txns :: Annotator (StrictSeq (StrictSeq (Tx era)))
      txns =
        traverse
          ( \bodies' ->
              sequenceA $
                StrictSeq.forceToStrict $
                  Seq.zipWith4 alonzoSegwitTx bodies' ws vs auxData
          )
          (StrictSeq.forceToStrict bodies)

    pure $
      ConwayTxZonesRaw
        <$> txns
        <*> bodiesAnn
        <*> witsAnn
        <*> auxDataAnn
        <*> isValAnn

-- | Hash a given block body
hashConwayTxZones ::
  forall era.
  Era era =>
  ConwayTxZones era ->
  Hash (Core.EraCrypto era) Core.EraIndependentBlockBody
hashConwayTxZones (ConwayTxZonesRaw _ bodies ws md vs) =
  coerce $
    hashStrict $
      BSL.toStrict $
        toLazyByteString $
          mconcat
            [ hashPart bodies
            , hashPart ws
            , hashPart md
            , hashPart vs
            ]
  where
    hashStrict :: ByteString -> Hash (Core.EraCrypto era) ByteString
    hashStrict = Hash.hashWith id
    hashPart = shortByteString . Hash.hashToBytesShort . hashStrict . BSL.toStrict

data ConwayTxZones era = ConwayTxZonesRaw
  { txZonesTxns :: !(StrictSeq (StrictSeq (Core.Tx era)))
  , txZonesBodyBytes :: BSL.ByteString
  -- ^ Bytes encoding @Seq ('AlonzoTxBody' era)@
  , txZonesWitsBytes :: BSL.ByteString
  -- ^ Bytes encoding @Seq ('TxWitness' era)@
  , txZonesMetadataBytes :: BSL.ByteString
  -- ^ Bytes encoding a @Map Int ('AuxiliaryData')@. Missing indices have
  -- 'SNothing' for metadata
  , txZonesIsValidBytes :: BSL.ByteString
  -- ^ Bytes representing a set of integers. These are the indices of
  -- transactions with 'isValid' == False.
  }
  deriving (Generic)

pattern ConwayTxZones ::
  forall era.
  ( AlonzoEraTx era
  , SafeToHash (Core.TxWits era)
  ) =>
  StrictSeq (StrictSeq (Core.Tx era)) ->
  ConwayTxZones era
pattern ConwayTxZones xs <-
  ConwayTxZonesRaw xs _ _ _ _
  where
    ConwayTxZones txns =
      let version = eraProtVerLow @era
          serializeFoldablePreEncoded x =
            serialize version $
              encodeFoldableEncoder encodePreEncoded x
          metaChunk index m = encodeIndexed <$> strictMaybeToMaybe m
            where
              encodeIndexed metadata = encCBOR index <> encodePreEncoded metadata
          flattenedTxns =
            StrictSeq.forceToStrict
              (StrictSeq.fromStrict =<< StrictSeq.fromStrict txns)
       in ConwayTxZonesRaw
            { txZonesTxns = txns
            , txZonesBodyBytes =
                serializeFoldablePreEncoded $ originalBytes . view bodyTxL <$> flattenedTxns
            , txZonesWitsBytes =
                serializeFoldablePreEncoded $ originalBytes . view witsTxL <$> flattenedTxns
            , txZonesMetadataBytes =
                serialize version . encodeFoldableMapEncoder metaChunk $
                  fmap originalBytes . view auxDataTxL
                    <$> StrictSeq.forceToStrict
                      (StrictSeq.fromStrict =<< StrictSeq.fromStrict txns)
            , txZonesIsValidBytes =
                serialize version $ encCBOR $ nonValidatingIndices flattenedTxns
            }

{-# COMPLETE ConwayTxZones #-}

type TxZones era = ConwayTxZones era

{-# DEPRECATED TxZones "Use `ConwayTxZones` instead" #-}

deriving via
  AllowThunksIn
    '[ "txZonesBodyBytes"
     , "txZonesWitsBytes"
     , "txZonesMetadataBytes"
     , "txZonesIsValidBytes"
     ]
    (TxZones era)
  instance
    (Typeable era, NoThunks (Core.Tx era)) => NoThunks (TxZones era)

deriving stock instance Show (Core.Tx era) => Show (TxZones era)

deriving stock instance Eq (Core.Tx era) => Eq (TxZones era)

--------------------------------------------------------------------------------
-- Internal utility functions
--------------------------------------------------------------------------------

-- | Given a sequence of transactions, return the indices of those which do not
-- validate. We store the indices of the non-validating transactions because we
-- expect this to be a much smaller set than the validating transactions.
nonValidatingIndices :: AlonzoEraTx era => StrictSeq (Tx era) -> [Int]
nonValidatingIndices (StrictSeq.fromStrict -> xs) =
  Seq.foldrWithIndex
    ( \idx tx acc ->
        if tx ^. isValidTxL == IsValid False
          then idx : acc
          else acc
    )
    []
    xs

-- | Given the number of transactions, and the set of indices for which these
-- transactions do not validate, create an aligned sequence of `IsValid`
-- flags.
--
-- This function operates much as the inverse of 'nonValidatingIndices'.
alignedValidFlags :: Int -> [Int] -> Seq.Seq IsValid
alignedValidFlags = alignedValidFlags' (-1)
  where
    alignedValidFlags' _ n [] = Seq.replicate n $ IsValid True
    alignedValidFlags' prev n (x : xs) =
      Seq.replicate (x - prev - 1) (IsValid True)
        Seq.>< IsValid False
        Seq.<| alignedValidFlags' x (n - (x - prev)) xs
