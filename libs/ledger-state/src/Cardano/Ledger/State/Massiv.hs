{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Ledger.State.Massiv where

import Control.Monad
import Control.DeepSeq
import qualified Cardano.Address.Style.Byron as AB
import qualified Cardano.Address.Style.Icarus as AI
import qualified Cardano.Address.Style.Shelley as AS
import qualified Cardano.Chain.Common as Byron
import Cardano.Crypto.Hash.Class
import qualified Cardano.Crypto.Hashing as Byron
import Cardano.Ledger.Address
import Cardano.Ledger.Alonzo
import Cardano.Ledger.Alonzo.Tx as Alonzo
import Cardano.Ledger.Alonzo.TxBody as Alonzo
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Compactible
import Cardano.Ledger.Core as Core
import Cardano.Ledger.Credential
import Cardano.Ledger.Crypto
import qualified Cardano.Ledger.Hashes as Hashes
import qualified Cardano.Ledger.Keys as Keys
import qualified Cardano.Ledger.Mary.Value as Mary
import qualified Cardano.Ledger.SafeHash as SafeHash
import qualified Cardano.Ledger.Shelley.API as Shelley
import Cardano.Ledger.Shelley.CompactAddr
import Cardano.Ledger.Shelley.TxBody
import Cardano.Ledger.Shelley.UTxO
import Cardano.Ledger.State.UTxO
import qualified Conduit as C
import Control.Monad
import Data.ByteString.Short
import qualified Data.Conduit.List as C
import qualified Data.IntMap.Strict as IntMap
import Data.Kind
import qualified Data.Map.Strict as Map
import Data.Massiv.Array as A
import Data.Massiv.Array.Mutable.Algorithms as A
import Data.Massiv.Array.Unsafe as A
import Data.Proxy
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Read as T
import Data.Word
import Debug.Trace



loadMassivUTxO :: FilePath -> IO UTxOs
loadMassivUTxO fp = consumeUTxO fp sinkMassivUTxO

utxoFromMap :: Map.Map (TxIn C) (Alonzo.TxOut CurrentEra) -> IO UTxOs
utxoFromMap m = C.runConduit $ C.sourceList (Map.toList m) C..| sinkMassivUTxO

sinkMassivUTxO :: C.ConduitT (TxIn C, Alonzo.TxOut CurrentEra) C.Void IO UTxOs
sinkMassivUTxO = do
  let consTxOut txId txOut Nothing = Just $! A.singleton $ KVPair txId txOut
      consTxOut txId txOut (Just a) = Just $! A.cons (KVPair txId txOut) a
  das <-
    C.foldlC
      (\im (!(TxIn txId txIx), !txOut) ->
         let !txOut' = restructureTxOut txOut
             !vec = txOut' `deepseq` consTxOut txId txOut'
         in IntMap.alter vec (fromIntegral txIx) im)
      mempty
  C.lift $
    UTxOs . fromIntMap <$>
    traverse (\da -> withLoadMArray_ da quicksortKVMArray_) das

restructureTxOut :: Alonzo.TxOut CurrentEra -> TxOut'
restructureTxOut =
  \case
    Alonzo.TxOutCompact cAddr cVal
      | Mary.CompactValue (Mary.CompactValueAdaOnly ada) <- cVal ->
        TxOut' (restructureAddr cAddr) ada
      | Mary.CompactValue (Mary.CompactValueMultiAsset ada ma rep) <- cVal ->
        TxOutMA' (restructureAddr cAddr) ada ma rep
    Alonzo.TxOutCompactDH cAddr cVal dh
      | Mary.CompactValue (Mary.CompactValueAdaOnly ada) <- cVal ->
        TxOutDH' (restructureAddr cAddr) ada dh
      | Mary.CompactValue (Mary.CompactValueMultiAsset ada ma rep) <- cVal ->
        TxOutMADH' (restructureAddr cAddr) ada ma rep dh
  where
    restructureAddr cAddr =
      case decompactAddr cAddr of
        AddrBootstrap _ -> AddrBoot' cAddr
        Addr ni pc sr
          | KeyHashObj kh <- pc -> AddrKeyHash' ni kh $ restructureStakeIx sr
          | ScriptHashObj sh <- pc -> AddrScript' ni sh $ restructureStakeIx sr
    restructureStakeIx =
      \case
        StakeRefBase (KeyHashObj kh) -> StakeKeyHash kh
        StakeRefBase (ScriptHashObj sh) -> StakeCredScript sh
        StakeRefPtr ptr -> StakePtr ptr
        StakeRefNull -> StakeNull


testMassivUTxO :: Map.Map (TxIn C) (Alonzo.TxOut CurrentEra) -> UTxOs -> IO ()
testMassivUTxO m utxo@UTxOs{} =
  Control.Monad.forM_ (Map.toList m) $ \(txIn, txOut) -> do
    case lookupUTxOs txIn utxo of
      Nothing -> error $ "Could not find: " <> show txIn
      Just txOut' ->
        when (txOut' /= txOut) $
        error $
        unlines
          ["Recovered txOut doesn match: ", show txOut', " /= ", show txOut]


collectSharedKeys ::
  Vector B (Vector (KV S BN) (KVPair (TxId C) TxOut')) ->
  Vector S (Keys.KeyHash 'Shelley.Witness C)
collectSharedKeys txOutVec =
  let extractKeyHashes !accVec =
        \case
          AddrKeyHash' _ kh (StakeKeyHash skh) ->
            A.cons (Keys.asWitness kh) (A.cons (Keys.asWitness skh) accVec)
          AddrScript' _ _ (StakeKeyHash skh) ->
            A.cons (Keys.asWitness skh) accVec
          _ -> accVec
      collectKeys !accVec =
        \case
          TxOut' addr _ -> extractKeyHashes accVec addr
          TxOutMA' addr _ _ _ -> extractKeyHashes accVec addr
          TxOutDH' addr _ _ -> extractKeyHashes accVec addr
          TxOutMADH' addr _ _ _ _ -> extractKeyHashes accVec addr
      collectHashes !a (KVArray _ vals) = A.foldlS collectKeys a vals
      vectorWithKeyHashes :: Vector S (Keys.KeyHash 'Shelley.Witness C)
      vectorWithKeyHashes = A.compute $ foldlS collectHashes A.empty txOutVec
   in A.clone $ -- in order to free up leftover memory allocated while vector was growing
      A.compute $
      A.smapMaybe (\(k, c) -> guard (c > 1) >> Just k) $
      A.tally vectorWithKeyHashes


-- constructMassivUTxO ::
--   Vector (KV P B) (KVPair Int (Vector (KV S B) (KVPair (TxId C) (Alonzo.TxOut CurrentEra)))) ->
--   UTxOs
-- constructMassivUTxO (KVArray txIxs txOutVec) =
--   let !utxoWithSharing = KVArray txIxs utxoNoSharing
--       _applySharing kv@(KVArray _keys _values) = kv
--         -- let !values' = A.compute $ A.map applyTxOutSharing values
--         --     applyTxOutSharing =
--         --       \case
--         --         TxOut' addr ada -> TxOut' (applyAddrSharing addr) ada
--         --         TxOutMA' addr ada ma rep -> TxOutMA' (applyAddrSharing addr) ada ma rep
--         --         TxOutDH' addr ada dh -> TxOutDH' (applyAddrSharing addr) ada dh
--         --         TxOutMADH' addr ada ma rep dh -> TxOutMADH' (applyAddrSharing addr) ada ma rep dh
--         --     applyStakeSharing =
--         --       \case
--         --         StakeKeyHash skh
--         --           | Just skhix <- lookupKeyHashIx skh -> StakeKeyIx skhix
--         --         s -> s
--         --     applyAddrSharing =
--         --       \case
--         --         AddrScript' ni sh s -> AddrScript' ni sh $ applyStakeSharing s
--         --         AddrKeyHash' ni kh s -> AddrKeyHash' ni kh $ applyStakeSharing s
--         --         addr -> addr
--         --  in KVArray keys values'
--       !utxoNoSharing = A.compute $ A.map applyRestructure txOutVec
--       applyRestructure ::
--            Vector (KV S B) (KVPair (TxId C) (Alonzo.TxOut CurrentEra))
--         -> Vector (KV S BN) (KVPair (TxId C) TxOut')
--       applyRestructure (KVArray txIds txOuts) =
--         let !txOuts' = A.compute $ A.map restructureTxOut txOuts
--          in KVArray txIds txOuts'
--    in UTxOs {utxoMap = utxoWithSharing}

deriving instance Storable (TxId C)

deriving instance Storable (Keys.KeyHash kr C)

data StakeIx
  = StakeKeyHash !(Keys.KeyHash 'Shelley.Staking C)
  | StakeCredScript !(Shelley.ScriptHash C)
  | StakePtr !Ptr
  | StakeNull

instance NFData StakeIx where
  rnf = \case
    StakeKeyHash kh -> rnf kh
    StakeCredScript sh -> rnf sh
    StakePtr ptr -> rnf ptr
    StakeNull -> ()

data Addr'
  = AddrKeyHash' !Network !(Keys.KeyHash 'Shelley.Payment C) !StakeIx
  | AddrScript' !Network !(Shelley.ScriptHash C) !StakeIx
  | AddrBoot' !(CompactAddr C)

instance NFData Addr' where
  rnf = \case
    AddrKeyHash' !_ !kh si -> kh `deepseq` rnf si
    AddrScript' !_ !sh si -> sh `deepseq` rnf si
    AddrBoot' !_ -> ()

data TxOut'
  = TxOut' !Addr' !Word64
  | TxOutMA' !Addr' !Word64 !Word32 !ShortByteString
  | TxOutDH' !Addr' !Word64 !(DataHash C)
  | TxOutMADH' !Addr' !Word64 !Word32 !ShortByteString !(DataHash C)

instance NFData TxOut' where
  rnf = \case
    TxOut' a !_ -> rnf a
    TxOutMA' a !_ !_ !_ -> rnf a
    TxOutDH' a !_ dh -> a `deepseq` rnf dh
    TxOutMADH' a !_ !_ !_ dh -> a `deepseq` rnf dh


newtype UTxOs = UTxOs
  { utxoMap :: Vector (KV P BN) (KVPair Int (Vector (KV S BN) (KVPair (TxId C) TxOut')))
  }

instance NFData UTxOs where
  rnf UTxOs {..} = rnf utxoMap

lookupUTxOs :: TxIn C -> UTxOs -> Maybe (Alonzo.TxOut CurrentEra)
lookupUTxOs (TxIn txId txIx) UTxOs {..} = do
  txOut' <-
    lookupSortedKVArray txId =<< lookupSortedKVArray (fromIntegral txIx) utxoMap
  let toStakeRef =
        \case
          StakeKeyHash kh -> pure $ StakeRefBase $ KeyHashObj kh
          StakeCredScript sh -> pure $ StakeRefBase $ ScriptHashObj sh
          StakePtr ptr -> pure $ StakeRefPtr ptr
          StakeNull -> pure StakeRefNull
      toCompactAddr' =
        \case
          AddrKeyHash' ni kh stakeIx -> do
            sr <- toStakeRef stakeIx
            pure $ compactAddr $ Addr ni (KeyHashObj kh) sr
          AddrScript' ni sh stakeIx -> do
            sr <- toStakeRef stakeIx
            pure $ compactAddr $ Addr ni (ScriptHashObj sh) sr
          AddrBoot' bootAddr -> pure bootAddr
  case txOut' of
    TxOut' addr' ada -> do
      addr <- toCompactAddr' addr'
      let cv = Mary.CompactValue (Mary.CompactValueAdaOnly ada)
      pure $ Alonzo.TxOutCompact addr cv
    TxOutMA' addr' ada ma rep -> do
      addr <- toCompactAddr' addr'
      let cv = Mary.CompactValue (Mary.CompactValueMultiAsset ada ma rep)
      pure $ Alonzo.TxOutCompact addr cv
    TxOutDH' addr' ada dh -> do
      addr <- toCompactAddr' addr'
      let cv = Mary.CompactValue (Mary.CompactValueAdaOnly ada)
      pure $ Alonzo.TxOutCompactDH addr cv dh
    TxOutMADH' addr' ada ma rep dh -> do
      addr <- toCompactAddr' addr'
      let cv = Mary.CompactValue (Mary.CompactValueMultiAsset ada ma rep)
      pure $ Alonzo.TxOutCompactDH addr cv dh


printStats :: UTxOs -> IO ()
printStats UTxOs {..} = do
  putStrLn $
    unlines
      [ "Number of unique txIxs: " <> show (A.elemsCount utxoMap)
      ]
  A.mapM_ printUTxO $ utxoMap
  where
    printUTxO :: KVPair Int (Vector (KV S BN) (KVPair (TxId C) TxOut')) -> IO ()
    printUTxO (KVPair txIx v) =
      putStrLn $
      "<TxIx: " <> show txIx <> "> - TxOuts: " <> show (A.elemsCount v)

quicksortKVMArray_ ::
     (Manifest kr k, Manifest kv v, Ord k)
  => Scheduler RealWorld ()
  -> MVector RealWorld (KV kr kv) (KVPair k v)
  -> IO ()
quicksortKVMArray_ = quicksortByM_ (\(KVPair k1 _) (KVPair k2 _) -> pure (compare k1 k2))

fromMap :: (Manifest vr v, Manifest kr k) => Map.Map k v -> Vector (KV kr vr) (KVPair k v)
fromMap m = fromAscListN (Map.size m) $ Map.toAscList m

fromIntMap :: Manifest vr v => IntMap.IntMap v -> Vector (KV P vr) (KVPair Int v)
fromIntMap m = fromAscListN (IntMap.size m) $ IntMap.toAscList m

toIntMap :: Manifest vr v => Vector (KV P vr) (KVPair Int v) -> IntMap.IntMap v
toIntMap = IntMap.fromAscList . toAscList

fromAscList :: (Manifest vr v, Manifest kr k) => [(k, v)] -> Vector (KV kr vr) (KVPair k v)
fromAscList = A.compute . A.smap (\(k, v) -> KVPair k v) . A.sfromList

fromAscListN :: (Manifest vr v, Manifest kr k) => Int -> [(k, v)] -> Vector (KV kr vr) (KVPair k v)
fromAscListN n = A.compute . A.smap (\(k, v) -> KVPair k v) . A.sfromListN (Sz n)

toAscList :: (Manifest vr v, Manifest kr k) => Vector (KV kr vr) (KVPair k v) -> [(k, v)]
toAscList = A.toList . A.map (\(KVPair k v) -> (k, v))




lookupIxSortedArray ::
  (Manifest kr k, Ord k) => k -> Vector kr k -> Maybe Ix1
lookupIxSortedArray key keys = go 0 (elemsCount keys)
  where
    go !l !u = do
      guard (l < u)
      let !i = ((u - l) `div` 2) + l
      key' <- indexM keys i
      case compare key key' of
        LT -> go l i
        GT -> go (i + 1) u
        EQ -> Just i

lookupSortedKVArray ::
  (Manifest kr k, Manifest vr v, Ord k) => k -> Vector (KV kr vr) (KVPair k v) -> Maybe v
lookupSortedKVArray key (KVArray keys values) = do
  i <- lookupIxSortedArray key keys
  indexM values i


data KV kr vr = KV !kr !vr

data KVPair k v = KVPair !k !v

type family KVValue e :: Type

type family KVKey e :: Type

type instance KVKey (KVPair k v) = k

type instance KVValue (KVPair k v) = v

data instance Array (KV kr vr) ix e = KVArray
  { keysArray :: !(Array kr ix (KVKey e)),
    valsArray :: !(Array vr ix (KVValue e))
  }

instance (NFData (Array kr ix k), (NFData (Array kv ix v))) =>
         NFData (Array (KV kr kv) ix (KVPair k v)) where
  rnf KVArray {..} = keysArray `deepseq` valsArray `deepseq` ()

instance (Size kr, Size vr) => Size (KV kr vr) where
  size (KVArray k _) = size k
  unsafeResize sz (KVArray k v) = KVArray (unsafeResize sz k) (unsafeResize sz v)

instance (Strategy kr, Strategy vr) => Strategy (KV kr vr) where
  setComp c (KVArray k v) = KVArray (setComp c k) (setComp c v)
  getComp (KVArray k v) = getComp k <> getComp v

instance (Source kr k, Source vr v) => Source (KV kr vr) (KVPair k v) where
  unsafeLinearIndex (KVArray keys vals) ix =
    KVPair (unsafeLinearIndex keys ix) (unsafeLinearIndex vals ix)
  {-# INLINE unsafeLinearIndex #-}
  unsafeLinearSlice ix sz (KVArray keys vals) =
    KVArray (unsafeLinearSlice ix sz keys) (unsafeLinearSlice ix sz vals)
  {-# INLINE unsafeLinearSlice #-}

data instance MArray s (KV kr vr) ix e = KVMArray
  { keysMArray :: !(MArray s kr ix (KVKey e)),
    valsMArray :: !(MArray s vr ix (KVValue e))
  }

instance (Manifest kr k, Manifest vr v) => Manifest (KV kr vr) (KVPair k v) where
  unsafeLinearIndexM (KVArray keys vals) ix =
    KVPair (unsafeLinearIndexM keys ix) (unsafeLinearIndexM vals ix)
  {-# INLINE unsafeLinearIndexM #-}

  sizeOfMArray (KVMArray k _) = sizeOfMArray k
  {-# INLINE sizeOfMArray #-}

  unsafeResizeMArray sz (KVMArray k v) =
    KVMArray (unsafeResizeMArray sz k) (unsafeResizeMArray sz v)
  {-# INLINE unsafeResizeMArray #-}

  unsafeLinearSliceMArray ix sz (KVMArray keys vals) =
    KVMArray (unsafeLinearSliceMArray ix sz keys) (unsafeLinearSliceMArray ix sz vals)
  {-# INLINE unsafeLinearSliceMArray #-}

  unsafeThaw (KVArray k v) = KVMArray <$> unsafeThaw k <*> unsafeThaw v
  {-# INLINE unsafeThaw #-}

  unsafeFreeze comp (KVMArray k v) =
    KVArray <$> unsafeFreeze comp k <*> unsafeFreeze comp v
  {-# INLINE unsafeFreeze #-}

  unsafeNew sz = KVMArray <$> unsafeNew sz <*> unsafeNew sz
  {-# INLINE unsafeNew #-}

  unsafeLinearRead (KVMArray keys vals) ix =
    KVPair <$> unsafeLinearRead keys ix <*> unsafeLinearRead vals ix
  {-# INLINE unsafeLinearRead #-}

  unsafeLinearWrite (KVMArray keys vals) ix (KVPair k v) = do
    unsafeLinearWrite keys ix k
    unsafeLinearWrite vals ix v
  {-# INLINE unsafeLinearWrite #-}

  initialize (KVMArray keys vals) = initialize keys >> initialize vals
  {-# INLINE initialize #-}

  unsafeLinearSet (KVMArray keys vals) offset len (KVPair k v) = do
    unsafeLinearSet keys offset len k
    unsafeLinearSet vals offset len v
  {-# INLINE unsafeLinearSet #-}

  unsafeLinearCopy (KVMArray keysFrom valsFrom) iFrom (KVMArray keysTo valsTo) iTo n = do
    unsafeLinearCopy keysFrom iFrom keysTo iTo n
    unsafeLinearCopy valsFrom iFrom valsTo iTo n
  {-# INLINE unsafeLinearCopy #-}

  unsafeArrayLinearCopy (KVArray keysFrom valsFrom) iFrom (KVMArray keysTo valsTo) iTo n = do
    unsafeArrayLinearCopy keysFrom iFrom keysTo iTo n
    unsafeArrayLinearCopy valsFrom iFrom valsTo iTo n
  {-# INLINE unsafeArrayLinearCopy #-}

  unsafeLinearShrink (KVMArray keys vals) sz =
    KVMArray <$> unsafeLinearShrink keys sz <*> unsafeLinearShrink vals sz
  {-# INLINE unsafeLinearShrink #-}

  unsafeLinearGrow (KVMArray keys vals) sz =
    KVMArray <$> unsafeLinearGrow keys sz <*> unsafeLinearGrow vals sz
  {-# INLINE unsafeLinearGrow #-}


