{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Ledger.State.Massiv where

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

loadMassivUTxO ::
  FilePath ->
  IO UTxOs
loadMassivUTxO fp = do
  let consTxOut txId txOut Nothing = Just $! A.singleton $ KVPair txId txOut
      consTxOut txId txOut (Just a) = Just $! A.cons (KVPair txId txOut) a
  das <-
    consumeUTxO fp $
      C.foldlC
        ( \im (!(TxIn txId txIx), !txOut) ->
            IntMap.alter (consTxOut txId txOut) (fromIntegral txIx) im
        )
        mempty
  constructMassivUTxO <$> traverse (\da -> withLoadMArray_ da quicksortKVMArray_) das

shareMassivUTxO ::
  IntMap.IntMap (Vector (KV S B) (KVPair (TxId C) (Alonzo.TxOut CurrentEra))) ->
  Vector (KV S S) (KVPair (Keys.KeyHash 'Shelley.Witness C) Word32)
shareMassivUTxO im =
  let extractKeyHash (KeyHashObj kh) = [Keys.asWitness kh]
      extractKeyHash _ = []
      extractKeyHashes cAddr =
        case decompactAddr cAddr of
          AddrBootstrap _ -> []
          Addr _ pc sr
            | StakeRefBase cred <- sr ->
              extractKeyHash pc <> extractKeyHash cred
            | otherwise -> extractKeyHash pc
      collectKeys =
        foldMono $ \(KVPair _ txOut) ->
          case txOut of
            Alonzo.TxOutCompact cAddr _ -> extractKeyHashes cAddr
            Alonzo.TxOutCompactDH cAddr _ _ -> extractKeyHashes cAddr
   in A.compute $
        A.smap (\(k, c) -> KVPair k (fromIntegral c)) $
          A.tally
            (A.compute $ sfromList (foldMap collectKeys im) :: Vector S (Keys.KeyHash 'Shelley.Witness C))

collectSharedKeys ::
  IntMap.IntMap (Vector (KV S B) (KVPair (TxId C) TxOut')) ->
  Vector (KV S S) (KVPair (Keys.KeyHash 'Shelley.Witness C) Word32)
collectSharedKeys im =
  let extractKeyHashes !accVec =
        \case
          AddrKeyIx' _ _ (StakeKeyHash skh) -> A.cons (Keys.asWitness skh) accVec
          AddrKeyHash' _ kh (StakeKeyHash skh) ->
            A.cons (Keys.asWitness kh) (A.cons (Keys.asWitness skh) accVec)
          AddrScript' _ _ (StakeKeyHash skh) -> A.cons (Keys.asWitness skh) accVec
          _ -> accVec
      collectKeys !accVec =
        \case
          TxOut' addr _ -> extractKeyHashes accVec addr
          TxOutDH' addr _ _ -> extractKeyHashes accVec addr
      collectHashes !a (KVArray _ vals) = A.foldlS collectKeys a vals
      vectorWithKeyHashes :: Vector S (Keys.KeyHash 'Shelley.Witness C)
      vectorWithKeyHashes =
        A.compute $ IntMap.foldl' collectHashes A.empty im
   in A.compute $
        A.smap (\(k, c) -> KVPair k (fromIntegral c)) $ A.tally vectorWithKeyHashes


constructMassivUTxO ::
  (IntMap.IntMap (Vector (KV S B) (KVPair (TxId C) (Alonzo.TxOut CurrentEra)))) ->
  UTxOs
constructMassivUTxO im =
  let !sharedKeyHashes =
        trace "done creating sharedKeyHashes" $ collectSharedKeys utxoNoSharing
      lookupKeyHashIx :: Shelley.KeyHash kr C -> Maybe Ix1
      lookupKeyHashIx kh = lookupIxSortedKVArray (Keys.asWitness kh) sharedKeyHashes
      !utxoWithSharing = IntMap.map applySharing utxoNoSharing
      applySharing (KVArray keys values) =
        let !values' = A.compute $ A.map applyTxOutSharing values
            applyTxOutSharing =
              \case
                TxOut' addr cVal -> TxOut' (applyAddrSharing addr) cVal
                TxOutDH' addr cVal mData -> TxOutDH' (applyAddrSharing addr) cVal mData
            applyStakeSharing =
              \case
                StakeKeyHash skh
                  | Just skhix <- lookupKeyHashIx skh -> StakeKeyIx skhix
                s -> s
            applyAddrSharing =
              \case
                AddrKeyIx' ni khix s -> AddrKeyIx' ni khix $ applyStakeSharing s
                AddrScript' ni sh s -> AddrScript' ni sh $ applyStakeSharing s
                AddrKeyHash' ni kh s
                  | Just khix <- lookupKeyHashIx kh ->
                    AddrKeyIx' ni khix $ applyStakeSharing s
                  | otherwise -> AddrKeyHash' ni kh $ applyStakeSharing s
                addr -> addr
         in KVArray keys values'
      !utxoNoSharing = IntMap.map applyRestructure im
      applyRestructure (KVArray keys values) =
        let !values' = A.compute $ A.map restructureTxOut values
            restructureTxOut =
              \case
                Alonzo.TxOutCompact cAddr cVal ->
                  TxOut' (restructureAddr cAddr) cVal
                Alonzo.TxOutCompactDH cAddr cVal mData ->
                  TxOutDH' (restructureAddr cAddr) cVal mData
            restructureAddr cAddr =
              case decompactAddr cAddr of
                AddrBootstrap _ -> AddrBoot' cAddr
                Addr ni pc sr
                  | KeyHashObj kh <- pc ->
                    AddrKeyHash' ni kh $ restructureStakeIx sr
                  | ScriptHashObj sh <- pc ->
                    AddrScript' ni sh $ restructureStakeIx sr
            restructureStakeIx =
              \case
                StakeRefBase (KeyHashObj kh) -> StakeKeyHash kh
                StakeRefBase (ScriptHashObj sh) -> StakeCredScript sh
                StakeRefPtr ptr -> StakePtr ptr
                StakeRefNull -> StakeNull
         in KVArray keys values'
   in UTxOs {utxoMap = utxoWithSharing, utxoSharedKeyHashes = sharedKeyHashes}

deriving instance Storable (TxId C)

deriving instance Storable (Keys.KeyHash kr C)

data StakeIx
  = StakeKeyIx !Ix1
  | StakeKeyHash !(Keys.KeyHash 'Shelley.Staking C)
  | StakeCredScript !(Shelley.ScriptHash C)
  | StakePtr !Ptr
  | StakeNull

data Addr'
  = AddrKeyIx' !Network !Ix1 !StakeIx
  | AddrKeyHash' !Network !(Keys.KeyHash 'Shelley.Payment C) !StakeIx
  | AddrScript' !Network !(Shelley.ScriptHash C) !StakeIx
  | AddrBoot' !(CompactAddr C)

data TxOut'
  = TxOut' !Addr' !(CompactForm (Mary.Value C))
  | TxOutDH' !Addr' !(CompactForm (Mary.Value C)) !(DataHash C)

data UTxOs = UTxOs
  { utxoMap :: !(IntMap.IntMap (Vector (KV S B) (KVPair (TxId C) TxOut'))),
    -- | Set of all key hashes and their usage counter.
    utxoSharedKeyHashes :: !(Vector (KV S S) (KVPair (Keys.KeyHash 'Shelley.Witness C) Word32))
  }

printStats :: UTxOs -> IO ()
printStats UTxOs {..} = do
  putStrLn $
    unlines
      [ "KeyHashSet size: " <> show (A.elemsCount utxoSharedKeyHashes),
        "Max shares: " <> show (A.stake 40 (quicksortBy (\x y -> compare y x) (valsArray utxoSharedKeyHashes))),
        "Number of unique txIxs: " <> show (IntMap.size utxoMap)
      ]
  Prelude.mapM_ printUTxO $ IntMap.toAscList utxoMap
  where
    printUTxO :: (Int, Vector (KV S B) (KVPair (TxId C) TxOut')) -> IO ()
    printUTxO (txIx, v) =
      putStrLn $ "<TxIx: " <> show txIx <> "> - TxOuts: " <> show (A.elemsCount v)

quicksortKVMArray_ ::
  Scheduler RealWorld () ->
  MVector RealWorld (KV S B) (KVPair (TxId C) (Alonzo.TxOut CurrentEra)) ->
  IO ()
quicksortKVMArray_ = quicksortByM_ (\(KVPair k1 _) (KVPair k2 _) -> pure (compare k1 k2))

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

lookupIxSortedKVArray ::
  (Manifest kr k, Ord k) => k -> Vector (KV kr vr) (KVPair k v) -> Maybe Ix1
lookupIxSortedKVArray key (KVArray keys _) = go 0 (elemsCount keys)
  where
    go !l !u = do
      guard (False && l < u)
      let !i = ((u - l) `div` 2) + l
      key' <- indexM keys i
      case compare key key' of
        LT -> go l i
        GT -> go (i + 1) u
        EQ -> Just i

-- getIxSortedKVArray ::
--   (Manifest kr a, Ord a, Show a) =>
--   a ->
--   Vector (KV kr vr) (KVPair a v) ->
--   Ix1
-- getIxSortedKVArray key arr =
--   case lookupIxSortedKVArray key arr of
--     Nothing -> error $ "Cannot find index for key: " ++ show key
--     Just i -> i

-- Using lists, seems to be more memory hungry
-- collectSharedKeys ::
--   IntMap.IntMap (Vector (KV S B) (KVPair (TxId C) TxOut')) ->
--   Vector (KV S S) (KVPair (Keys.KeyHash 'Shelley.Witness C) Word32)
-- collectSharedKeys im =
--   let extractKeyHashes !accVec =
--         \case
--           AddrKeyIx' _ _ (StakeKeyHash skh) -> Keys.asWitness skh : accVec
--           AddrKeyHash' _ kh (StakeKeyHash skh) ->
--             let !accVec' = Keys.asWitness skh : accVec
--             in Keys.asWitness kh : accVec'
--           AddrScript' _ _ (StakeKeyHash skh) -> Keys.asWitness skh : accVec
--           _ -> accVec
--       collectKeys !accVec =
--         \case
--           TxOut' addr _ -> extractKeyHashes accVec addr
--           TxOutDH' addr _ _ -> extractKeyHashes accVec addr
--       collectHashes !a (KVArray _ vals) = A.foldlS collectKeys a vals
--       vectorWithKeyHashes :: Vector S (Keys.KeyHash 'Shelley.Witness C)
--       vectorWithKeyHashes = A.compute $ sfromList $ IntMap.foldl' collectHashes [] im
--    in A.compute $
--         A.smap (\(k, c) -> KVPair k (fromIntegral c)) $ A.tally vectorWithKeyHashes
