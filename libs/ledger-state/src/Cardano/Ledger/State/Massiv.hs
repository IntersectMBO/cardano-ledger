
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}

module Cardano.Ledger.State.Massiv where

import Cardano.Ledger.State.UTxO
import Control.Monad
import Cardano.Ledger.Compactible
import Data.ByteString.Short
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
import Cardano.Ledger.Credential
import Cardano.Ledger.Crypto
import Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Hashes as Hashes
import qualified Cardano.Ledger.Keys as Keys
import qualified Cardano.Ledger.Mary.Value as Mary
import qualified Cardano.Ledger.SafeHash as SafeHash
import qualified Cardano.Ledger.Shelley.API as Shelley
import Cardano.Ledger.Shelley.CompactAddr
import Cardano.Ledger.Shelley.TxBody
import Cardano.Ledger.Shelley.UTxO
import Conduit
import Data.Aeson as Aeson
import Data.Aeson.Parser as Aeson
import Data.Aeson.Types as Aeson
import Data.Attoparsec.ByteString as Atto
import qualified Data.Attoparsec.ByteString.Char8 as Atto8
import qualified Data.ByteString.Base16 as B16
import Data.Conduit.Attoparsec
import qualified Data.Conduit.List as C
import qualified Data.Map.Strict as Map
import qualified Data.IntMap.Strict as IntMap
import Data.Proxy
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Read as T

import Data.Massiv.Array as A
import Data.Massiv.Array.Unsafe as A
import Data.Massiv.Array.Mutable.Algorithms as A


newtype UTxOv = UTxOv (Vector B (TxIn C, Alonzo.TxOut CurrentEra))

newtype UTxOv' = UTxOv' (Vector B (TxId C, IntMap.IntMap TxOut'))

loadMassivUTxO :: FilePath -> IO (ArrayMap (TxIn C) (Alonzo.TxOut CurrentEra))
loadMassivUTxO fp = do
  da <-
    consumeUTxO fp $
    foldlC (\a (!txIn, !txOut) -> A.cons (MapElt txIn txOut) a) A.empty
  vec <-
    withLoadMArray_ da $
    quicksortByM_
      (\(MapElt txIn1 _) (MapElt txIn2 _) -> pure (compare txIn1 txIn2))
  pure $ ArrayMap vec


-- loadMassivUTxO' :: FilePath -> IO (ArrayMap (TxIn C) TxOut')
-- loadMassivUTxO' fp = do
--   ma <- newMArray' (Sz1 3272356)
--   _ <- consumeUTxO fp $
--     foldMC (\i (!txIn, !txOut) -> (i + 1) <$ write_ ma i (MapElt txIn txOut)) 0
--   withMassivScheduler_ Par $ \ scheduler ->
--     quicksortByM_ (\(MapElt txIn1 _) (MapElt txIn2 _) ->
--                      pure (compare txIn1 txIn2)) scheduler ma
--   ArrayMap <$> A.unsafeFreeze Par ma
data CredRef
  = CredKeyRef !(Keys.KeyHash 'Shelley.Witness C)
  | CredScriptRef !(Shelley.ScriptHash C)
  deriving Eq

data CredIx
  = CredKeyIx !Ix1
  | CredScriptIx !Ix1
  deriving Eq

data StakeIx = StakeIx !CredIx
             | StakePtr !Ptr
             | StakeNull

data Addr'
  = AddrBoot' !(CompactAddr C)
  | Addr' !Network !CredIx !StakeIx

data TxOut'
  = TxOut' !Addr' !(CompactForm (Mary.Value C))
  | TxOutDH' !Addr' !(CompactForm (Mary.Value C)) !(DataHash C)


data UTxOs = UTxOs
  { utxoMap :: !(ArrayMap (TxId C) (IntMap.IntMap TxOut'))
  , utxoKeyHashSet :: !(RefSet (Keys.KeyHash 'Shelley.Witness C))
  , utxoScriptHashSet :: !(RefSet (Shelley.ScriptHash C))
  , utxoDataHashSet :: !(RefSet (DataHash C))
  }

loadMassivUTxOv' ::
     ArrayMap (TxIn C) TxOut' -> ArrayMap (TxId C) (IntMap.IntMap TxOut')
loadMassivUTxOv' (ArrayMap v) = do
  let
    collect (i, Nothing) = do
      e <- v !? i
      collect (i + 1, Just e)
    collect (i, Just (MapElt (TxIn txId txIx) txOut)) =
      collectForward i txId $! IntMap.singleton (fromIntegral txIx) txOut
    collectForward !i !txId !m =
      case v !? i of
        Nothing -> Just (MapElt txId m, (i, Nothing))
        Just e@(MapElt (TxIn txId' txIx') txOut')
          | txId' == txId ->
            collectForward (i + 1) txId $! IntMap.insert (fromIntegral txIx') txOut' m
          | otherwise -> Just (MapElt txId m, (i + 1, Just e))
  ArrayMap $ compute (sunfoldrN (size v) collect (0, Nothing))




loadMassivUTxOs :: FilePath -> IO UTxOs
loadMassivUTxOs fp = do
  ArrayMap vTxOut <- loadMassivUTxO fp
  A.map
  u <-
    case foldrS convertToRef (mempty, mempty, mempty, mempty) vTxOut of
      (!khMap, !shMap, !dhMap, vec) -> do
        putStrLn "Done to folding, starting to compute"
        arrMap <- computeIO vec
        putStrLn "Computed"
        pure $! UTxOs
          { utxoMap = loadMassivUTxOv' $ ArrayMap arrMap
          , utxoKeyHashSet = refSetFromMap khMap
          , utxoScriptHashSet = refSetFromMap shMap
          , utxoDataHashSet = refSetFromMap dhMap
          }
  pure u
  where
    getCredential = \case
        KeyHashObj kh -> (Just (Keys.coerceKeyRole kh), Nothing)
        ScriptHashObj sh -> (Nothing, Just sh)
    getRefs (MapElt txIn txOut) =
      let (!cAddr', !cVal', !mDataHash) =
            case txOut of
              Alonzo.TxOutCompact cAddr cVal -> (cAddr, cVal, Nothing)
              Alonzo.TxOutCompactDH cAddr cVal dh -> (cAddr, cVal, Just dh)
          !addr' =
            case decompactAddr cAddr' of
              AddrBootstrap _ -> (AddrBoot' cAddr', Nothing, Nothing, Nothing, Nothing)
              Addr ni pc sr ->
                let mkAddr' = Addr' ni pcRef
                    (pck, pcs) = getCredential pc
                 in case sr of
                      StakeRefBase cred -> getCredential cred
                        | (!credRef, !khmap', !shmap') <- insertCredential cred khmap shmap ->
                          (mkAddr' (StakeRef credRef), khmap', shmap')
                      StakeRefPtr ptr -> (mkAddr' (StakePtr ptr), khmap, shmap)
                      StakeRefNull -> (mkAddr' StakeNull, khmap, shmap)
          !elt = MapElt txIn (mkTxOut' addr')
          vec' :: Vector DL (MapElt (TxIn C) TxOut')
          vec' = cons elt vec
       in (khMap', shMap', dhMap', vec')



data MapElt k v =
  MapElt
    { mapEltKey :: !k
    , mapEltValue :: !v
    }

newtype ArrayMap k v =
  ArrayMap
    { unArrayMap :: Vector B (MapElt k v)
    }

lookupArrayMap :: Ord k => k -> ArrayMap k v -> Maybe v
lookupArrayMap k (ArrayMap vec) =
  mapEltValue <$> lookupSortedOn mapEltKey k vec


data SetElt v =
  SetElt
    { setElt :: !v
    , setEltCounter :: !Int
    }

newtype RefSet v =
  RefSet
    { unRefSet :: Vector B (SetElt v)
    }


refSetFromMap :: Map.Map v Int -> RefSet v
refSetFromMap m =
  RefSet $
  compute $
  smap (uncurry SetElt) $ sfromListN (Sz1 (Map.size m)) $ Map.toAscList m


insertRefMap :: Ord v => v -> Map.Map v Int -> (v, Map.Map v Int)
insertRefMap v m =
  case Map.lookupIndex v m of
    Nothing -> (v, Map.insert v 1 m)
    Just i
      | (v', _) <- Map.elemAt i m -> (v', Map.adjust (+ 1) v' m)

lookupRefSet :: Ord v => v -> RefSet v -> Maybe v
lookupRefSet k (RefSet vec) = setEltRef <$> lookupSortedOn setEltRef k vec

lookupSortedIxOn :: (Manifest r a, Ord b) => (a -> b) -> b -> Vector r a -> Maybe Ix1
lookupSortedIxOn f e v = go (k `div` 2) k
  where
    Sz k = size v
    go !i !n = do
      guard (i /= n)
      b <- indexM v i
      case compare e (f b) of
        LT -> go (i `div` 2) i
        GT -> go ((n - i) `div` 2) n
        EQ -> Just i


-- loadMassivUTxO'' :: FilePath -> IO UTxOv
-- loadMassivUTxO'' fp = do
--   ma <- newMArray' (Sz1 3272356)
--   _ <- consumeUTxO fp (foldMC (\i e@(!_, !_) -> (i + 1) <$ write_ ma i e) 0)
--   rebalanceM_ ma
--   UTxOv <$> A.unsafeFreeze Par ma


-- loadMassivUTxOs :: FilePath -> IO UTxOs
-- loadMassivUTxOs fp = do
--   ArrayMap vTxOut <- loadMassivUTxO fp
--   putStrLn "Starting to fold"
--   u <-
--     case foldrS convertToRef (mempty, mempty, mempty, mempty) vTxOut of
--       (!khMap, !shMap, !dhMap, vec) -> do
--         putStrLn "Done to folding, starting to compute"
--         arrMap <- computeIO vec
--         putStrLn "Computed"
--         pure $! UTxOs
--           { utxoMap = loadMassivUTxOv' $ ArrayMap arrMap
--           , utxoKeyHashSet = refSetFromMap khMap
--           , utxoScriptHashSet = refSetFromMap shMap
--           , utxoDataHashSet = refSetFromMap dhMap
--           }
--   putStrLn $ unlines
--     [ "utxoMap: " <> show (A.size (unArrayMap (utxoMap u)))
--     , "utxoKeyHashSet: " <> show (A.size (unRefSet (utxoKeyHashSet u)))
--     , "utxoScriptHashSet: " <> show (A.size (unRefSet (utxoScriptHashSet u)))
--     , "utxoDataHashSet: " <> show (A.size (unRefSet (utxoDataHashSet u)))
--     ]
--   pure u
--   where
--     insertCredential cred khmap shmap =
--       case cred of
--         KeyHashObj kh
--           | (khRef, khmap') <- insertRefMap (Keys.coerceKeyRole kh) khmap ->
--             (CredKeyRef khRef, khmap', shmap)
--         ScriptHashObj sh
--           | (shRef, shmap') <- insertRefMap sh shmap ->
--             (CredScriptRef shRef, khmap, shmap')
--     convertToRef ::
--          MapElt (TxIn C) (Alonzo.TxOut CurrentEra)
--       -> ( Map.Map (Shelley.KeyHash 'Shelley.Witness C) Int
--          , Map.Map (Shelley.ScriptHash C) Int
--          , Map.Map (DataHash C) Int
--          , Vector DL (MapElt (TxIn C) TxOut'))
--       -> ( Map.Map (Shelley.KeyHash 'Shelley.Witness C) Int
--          , Map.Map (Shelley.ScriptHash C) Int
--          , Map.Map (DataHash C) Int
--          , Vector DL (MapElt (TxIn C) TxOut'))
--     convertToRef (MapElt txIn txOut) (!khMap, !shMap, !dhMap, vec) =
--       let (!cAddr', !mkTxOut', !dhMap') =
--             case txOut of
--               Alonzo.TxOutCompact cAddr cVal ->
--                 (cAddr, \a -> TxOut' a cVal, dhMap)
--               Alonzo.TxOutCompactDH cAddr cVal dh
--                 | (dhRef, dhmap) <- insertRefMap dh dhMap ->
--                   (cAddr, \a -> TxOutDH' a cVal dhRef, dhmap)
--           (!addr', !khMap', !shMap') =
--             case decompactAddr cAddr' of
--               AddrBootstrap _ -> (AddrBoot' cAddr', khMap, shMap)
--               Addr ni pc sr ->
--                 let (!pcRef, !khmap, !shmap) = insertCredential pc khMap shMap
--                     mkAddr' = Addr' ni pcRef
--                  in case sr of
--                       StakeRefBase cred
--                         | (!credRef, !khmap', !shmap') <- insertCredential cred khmap shmap ->
--                           (mkAddr' (StakeRef credRef), khmap', shmap')
--                       StakeRefPtr ptr -> (mkAddr' (StakePtr ptr), khmap, shmap)
--                       StakeRefNull -> (mkAddr' StakeNull, khmap, shmap)
--           !elt = MapElt txIn (mkTxOut' addr')
--           vec' :: Vector DL (MapElt (TxIn C) TxOut')
--           vec' = cons elt vec
--        in (khMap', shMap', dhMap', vec')
