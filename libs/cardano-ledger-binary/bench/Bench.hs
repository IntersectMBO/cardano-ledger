{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Cardano.Ledger.Binary hiding (serialize)
import Cardano.Ledger.Binary.Coders
import Control.DeepSeq
import Control.Monad
import Criterion.Main
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import System.Random.Stateful

data Serialized a = Serialized !Version !BS.ByteString

instance NFData (Serialized a) where
  rnf (Serialized v bs) = v `deepseq` rnf bs

serialize :: Version -> (a -> Encoding) -> a -> Serialized a
serialize v e t = Serialized v (serialize' v (e t))

deserialize :: (forall s. Decoder s a) -> Serialized a -> a
deserialize d (Serialized v bs) =
  case decodeFullDecoder' v "Bench" d bs of
    Left err -> error $ "Unexpected deserialization error: " ++ show err
    Right val -> val

deserializeA :: (forall s. Decoder s (Annotator a)) -> Serialized a -> a
deserializeA d (Serialized v bs) =
  case decodeFullAnnotator v "Bench" d (BSL.fromStrict bs) of
    Left err -> error $ "Unexpected deserialization error: " ++ show err
    Right val -> val

main :: IO ()
main = do
  let
    sz = 100000
    stdGen = mkStdGen 2023
    unifomMapM :: StatefulGen g m => Int -> g -> m (Map Integer Integer)
    unifomMapM mapSize gen =
      let largeInteger = uniformRM (0, 100000000 * toInteger (maxBound :: Word)) gen
       in Map.fromList <$> replicateM mapSize ((,) <$> largeInteger <*> largeInteger)
    uniformMapIO n = runStateGenT_ stdGen (unifomMapM n)

  defaultMain
    [ bgroup "Map" (benchMap (uniformMapIO sz))
    , bgroup "Set" (benchSet (Map.keysSet <$> uniformMapIO sz))
    ]

benchMap ::
  (EncCBOR a, EncCBOR b, NFData a, NFData b, Ord a, DecCBOR a, DecCBOR b) =>
  IO (Map a b) ->
  [Benchmark]
benchMap mkUniformMap =
  [ bgroup
      "decodeMap"
      [ env (serialize (natVersion @1) encCBOR <$> mkUniformMap) $
          bench "Byron" . nf (deserialize decCBOR)
      , env (serialize (natVersion @2) encCBOR <$> mkUniformMap) $
          bench "Shelley" . nf (deserialize decCBOR)
      , env (serialize (natVersion @9) encCBOR <$> mkUniformMap) $
          bench "Conway" . nf (deserialize decCBOR)
      , env (serialize (natVersion @9) encCBOR <$> mkUniformMap) $
          bench "Older" . nf (deserialize decodeMapCheckDuplicates)
      ]
  , bgroup
      "Coders"
      [ env (serialize (natVersion @2) encCBOR <$> mkUniformMap) $
          bench "D Shelley" . nf (deserialize (decode From))
      , env (serialize (natVersion @9) encCBOR <$> mkUniformMap) $
          bench "D Conway" . nf (deserialize (decode From))
      ]
  , bgroup
      "mapDecodeA"
      [ env (serialize (natVersion @2) encCBOR <$> mkUniformMap) $
          bench "Shelley"
            . nf (deserializeA (decode (mapDecodeA (D (pure <$> decCBOR)) (D (pure <$> decCBOR)))))
      , env (serialize (natVersion @9) encCBOR <$> mkUniformMap) $
          bench "Conway"
            . nf (deserializeA (decode (mapDecodeA (D (pure <$> decCBOR)) (D (pure <$> decCBOR)))))
      ]
  ]
  where
    -- older implementation that checked duplicates for each element and used an insert
    decodeMapCheckDuplicates = do
      let decodeInlinedPair m = do
            !key <- decCBOR
            when (key `Map.member` m) $ fail "Duplicate detected"
            !value <- decCBOR
            pure (key, value)
      snd <$> decodeListLikeWithCount decodeMapLenOrIndef (uncurry Map.insert) decodeInlinedPair

benchSet ::
  (EncCBOR a, NFData a, Ord a, DecCBOR a) =>
  IO (Set a) ->
  [Benchmark]
benchSet mkUniformSet =
  [ bgroup
      "decodeSet"
      [ env (serialize (natVersion @1) encCBOR <$> mkUniformSet) $
          bench "Byron" . nf (deserialize decCBOR)
      , env (serialize (natVersion @2) encCBOR <$> mkUniformSet) $
          bench "Shelley" . nf (deserialize decCBOR)
      , env (serialize (natVersion @9) encCBOR <$> mkUniformSet) $
          bench "Conway" . nf (deserialize decCBOR)
      , env (serialize (natVersion @9) encCBOR <$> mkUniformSet) $
          bench "Older" . nf (deserialize decodeSetCheckDuplicates)
      ]
  , bgroup
      "Coders"
      [ env (serialize (natVersion @2) encCBOR <$> mkUniformSet) $
          bench "D Shelley" . nf (deserialize (decode From))
      , env (serialize (natVersion @9) encCBOR <$> mkUniformSet) $
          bench "D Conway" . nf (deserialize (decode From))
      ]
  , bgroup
      "mapDecodeA"
      [ env (serialize (natVersion @2) encCBOR <$> mkUniformSet) $
          bench "Shelley"
            . nf (deserializeA (decode (setDecodeA (D (pure <$> decCBOR)))))
      , env (serialize (natVersion @9) encCBOR <$> mkUniformSet) $
          bench "Conway"
            . nf (deserializeA (decode (setDecodeA (D (pure <$> decCBOR)))))
      ]
  ]
  where
    -- older implementation that checked duplicates for each element and used an insert
    decodeSetCheckDuplicates = do
      let decodeInlinedPair m = do
            !value <- decCBOR
            when (value `Set.member` m) $ fail "Duplicate detected"
            pure value
      allowTag setTag
      snd <$> decodeListLikeWithCount decodeListLenOrIndef Set.insert decodeInlinedPair
