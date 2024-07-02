{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Cardano.Ledger.Address
import Cardano.Ledger.Api.Era
import Cardano.Ledger.Api.State.Query (queryStakePoolDelegsAndRewards)
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Binary
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley.API.Mempool
import Cardano.Ledger.Shelley.API.Wallet (getFilteredUTxO, getUTxO)
import Cardano.Ledger.Shelley.Genesis (
  ShelleyGenesis (..),
  fromNominalDiffTimeMicro,
  mkShelleyGlobals,
 )
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.State.UTxO (CurrentEra, readNewEpochState)
import Cardano.Ledger.UMap
import Cardano.Ledger.UTxO
import Cardano.Ledger.Val
import Cardano.Slotting.EpochInfo (fixedEpochInfo)
import Cardano.Slotting.Time (mkSlotLength)
import Control.DeepSeq
import Criterion.Main
import Data.Aeson
import Data.Bifunctor (first)
import Data.ByteString.Base16.Lazy as BSL16
import Data.ByteString.Lazy (ByteString)
import Data.Default.Class (def)
import Data.Foldable as F
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.MapExtras (extractKeys, extractKeysSmallSet)
import Data.Set (Set)
import qualified Data.Set as Set
import Lens.Micro ((^.))
import System.Environment (getEnv)
import System.Random.Stateful
import Test.Cardano.Ledger.Api.State.Query (getFilteredDelegationsAndRewardAccounts)
import Test.Cardano.Ledger.Core.Arbitrary (uniformSubset)

main :: IO ()
main = do
  let ledgerVarName = "BENCH_LEDGER_STATE_PATH"
      genesisVarName = "BENCH_GENESIS_PATH"
  ledgerStateFilePath <- getEnv ledgerVarName
  genesisFilePath <- getEnv genesisVarName
  genesis <- either error id <$> eitherDecodeFileStrict' genesisFilePath

  let toMempoolState :: NewEpochState CurrentEra -> MempoolState CurrentEra
      toMempoolState NewEpochState {nesEs = EpochState {esLState}} = esLState
      pp :: PParams CurrentEra
      pp = def
      !globals = mkGlobals genesis pp
      !slotNo = SlotNo 55733343
      applyTx' mempoolEnv mempoolState =
        either (error . show) seqTuple
          . applyTx globals mempoolEnv mempoolState
      reapplyTx' mempoolEnv mempoolState tx =
        case reapplyTx globals mempoolEnv mempoolState tx of
          Left err -> error (show err)
          Right st -> st
  putStrLn $ "Importing NewEpochState from: " ++ show ledgerStateFilePath
  es <- readNewEpochState ledgerStateFilePath
  putStrLn "Done importing NewEpochState"
  let largeKeysNum = 100000
      stdGen = mkStdGen 2022
  largeKeys <- selectRandomMapKeys 100000 stdGen (unUTxO (getUTxO es))
  defaultMain
    [ env (pure (mkMempoolEnv es slotNo, toMempoolState es)) $ \ ~(mempoolEnv, mempoolState) ->
        bgroup
          "reapplyTx"
          [ env (pure validatedTx1) $
              bench "Tx1" . whnf (reapplyTx' mempoolEnv mempoolState)
          , env (pure validatedTx2) $
              bench "Tx2" . whnf (reapplyTx' mempoolEnv mempoolState)
          , env (pure validatedTx3) $
              bench "Tx3" . whnf (reapplyTx' mempoolEnv mempoolState)
          , env
              (pure [validatedTx1, validatedTx2, validatedTx3])
              $ bench "Tx1+Tx2+Tx3" . whnf (F.foldl' (reapplyTx' mempoolEnv) mempoolState)
          ]
    , env (pure (mkMempoolEnv es slotNo, toMempoolState es)) $ \ ~(mempoolEnv, mempoolState) ->
        bgroup
          "applyTx"
          [ env (pure (extractTx validatedTx1)) $
              bench "Tx1" . whnf (applyTx' mempoolEnv mempoolState)
          , env (pure (extractTx validatedTx2)) $
              bench "Tx2" . whnf (applyTx' mempoolEnv mempoolState)
          , env (pure (extractTx validatedTx3)) $
              bench "Tx3" . whnf (applyTx' mempoolEnv mempoolState)
          ]
    , env (pure (getUTxO es)) $ \utxo ->
        bgroup
          "UTxO"
          [ bench "balance" $ nf balance utxo
          , bench "coinBalance" $ nf coinBalance utxo
          , -- We need to filter out all multi-assets to prevent `areAllAdaOnly`
            -- from short circuiting and producing results that are way better
            -- than the worst case
            env (pure $ Map.filter (\txOut -> isAdaOnly (txOut ^. valueTxOutL)) $ unUTxO utxo) $
              bench "areAllAdaOnly" . nf areAllAdaOnly
          ]
    , env (pure es) $ \newEpochState ->
        let utxo = getUTxO es
            (_, minTxOut) = Map.findMin $ unUTxO utxo
            (_, maxTxOut) = Map.findMax $ unUTxO utxo
            setAddr =
              Set.fromList [minTxOut ^. addrTxOutL, maxTxOut ^. addrTxOutL]
         in bgroup
              "MinMaxTxId"
              [ env (pure setAddr) $
                  bench "getFilteredNewUTxO" . nf (getFilteredUTxO newEpochState)
              , env (pure setAddr) $
                  bench "getFilteredOldUTxO" . nf (getFilteredOldUTxO newEpochState)
              ]
    , env (pure es) $ \newEpochState ->
        let umap = dsUnified . certDState . lsCertState . esLState $ nesEs newEpochState
            elems = umElems umap
            creds = runStateGen_ stdGen (uniformSubset (Just 10) (Map.keysSet elems))
         in bgroup
              ( "GetFilteredDelegationsAndRewardAccounts ("
                  ++ show (Set.size creds)
                  ++ "/"
                  ++ show (Map.size elems)
                  ++ ")"
              )
              [ env (pure creds) $
                  bench "getFilteredDelegationsAndRewardAccounts"
                    . nf (getFilteredDelegationsAndRewardAccounts umap)
              , env (pure creds) $
                  bench "queryStakePoolDelegsAndRewards"
                    . nf (queryStakePoolDelegsAndRewards newEpochState)
              ]
    , bgroup
        "DeleteTxOuts"
        [ extractKeysBench (unUTxO (getUTxO es)) largeKeysNum largeKeys
        , extractKeysBench (unUTxO (getUTxO es)) 9 (Set.take 9 largeKeys)
        , extractKeysBench (unUTxO (getUTxO es)) 5 (Set.take 5 largeKeys)
        , extractKeysBench (unUTxO (getUTxO es)) 2 (Set.take 2 largeKeys)
        ]
    ]

extractKeysBench ::
  (NFData k, NFData a, Ord k) =>
  Map k a ->
  Int ->
  Set k ->
  Benchmark
extractKeysBench utxo n ks =
  bgroup
    (show n)
    [ env (pure utxo) $ \m ->
        bench "extractKeys" $ whnf (seqTuple . extractKeys m) ks
    , env (pure utxo) $ \m ->
        bench "extractKeysSmallSet" $ whnf (seqTuple . extractKeysSmallSet m) ks
    , env (pure utxo) $ \m ->
        bench "extractKeysNaive" $ whnf (seqTuple . extractKeysNaive m) ks
    ]

-- | Pick out randomly n unique keys from the Map
selectRandomMapKeys :: (Monad m, Ord k) => Int -> StdGen -> Map k v -> m (Set k)
selectRandomMapKeys n gen m = runStateGenT_ gen $ \g ->
  let go !ixs !ks
        | Set.size ixs < n = do
            ix <- uniformRM (0, Map.size m - 1) g
            if ix `Set.member` ixs
              then go ixs ks
              else go (Set.insert ix ixs) (Set.insert (fst $ Map.elemAt ix m) ks)
        | otherwise = pure ks
   in go Set.empty Set.empty

extractKeysNaive :: Ord k => Map k a -> Set.Set k -> (Map k a, Map k a)
extractKeysNaive sm s = (Map.withoutKeys sm s, Map.restrictKeys sm s)

decodeTx :: ByteString -> Tx CurrentEra
decodeTx hex = either error id $ do
  bsl <- BSL16.decode hex
  first show $ decodeFullAnnotator (eraProtVerHigh @CurrentEra) "Tx" decCBOR bsl

-- | Most basic ada-only transaction:
--
-- * One input with Shelley address without staking
-- * One destination and change back to the address from original input.
validatedTx1 :: Validated (Tx CurrentEra)
validatedTx1 =
  unsafeMakeValidated $
    decodeTx
      "84a500818258201a42ba3e89f7ac4e526a61836b29847cb143c504e1429caa75c9ee06\
      \312f7ef7000d80018282581d61780648b89ea2f11fa9bbdd67552db5dd020eda1c9a54\
      \142dd9f1b1361a001440b3825839011e9c9362752648dda1dfa9382c70aad2bedbb683\
      \c213c6cdf4c4ef14800525c22102c1a6e05589b0209ccc06f57cb332960920d45080ba\
      \c31a000f4240021a0002a2ad0e81581c780648b89ea2f11fa9bbdd67552db5dd020eda\
      \1c9a54142dd9f1b136a10081825820cf2477066091b565f87f0445817c4df726900b29\
      \af3f05d229309afdbf94296d5840028182010e2204fae981f24df1be5e890122d2d854\
      \dde86c57635fa88de1df834b4321e2aaade0353947b682f265cc546abc4db86853f435\
      \842a808e500e4201f5f6"

-- | Slightly less basic ada-only transaction:
--
-- * One input with Shelley address /with/ staking address
-- * One destination and change back to the address from original input.
validatedTx2 :: Validated (Tx CurrentEra)
validatedTx2 =
  unsafeMakeValidated $
    decodeTx
      "84a500818258204c94b067b71c219d178e81d5aa87d2bc8c567855056f646fd244979d\
      \7b989f83000d80018282583901780648b89ea2f11fa9bbdd67552db5dd020eda1c9a54\
      \142dd9f1b13693d02e102e7e1917f14280d263c6878f9a7f238c2a24e702e9ea6ccf1a\
      \0011cae3825839011e9c9362752648dda1dfa9382c70aad2bedbb683c213c6cdf4c4ef\
      \14800525c22102c1a6e05589b0209ccc06f57cb332960920d45080bac31a000f424002\
      \1a0002a77d0e81581c780648b89ea2f11fa9bbdd67552db5dd020eda1c9a54142dd9f1\
      \b136a10081825820cf2477066091b565f87f0445817c4df726900b29af3f05d229309a\
      \fdbf94296d5840634a53d6826e3c63a0fd451e142c565beea8e08ff47bb302b7365a97\
      \5a723618c48ffafe2048ae9181a388bdea5f61ca7f085d1073e91d64722a2e76509c7b\
      \0ef5f6"

-- | Transaction with non-ADA value
--
-- * One input with Shelley address /with/ staking address and some tokens
-- * One destination and change back to the address from original input.
validatedTx3 :: Validated (Tx CurrentEra)
validatedTx3 =
  unsafeMakeValidated $
    decodeTx
      "84a500818258200e5d59af740d8682656ade4af6c069f926b9ec51689ee962260b4127\
      \8d21538e000d80018282583901780648b89ea2f11fa9bbdd67552db5dd020eda1c9a54\
      \142dd9f1b13693d02e102e7e1917f14280d263c6878f9a7f238c2a24e702e9ea6ccf1a\
      \002a26f282583901780648b89ea2f11fa9bbdd67552db5dd020eda1c9a54142dd9f1b1\
      \3693d02e102e7e1917f14280d263c6878f9a7f238c2a24e702e9ea6ccf821a00150bd0\
      \a1581ca89568bb399d0cdc38367e47831c95186f5c79e58174e08a18232396a14a4554\
      \424643546f6b656e1a006cc9f2021a0002afe90e81581c780648b89ea2f11fa9bbdd67\
      \552db5dd020eda1c9a54142dd9f1b136a10081825820cf2477066091b565f87f044581\
      \7c4df726900b29af3f05d229309afdbf94296d584088444a5845b198a2d255175770be\
      \7120c2d3482751b14f06dd41d7ff023eeae6e63933b097c023c1ed19df6a061173c45aa\
      \54cceb568ff1886e2716e84e6260df5f6"

mkGlobals :: ShelleyGenesis StandardCrypto -> PParams CurrentEra -> Globals
mkGlobals genesis pp =
  mkShelleyGlobals genesis epochInfoE majorPParamsVer
  where
    majorPParamsVer = pvMajor $ pp ^. ppProtocolVersionL
    epochInfoE =
      fixedEpochInfo
        (sgEpochLength genesis)
        (mkSlotLength . fromNominalDiffTimeMicro . sgSlotLength $ genesis)

getFilteredOldUTxO ::
  EraTxOut era =>
  NewEpochState era ->
  Set (Addr (EraCrypto era)) ->
  UTxO era
getFilteredOldUTxO ss addrs =
  UTxO $
    Map.filter (\txOut -> (txOut ^. compactAddrTxOutL) `Set.member` addrSBSs) fullUTxO
  where
    UTxO fullUTxO = getUTxO ss
    addrSBSs = Set.map compactAddr addrs

seqTuple :: (a, b) -> (a, b)
seqTuple (x, y) = x `seq` y `seq` (x, y)
{-# INLINE seqTuple #-}
