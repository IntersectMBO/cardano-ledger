{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Bench.Cardano.Ledger.TxOut (benchTxOut) where

import Cardano.Binary (decodeFull, serialize)
import Cardano.Crypto.Hash.Class
import Cardano.Ledger.Address
import Cardano.Ledger.Alonzo (AlonzoEra)
import Cardano.Ledger.Alonzo.Data
import Cardano.Ledger.Alonzo.TxBody
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin
import Cardano.Ledger.CompactAddress
import Cardano.Ledger.Compactible
import Cardano.Ledger.Credential
import Cardano.Ledger.Crypto
import Cardano.Ledger.Era (getTxOutAddr)
import Cardano.Ledger.Hashes
import Cardano.Ledger.Keys
import Cardano.Ledger.Mary.Value
import Cardano.Ledger.SafeHash
import Cardano.Ledger.Val
import Control.DeepSeq
import Criterion.Main
import Data.Map.Strict (singleton)
import Data.Maybe
import qualified Data.Text as T
import GHC.Records

type A = AlonzoEra StandardCrypto

benchTxOut :: Benchmark
benchTxOut =
  let ada :: Value StandardCrypto
      ada = inject (Coin 100)
      key :: Int -> Credential 'Payment StandardCrypto
      key = KeyHashObj . payAddr28
      stake :: StakeReference StandardCrypto
      stake = StakeRefBase (KeyHashObj stakeAddr28)
      addr :: Int -> Addr StandardCrypto
      addr n = Addr Mainnet (key n) stake
      value :: Value StandardCrypto
      value = Value 200 (singleton (PolicyID policyId28) (singleton assName 217))
      txOutAddr :: Int -> TxOut A
      txOutAddr n = TxOut (addr n) value (SJust dataHash32)
      txOutAddrAdaOnly :: Int -> TxOut A
      txOutAddrAdaOnly n = TxOut (addr n) ada SNothing
      txOutAddrAdaOnlyDataHash :: Int -> TxOut A
      txOutAddrAdaOnlyDataHash n = TxOut (addr n) ada (SJust dataHash32)
      count :: Int
      count = 10000
   in bgroup
        "TxOut"
        [ bgroup
            "construct"
            [ constructTxOutAlonzoBench count "ValueDataHash" addr value (SJust dataHash32),
              constructTxOutAlonzoBench count "AdaOnlyDataHash" addr ada (SJust dataHash32),
              constructTxOutAlonzoBench count "AdaOnly" addr ada SNothing
            ],
          bgroup
            "access"
            [ accessTxOutAlonzoBench count "ValueDataHash" txOutAddr,
              accessTxOutAlonzoBench count "AdaOnlyDataHash" txOutAddrAdaOnlyDataHash,
              accessTxOutAlonzoBench count "AdaOnly" txOutAddrAdaOnly
            ],
          bgroup
            "serialize"
            [ serializeTxOutAlonzoBench count "ValueDataHash" txOutAddr,
              serializeTxOutAlonzoBench count "AdaOnlyDataHash" txOutAddrAdaOnlyDataHash,
              serializeTxOutAlonzoBench count "AdaOnly" txOutAddrAdaOnly
            ]
        ]

constructTxOutAlonzoBench ::
  Int ->
  String ->
  (Int -> Addr StandardCrypto) ->
  Value StandardCrypto ->
  StrictMaybe (DataHash StandardCrypto) ->
  Benchmark
constructTxOutAlonzoBench count name mkAddr value !mdh =
  cvalue
    `seq` bgroup
      name
      [ env (pure (mkAddr <$> [1 .. count])) $
          bench "TxOut" . nf (map (\addr -> TxOut addr value mdh :: TxOut A)),
        env (pure (compactAddr . mkAddr <$> [1 .. count])) $
          bench "TxOutCompact" . nf (map (\caddr -> mkTxOutCompact caddr cvalue :: TxOut A))
      ]
  where
    cvalue = maybe (error "Uncompactible") id $ toCompact value
    mkTxOutCompact ::
      CompactAddr StandardCrypto -> CompactForm (Value StandardCrypto) -> TxOut A
    mkTxOutCompact =
      case mdh of
        SNothing -> TxOutCompact
        SJust dh -> \a v -> TxOutCompactDH a v dh

accessTxOutAlonzoBench :: Int -> String -> (Int -> TxOut A) -> Benchmark
accessTxOutAlonzoBench count name mkTxOuts =
  bgroup
    name
    [ env (pure (mkTxOuts <$> [1 .. count])) $ \txOuts ->
        bench "TxOut" $
          nf (map (\(TxOut addr vl dh) -> addr `deepseq` vl `deepseq` dh)) txOuts,
      env (pure (mkTxOuts <$> [1 .. count])) $ \txOuts ->
        bench "TxOutCompact" $
          nf
            ( map
                ( \case
                    TxOutCompact addr vl -> addr `deepseq` vl
                    TxOutCompactDH addr vl dh -> addr `deepseq` dh `deepseq` vl
                )
            )
            txOuts,
      env (pure (mkTxOuts <$> [1 .. count])) $ \txOuts ->
        bench "getTxOutAddr" $
          nf
            (map getTxOutAddr)
            txOuts,
      env (pure (mkTxOuts <$> [1 .. count])) $ \txOuts ->
        bench "getField value" $ nf (map (getField @"value")) txOuts
    ]

serializeTxOutAlonzoBench :: Int -> String -> (Int -> TxOut A) -> Benchmark
serializeTxOutAlonzoBench count name mkTxOuts =
  bgroup
    name
    [ env (pure (mkTxOuts <$> [1 .. count])) $ bench "ToCBOR" . nf (map serialize),
      env (pure (serialize . mkTxOuts <$> [1 .. count])) $
      bench "FromCBOR" . nf (map (either (error . show) (id @(TxOut A)) . decodeFull))
    ]

payAddr28 :: Int -> KeyHash 'Payment StandardCrypto
payAddr28 n =
  let i = n `mod` 10
      prefix = T.pack (take 6 (cycle (show i)))
   in KeyHash $
        fromJust $
          hashFromTextAsHex
            (prefix <> "0405060708090a0b0c0d0e0f12131415161718191a1b1c1d1e")

stakeAddr28 :: KeyHash 'Staking StandardCrypto
stakeAddr28 =
  KeyHash $
    fromJust $
      hashFromTextAsHex "2122232425262728292a2b2c2d2e2f32333435363738393a3b3c3d3e"

dataHash32 :: DataHash StandardCrypto
dataHash32 =
  unsafeMakeSafeHash $
    fromJust $
      hashFromTextAsHex "404144434445464748494a4b4c4d4e4f505152555455565758595a5b5c5d5e5f"

policyId28 :: ScriptHash StandardCrypto
policyId28 =
  ScriptHash $
    fromJust $
      hashFromTextAsHex "6166636465666768696a6b6c6d6e6f72777475767778797a7b7c7d7e"

assName :: AssetName
assName = AssetName "Booyah"
