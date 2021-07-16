{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.Cardano.Chain.UTxO.Example
  ( exampleTxAux,
    exampleTxAux1,
    exampleTxId,
    exampleTxInList,
    exampleTxInUtxo,
    exampleTxPayload,
    exampleTxPayload1,
    exampleTxProof,
    exampleTxOut,
    exampleTxOut1,
    exampleTxOutList,
    exampleTxSig,
    exampleTxSigData,
    exampleTxWitness,
    exampleRedeemSignature,
    exampleHashTx,
  )
where

import Cardano.Chain.Common
  ( NetworkMagic (..),
    makeVerKeyAddress,
    mkAttributes,
    mkKnownLovelace,
    mkMerkleTree,
    mtRoot,
  )
import Cardano.Chain.UTxO
  ( Tx (..),
    TxAux,
    TxId,
    TxIn (..),
    TxInWitness (..),
    TxOut (..),
    TxPayload,
    TxProof (..),
    TxSig,
    TxSigData (..),
    TxWitness,
    mkTxAux,
    mkTxPayload,
  )
import Cardano.Crypto
  ( Hash,
    ProtocolMagicId (..),
    RedeemSignature,
    SignTag (..),
    VerificationKey (..),
    redeemDeterministicKeyGen,
    redeemSign,
    serializeCborHash,
    sign,
  )
import qualified Cardano.Crypto.Wallet as CC
import Cardano.Prelude
import Data.Coerce (coerce)
import Data.List.NonEmpty (fromList)
import Data.Maybe (fromJust)
import qualified Data.Vector as V
import Test.Cardano.Crypto.CBOR (getBytes)
import Test.Cardano.Crypto.Example (exampleSigningKey, exampleVerificationKey)

exampleTxAux :: TxAux
exampleTxAux = mkTxAux tx exampleTxWitness
  where
    tx = UnsafeTx exampleTxInList exampleTxOutList (mkAttributes ())

exampleTxAux1 :: TxAux
exampleTxAux1 = mkTxAux tx exampleTxWitness
  where
    tx = UnsafeTx exampleTxInList1 exampleTxOutList1 (mkAttributes ())

exampleTxId :: TxId
exampleTxId = exampleHashTx

exampleTxInList :: (NonEmpty TxIn)
exampleTxInList = fromList [exampleTxInUtxo]

exampleTxInList1 :: (NonEmpty TxIn)
exampleTxInList1 = fromList [exampleTxInUtxo, exampleTxInUtxo1]

exampleTxInUtxo :: TxIn
exampleTxInUtxo = TxInUtxo exampleHashTx 47 -- TODO: loop here

exampleTxInUtxo1 :: TxIn
exampleTxInUtxo1 = TxInUtxo exampleHashTx 74

exampleTxOut :: TxOut
exampleTxOut =
  TxOut
    (makeVerKeyAddress NetworkMainOrStage vkey)
    (mkKnownLovelace @47)
  where
    Right vkey = VerificationKey <$> CC.xpub (getBytes 0 64)

exampleTxOut1 :: TxOut
exampleTxOut1 = TxOut (makeVerKeyAddress (NetworkTestnet 74) vkey) (mkKnownLovelace @47)
  where
    Right vkey = VerificationKey <$> CC.xpub (getBytes 0 64)

exampleTxOutList :: (NonEmpty TxOut)
exampleTxOutList = fromList [exampleTxOut]

exampleTxOutList1 :: (NonEmpty TxOut)
exampleTxOutList1 = fromList [exampleTxOut, exampleTxOut1]

exampleTxPayload :: TxPayload
exampleTxPayload = mkTxPayload [exampleTxAux]

exampleTxPayload1 :: TxPayload
exampleTxPayload1 = mkTxPayload [exampleTxAux, exampleTxAux1]

exampleTxProof :: TxProof
exampleTxProof = TxProof 32 mroot hashWit
  where
    mroot =
      mtRoot $
        mkMerkleTree
          [(UnsafeTx exampleTxInList exampleTxOutList (mkAttributes ()))]
    hashWit = serializeCborHash $ [(V.fromList [(VKWitness exampleVerificationKey exampleTxSig)])]

exampleTxSig :: TxSig
exampleTxSig =
  sign (ProtocolMagicId 0) SignForTestingOnly exampleSigningKey exampleTxSigData

exampleTxSigData :: TxSigData
exampleTxSigData = TxSigData exampleHashTx

exampleTxWitness :: TxWitness
exampleTxWitness = V.fromList [(VKWitness exampleVerificationKey exampleTxSig)]

exampleRedeemSignature :: RedeemSignature TxSigData
exampleRedeemSignature =
  redeemSign
    (ProtocolMagicId 0)
    SignForTestingOnly
    rsk
    exampleTxSigData
  where
    rsk = fromJust (snd <$> redeemDeterministicKeyGen (getBytes 0 32))

exampleHashTx :: Hash Tx
exampleHashTx = coerce (serializeCborHash "golden" :: Hash Text)
