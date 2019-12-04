{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Test.Cardano.Chain.Elaboration.UTxO
  ( elaborateUTxOEnv
  , elaborateUTxO
  , elaborateTx
  , elaborateTxWitsBS
  , elaborateTxOut
  )
where

import Cardano.Prelude

import qualified Data.ByteString.Lazy as LBS
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import Formatting hiding (bytes)

import qualified Cardano.Binary as CBOR
import qualified Cardano.Chain.Common as Concrete
import qualified Cardano.Chain.UTxO as Concrete
import qualified Cardano.Chain.UTxO.UTxO as Concrete.UTxO
import qualified Cardano.Chain.UTxO.Validation as Concrete.UTxO
import qualified Cardano.Chain.Update as Concrete
import Cardano.Crypto

import qualified Cardano.Ledger.Spec.STS.UTXO as Abstract
import qualified Ledger.Core as Abstract
import qualified Ledger.UTxO as Abstract

import Test.Cardano.Chain.Elaboration.Keys
import Test.Cardano.Chain.Genesis.Dummy
import qualified Test.Cardano.Crypto.Dummy as Dummy


elaborateUTxOEnv :: Abstract.UTxOEnv -> Concrete.UTxO.Environment
elaborateUTxOEnv _abstractEnv = Concrete.UTxO.Environment
  { Concrete.UTxO.protocolMagic      = Dummy.aProtocolMagic
  , Concrete.UTxO.protocolParameters = dummyProtocolParameters
    { Concrete.ppTxFeePolicy =
      Concrete.TxFeePolicyTxSizeLinear $ Concrete.TxSizeLinear
        (Concrete.mkKnownLovelace @0)
        (Concrete.mkKnownLovelace @0)
    }
  , Concrete.UTxO.utxoConfiguration = Concrete.defaultUTxOConfiguration
  }

elaborateUTxO
  :: (Abstract.TxId -> Concrete.TxId)
  -> Abstract.UTxO
  -> Concrete.UTxO
elaborateUTxO elaborateTxId =
  Concrete.UTxO.fromList
    . fmap (elaborateUTxOEntry elaborateTxId)
    . M.toList
    . Abstract.unUTxO

elaborateUTxOEntry
  :: (Abstract.TxId -> Concrete.TxId)
  -> (Abstract.TxIn, Abstract.TxOut)
  -> (Concrete.TxIn, Concrete.TxOut)
elaborateUTxOEntry elaborateTxId (abstractTxIn, abstractTxOut) =
  (concreteTxIn, concreteTxOut)
 where
  concreteTxOut = elaborateTxOut abstractTxOut
  concreteTxIn  = elaborateTxIn elaborateTxId abstractTxIn

elaborateTxWitsBS
  :: (Abstract.TxId -> Concrete.TxId)
  -> Abstract.TxWits
  -> Concrete.ATxAux ByteString
elaborateTxWitsBS elaborateTxId =
  annotateTxAux . elaborateTxWits elaborateTxId
 where
  annotateTxAux :: Concrete.TxAux -> Concrete.ATxAux ByteString
  annotateTxAux txAux =
    map (LBS.toStrict . CBOR.slice bytes)
      . fromRight (panic "elaborateTxWitsBS: Error decoding TxAux")
      $ CBOR.decodeFull bytes
    where bytes = CBOR.serialize txAux

elaborateTxWits
  :: (Abstract.TxId -> Concrete.TxId) -> Abstract.TxWits -> Concrete.TxAux
elaborateTxWits elaborateTxId (Abstract.TxWits tx witnesses) =
  Concrete.mkTxAux concreteTx (elaborateWitnesses concreteTx witnesses)
  where concreteTx = elaborateTx elaborateTxId tx

elaborateTx :: (Abstract.TxId -> Concrete.TxId) -> Abstract.Tx -> Concrete.Tx
elaborateTx elaborateTxId (Abstract.Tx inputs outputs) =
  Concrete.UnsafeTx
    { Concrete.txInputs     = elaborateTxIns elaborateTxId inputs
    , Concrete.txOutputs    = elaborateTxOuts outputs
    , Concrete.txAttributes = Concrete.mkAttributes ()
    }

elaborateWitnesses :: Concrete.Tx -> [Abstract.Wit] -> Concrete.TxWitness
elaborateWitnesses concreteTx = V.fromList . fmap (elaborateWitness concreteTx)

elaborateWitness :: Concrete.Tx -> Abstract.Wit -> Concrete.TxInWitness
elaborateWitness concreteTx (Abstract.Wit key _) = Concrete.VKWitness
  concreteVK
  signature
 where
  (concreteVK, concreteSK) = elaborateKeyPair $ vKeyPair key
  signature = sign Dummy.protocolMagicId SignTx concreteSK sigData
  sigData   = Concrete.TxSigData $ hash concreteTx

elaborateTxIns
  :: (Abstract.TxId -> Concrete.TxId)
  -> [Abstract.TxIn]
  -> NonEmpty Concrete.TxIn
elaborateTxIns elaborateTxId =
  fromMaybe (panic "elaborateTxIns: Empty list of TxIns") . NE.nonEmpty . fmap
    (elaborateTxIn elaborateTxId)

elaborateTxIn
  :: (Abstract.TxId -> Concrete.TxId) -> Abstract.TxIn -> Concrete.TxIn
elaborateTxIn elaborateTxId (Abstract.TxIn txId index) =
  Concrete.TxInUtxo (elaborateTxId txId) (fromIntegral index)

elaborateTxOuts :: [Abstract.TxOut] -> NonEmpty Concrete.TxOut
elaborateTxOuts =
  fromMaybe
      (panic
        "elaborateTxOuts: Tried to elaborate an empty list of Abstract.TxOuts"
      )
    . NE.nonEmpty
    . fmap elaborateTxOut

elaborateTxOut :: Abstract.TxOut -> Concrete.TxOut
elaborateTxOut abstractTxOut = Concrete.TxOut
  { Concrete.txOutAddress = Concrete.makeVerKeyAddress
    (Concrete.makeNetworkMagic Dummy.protocolMagic)
    (elaborateVKey abstractVK)
  , Concrete.txOutValue   = lovelaceValue
  }
 where
  Abstract.TxOut (Abstract.Addr abstractVK) (Abstract.Lovelace value) =
    abstractTxOut

  lovelaceValue = case Concrete.mkLovelace (fromIntegral value) of
    Left  err -> panic $ sformat build err
    Right l   -> l
