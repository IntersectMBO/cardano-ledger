{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Shelley.Spec.Ledger.Address.Bootstrap
  ( genBootstrapAddress,
    testBootstrapSpending,
    testBootstrapNotSpending,
    bootstrapHashTest,
    genSignature,
  )
where

import Cardano.Binary (serialize')
import qualified Cardano.Chain.Common as Byron
import qualified Cardano.Crypto.DSIGN as DSIGN
import qualified Cardano.Crypto.Hash as Hash
import qualified Cardano.Crypto.Signing as Byron
import qualified Cardano.Crypto.Wallet as Byron
import Cardano.Prelude
  ( ByteString,
  )
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Shelley.Spec.Ledger.Address
  ( Addr (..),
    BootstrapAddress (..),
    bootstrapKeyHash,
  )
import Shelley.Spec.Ledger.Address.Bootstrap
import Shelley.Spec.Ledger.BaseTypes
  ( Network (..),
    StrictMaybe (..),
  )
import Shelley.Spec.Ledger.Coin (Coin (..))
import Shelley.Spec.Ledger.Credential
  ( Credential (..),
    StakeReference (..),
  )
import Shelley.Spec.Ledger.Crypto (Crypto (..))
import Shelley.Spec.Ledger.Hashing (hashAnnotated)
import Shelley.Spec.Ledger.Keys
  ( GenDelegs (..),
    KeyRole (..),
    VKey (..),
    coerceKeyRole,
    hashKey,
  )
import Shelley.Spec.Ledger.LedgerState
  ( PPUPState (..),
    UTxOState (..),
  )
import Shelley.Spec.Ledger.PParams
  ( PParams' (..),
    ProposedPPUpdates (..),
    emptyPParams,
  )
import Shelley.Spec.Ledger.STS.Utxo
  ( UtxoEnv (..),
  )
import Shelley.Spec.Ledger.STS.Utxow
  ( PredicateFailure (..),
    UTXOW,
  )
import Shelley.Spec.Ledger.Slot
  ( SlotNo (..),
  )
import Shelley.Spec.Ledger.Tx
  ( Tx (..),
    WitnessSetHKD (..),
  )
import Shelley.Spec.Ledger.TxData
  ( TxBody (..),
    TxId (..),
    TxIn (..),
    TxOut (..),
    Wdrl (..),
  )
import Shelley.Spec.Ledger.UTxO
  ( UTxO (..),
  )
import qualified Test.Cardano.Chain.Common.Gen as Byron
import qualified Test.Cardano.Crypto.Gen as Byron
import Test.Cardano.Prelude (genBytes)
import Test.QuickCheck (Gen)
import Test.QuickCheck.Hedgehog (hedgehog)
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes (ConcreteCrypto)
import Test.Shelley.Spec.Ledger.Generator.Core (genesisId)
import Test.Shelley.Spec.Ledger.Utils (testSTS)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit
  ( Assertion,
  )
import Test.Tasty.QuickCheck (testProperty, (===))

bootstrapHashTest :: TestTree
bootstrapHashTest = testProperty "rebuild the 'addr root' using a bootstrap witness" $
  do
    (byronVKey, byronAddr) <- genByronVKeyAddr
    sig <- genSignature
    let addr = BootstrapAddress byronAddr
        (shelleyVKey, chainCode) = unpackByronVKey @C byronVKey
        witness =
          BootstrapWitness
            { bwKey = shelleyVKey,
              bwChainCode = chainCode,
              bwSig = sig,
              bwAttributes = serialize' $ Byron.addrAttributes byronAddr
            }
    pure $ (coerceKeyRole $ bootstrapKeyHash addr) === bootstrapWitKeyHash witness

genSignature :: forall a b. DSIGN.DSIGNAlgorithm a => Gen (DSIGN.SignedDSIGN a b)
genSignature =
  DSIGN.SignedDSIGN
    . fromJust
    . DSIGN.rawDeserialiseSigDSIGN
    <$> hedgehog (genBytes . fromIntegral $ DSIGN.sizeSigDSIGN ([] @a))

genBootstrapAddress :: Gen (BootstrapAddress crypto)
genBootstrapAddress = BootstrapAddress . snd <$> genByronVKeyAddr

genByronVKeyAddr :: Gen (Byron.VerificationKey, Byron.Address)
genByronVKeyAddr = do
  vkey <- hedgehog Byron.genVerificationKey
  addr <- genByronAddrFromVKey vkey
  pure (vkey, addr)

genByronAddrFromVKey :: Byron.VerificationKey -> Gen Byron.Address
genByronAddrFromVKey vkey =
  Byron.makeAddress (Byron.VerKeyASD vkey) <$> hedgehog Byron.genAddrAttributes

utxo0 :: UTxO C
utxo0 =
  UTxO $
    Map.singleton
      (TxIn genesisId 0)
      (TxOut aliceAddr aliceInitCoin)

utxoState0 :: UTxOState C
utxoState0 =
  UTxOState
    { _utxo = utxo0,
      _deposited = 0,
      _fees = 0,
      _ppups = PPUPState (ProposedPPUpdates mempty) (ProposedPPUpdates mempty)
    }

tx :: Tx C
tx = Tx txBody mempty {bootWits = Set.fromList [aliceWitness]} SNothing

txBad :: Tx C
txBad = Tx txBody mempty {bootWits = Set.fromList [aliceBadWitness]} SNothing

utxoState1 :: UTxOState C
utxoState1 =
  UTxOState
    { _utxo = UTxO $ Map.fromList [bobResult, aliceResult],
      _deposited = 0,
      _fees = 10,
      _ppups = PPUPState (ProposedPPUpdates mempty) (ProposedPPUpdates mempty)
    }
  where
    txid = TxId $ hashAnnotated txBody
    bobResult = (TxIn txid 0, TxOut bobAddr coinsToBob)
    aliceResult = (TxIn txid 1, TxOut aliceAddr (Coin 998990))

utxoEnv :: UtxoEnv C
utxoEnv =
  UtxoEnv
    0
    emptyPParams {_maxTxSize = 1000}
    mempty
    (GenDelegs mempty)

aliceInitCoin :: Coin
aliceInitCoin = 1000000

aliceSigningKey :: Byron.SigningKey
aliceSigningKey = Byron.SigningKey $ Byron.generate seed (mempty :: ByteString)
  where
    seed :: ByteString -- 32 bytes
    seed = "12345678901234567890123456789012"

aliceVKey :: VKey 'Witness C
aliceVKey = fst . unpackByronVKey . Byron.toVerification $ aliceSigningKey

aliceByronAddr :: Byron.Address
aliceByronAddr = Byron.makeAddress asd attrs
  where
    asd = Byron.VerKeyASD $ byronVerificationKey
    attrs =
      Byron.AddrAttributes
        (Just (Byron.HDAddressPayload "a compressed lenna.png"))
        (Byron.NetworkTestnet 0)
    byronVerificationKey = Byron.toVerification aliceSigningKey

aliceAddr :: Addr C
aliceAddr = AddrBootstrap (BootstrapAddress aliceByronAddr)

aliceWitness :: BootstrapWitness C
aliceWitness =
  makeBootstrapWitness
    (hashAnnotated txBody)
    aliceSigningKey
    (Byron.addrAttributes aliceByronAddr)

aliceBadWitness :: BootstrapWitness C
aliceBadWitness =
  makeBootstrapWitness
    (hashAnnotated txBody {_ttl = SlotNo 100000000})
    aliceSigningKey
    (Byron.addrAttributes aliceByronAddr)

bobAddr :: Addr C
bobAddr = Addr Testnet (KeyHashObj k) StakeRefNull
  where
    k = coerceKeyRole $ hashKey aliceVKey

coinsToBob :: Coin
coinsToBob = 1000

txBody :: TxBody C
txBody =
  TxBody
    { _inputs = Set.fromList [TxIn genesisId 0],
      _outputs = StrictSeq.fromList [TxOut bobAddr coinsToBob, TxOut aliceAddr change],
      _certs = StrictSeq.fromList mempty,
      _wdrls = Wdrl Map.empty,
      _txfee = fee,
      _ttl = SlotNo 10,
      _txUpdate = SNothing,
      _mdHash = SNothing
    }
  where
    change = aliceInitCoin - coinsToBob - fee
    fee = 10

testBootstrapSpending :: Assertion
testBootstrapSpending =
  testSTS @(UTXOW C)
    utxoEnv
    utxoState0
    tx
    (Right utxoState1)

testBootstrapNotSpending :: Assertion
testBootstrapNotSpending =
  testSTS @(UTXOW C)
    utxoEnv
    utxoState0
    txBad
    (Left [[InvalidWitnessesUTXOW [aliceVKey]]])

data C

instance Crypto C where
  type KES C = KES (ConcreteCrypto Hash.ShortHash)
  type VRF C = VRF (ConcreteCrypto Hash.ShortHash)
  type DSIGN C = DSIGN.Ed25519DSIGN
  type HASH C = HASH (ConcreteCrypto Hash.ShortHash)
  type ADDRHASH C = Hash.Blake2b_224
