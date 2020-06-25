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
    bootstrapHashTest,
  )
where

import qualified Cardano.Chain.Common as Byron
import qualified Cardano.Crypto.DSIGN as DSIGN
import qualified Cardano.Crypto.Hash as Hash
import qualified Cardano.Crypto.Signing as Byron
import qualified Cardano.Crypto.Wallet as Byron
import Cardano.Prelude
  ( ByteString,
    Proxy (..),
  )
import Data.Coerce
  ( coerce,
  )
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Data.String (fromString)
import Hedgehog (Gen, (===))
import qualified Hedgehog as H
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
import Shelley.Spec.Ledger.Keys
  ( GenDelegs (..),
    KeyHash (..),
    coerceKeyRole,
  )
import Shelley.Spec.Ledger.LedgerState
  ( PPUPState (..),
    UTxOState (..),
    genesisId,
  )
import Shelley.Spec.Ledger.PParams
  ( PParams' (..),
    ProposedPPUpdates (..),
    emptyPParams,
  )
import Shelley.Spec.Ledger.STS.Utxo
  ( UtxoEnv (..),
  )
import Shelley.Spec.Ledger.STS.Utxow (UTXOW)
import Shelley.Spec.Ledger.Slot
  ( SlotNo (..),
  )
import Shelley.Spec.Ledger.Tx
  ( Tx (..),
    WitnessSetHKD (..),
  )
import Shelley.Spec.Ledger.TxData
  ( StakePools (..),
    TxBody (..),
    TxId (..),
    TxIn (..),
    TxOut (..),
    Wdrl (..),
  )
import Shelley.Spec.Ledger.UTxO
  ( UTxO (..),
    hashTxBody,
  )
import qualified Test.Cardano.Chain.Common.Gen as Byron
import qualified Test.Cardano.Crypto.Gen as Byron
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes (ConcreteCrypto)
import Test.Shelley.Spec.Ledger.Utils (testSTS)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit
  ( Assertion,
  )
import qualified Test.Tasty.Hedgehog as T

bootstrapHashTest :: TestTree
bootstrapHashTest = T.testProperty "rebuild the 'addr root' using a bootstrap witness" $
  H.property $
    do
      (byronVKey, byronAddr) <- H.forAll genByronVKeyAddr
      let addr = BootstrapAddress byronAddr
          (shelleyVKey, chainCode) = unpackByronVKey @C byronVKey
          witness =
            BootstrapWitness
              { bwKey = shelleyVKey,
                bwChainCode = chainCode,
                bwSig = dummySig,
                bwPadding = fromJust $ byronAddressPadding byronAddr
              }
      (coerceKeyRole $ bootstrapKeyHash addr) === bootstrapWitKeyHash witness

dummySig :: forall v a. DSIGN.DSIGNAlgorithm v => DSIGN.SignedDSIGN v a
dummySig =
  DSIGN.SignedDSIGN
    (fromJust $ DSIGN.rawDeserialiseSigDSIGN (fromString $ replicate size '0'))
  where
    size = fromIntegral $ DSIGN.sizeSigDSIGN (Proxy :: Proxy v)

genBootstrapAddress :: Gen (BootstrapAddress crypto)
genBootstrapAddress = BootstrapAddress . snd <$> genByronVKeyAddr

genByronVKeyAddr :: Gen (Byron.VerificationKey, Byron.Address)
genByronVKeyAddr = do
  vkey <- Byron.genVerificationKey
  addr <- genByronAddrFromVKey vkey
  pure (vkey, addr)

genByronAddrFromVKey :: Byron.VerificationKey -> Gen Byron.Address
genByronAddrFromVKey vkey =
  Byron.makeAddress (Byron.VerKeyASD vkey) <$> Byron.genAddrAttributes

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

utxoState1 :: UTxOState C
utxoState1 =
  UTxOState
    { _utxo = UTxO $ Map.fromList [bobResult, aliceResult],
      _deposited = 0,
      _fees = 10,
      _ppups = PPUPState (ProposedPPUpdates mempty) (ProposedPPUpdates mempty)
    }
  where
    txid = TxId $ hashTxBody txBody
    bobResult = (TxIn txid 0, TxOut bobAddr coinsToBob)
    aliceResult = (TxIn txid 1, TxOut aliceAddr (Coin 998990))

utxoEnv :: UtxoEnv C
utxoEnv =
  UtxoEnv
    0
    emptyPParams {_maxTxSize = 1000}
    (StakePools mempty)
    (GenDelegs mempty)

aliceInitCoin :: Coin
aliceInitCoin = 1000000

aliceSigningKey :: Byron.SigningKey
aliceSigningKey = Byron.SigningKey $ Byron.generate seed (mempty :: ByteString)
  where
    seed :: ByteString -- 32 bytes
    seed = "12345678901234567890123456789012"

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
  fromJust $
    makeBootstrapWitness
      (hashTxBody txBody)
      aliceSigningKey
      aliceByronAddr

bobAddr :: Addr C
bobAddr = Addr Testnet (KeyHashObj $ coerce someHash) StakeRefNull
  where
    someHash = "someHash" :: ByteString

coinsToBob :: Coin
coinsToBob = 1000

txBody :: TxBody C
txBody =
  TxBody
    (Set.fromList [TxIn genesisId 0]) -- inputs
    (StrictSeq.fromList [TxOut bobAddr coinsToBob, TxOut aliceAddr change]) -- outputs
    (StrictSeq.fromList mempty) -- dcert
    (Wdrl Map.empty)
    fee
    (SlotNo 10)
    SNothing -- up
    SNothing -- md
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

-- Test that we can use a BootstrapWitness to spend from a BootstrapAddress

-- where
-- lastByronHeaderHash :: HashHeader C
-- lastByronHeaderHash _ = HashHeader $ coerce (hash 0 :: Hash _ Int)

{-
--TODO: combine with STS test stuff
testSTS ::
  forall s.
  (BaseM s ~ ShelleyBase, STS s, Eq (State s), Show (State s)) =>
  Environment s ->
  State s ->
  Signal s ->
  Either [[PredicateFailure s]] (State s) ->
  Assertion
testSTS env initSt signal (Right expectedSt) = do
  checkTrace @s runShelleyBase env $ pure initSt .- signal .-> expectedSt
testSTS env initSt block predicateFailure@(Left _) = do
  let st = runShelleyBase $ applySTS @s (TRC (env, initSt, block))
  st @?= predicateFailure
-}
data C

instance Crypto C where
  type KES C = KES (ConcreteCrypto Hash.ShortHash)
  type VRF C = VRF (ConcreteCrypto Hash.ShortHash)
  type DSIGN C = DSIGN.Ed25519DSIGN
  type HASH C = HASH (ConcreteCrypto Hash.ShortHash)
  type ADDRHASH C = Hash.Blake2b_224
