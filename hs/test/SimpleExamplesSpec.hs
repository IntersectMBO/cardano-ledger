module SimpleExamplesSpec where

import           Control.Monad (foldM)
import           Crypto.Hash   (hash)
import qualified Data.Map      as Map
import qualified Data.Set      as Set
import           Test.Hspec

import           LedgerState
import           UTxO
import           Keys

alice :: KeyPair
alice = keyPair (Owner 1)

aliceAddr :: Addr
aliceAddr = Addr $ hash (vKey alice)

bob :: KeyPair
bob = keyPair (Owner 3)

bobAddr :: Addr
bobAddr = Addr $ hash (vKey bob)

genesis :: LedgerState
genesis = genesisState
            [ TxOut aliceAddr (Coin 10)
            , TxOut bobAddr (Coin 1) ]

ledgerState :: Ledger -> Either [ValidationError] LedgerState
ledgerState = foldM asStateTransition genesis

spec :: Spec
spec = do
  it "Valid Ledger - Alice gives Bob 3 of her 10 coins" $ do
    let
      tx1Body = Tx
              (Set.fromList [TxIn genesisId 0])
              [ TxOut aliceAddr (Coin 7)
              , TxOut bobAddr (Coin 3) ]
      aliceTx1Wit = makeWitness alice tx1Body
      tx1 = TxWits tx1Body (Set.fromList [aliceTx1Wit])
      tx1id = txid tx1Body
      utxo = Map.fromList
        [ (TxIn genesisId 1, TxOut bobAddr (Coin 1))
        , (TxIn tx1id 0, TxOut aliceAddr (Coin 7))
        , (TxIn tx1id 1, TxOut bobAddr (Coin 3)) ]
    ledgerState [tx1] `shouldBe` Right (LedgerState $ UTxO utxo)

  it "Invalid Ledger - Alice tries to spend a nonexistent input" $ do
    let
      tx1Body = Tx
              (Set.fromList [TxIn genesisId 42])
              [ TxOut aliceAddr (Coin 0) ]
      aliceTx1Wit = makeWitness alice tx1Body
      tx1 = TxWits tx1Body (Set.fromList [aliceTx1Wit])
    ledgerState [tx1] `shouldBe` Left [BadInputs, InsuffientWitnesses]
    -- Note that BadInputs implies InsuffientWitnesses

  it "Invalid Ledger - Alice tries to spend too much" $ do
    let
      tx1Body = Tx
              (Set.fromList [TxIn genesisId 0]) [ TxOut bobAddr (Coin 11) ]
      aliceTx1Wit = makeWitness alice tx1Body
      tx1 = TxWits tx1Body (Set.fromList [aliceTx1Wit])
    ledgerState [tx1] `shouldBe` Left [IncreasedTotalBalance]

  it "Invalid Ledger - Alice does not include a witness" $ do
    let
      tx1Body = Tx
              (Set.fromList [TxIn genesisId 0])
              [ TxOut aliceAddr (Coin 7)
              , TxOut bobAddr (Coin 3) ]
      tx1 = TxWits tx1Body Set.empty
    ledgerState [tx1] `shouldBe` Left [InsuffientWitnesses]

  it "Invalid Ledger - Alice tries to spend Bob's UTxO" $ do
    let
      tx1Body = Tx
        (Set.fromList [TxIn genesisId 1])
              [ TxOut aliceAddr (Coin 1)]
      aliceTx1Wit = makeWitness alice tx1Body
      tx1 = TxWits tx1Body (Set.fromList [aliceTx1Wit])
    ledgerState [tx1] `shouldBe` Left [InsuffientWitnesses]

  it "Invalid Ledger - Alice provides witness of wrong UTxO" $ do
    let
      tx1Body = Tx
        (Set.fromList [TxIn genesisId 1])
              [ TxOut aliceAddr (Coin 1)]
      tx2Body = Tx
        (Set.fromList [TxIn genesisId 0])
              [ TxOut aliceAddr (Coin 10)]
      aliceTx1Wit = makeWitness alice tx2Body
      tx1 = TxWits tx1Body (Set.fromList [aliceTx1Wit])
    ledgerState [tx1] `shouldBe` Left [InsuffientWitnesses]
