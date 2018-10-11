module SimpleExamplesSpec where

import           Control.Monad (foldM)
import           Crypto.Hash   (hash)
import qualified Data.Map      as Map
import qualified Data.Set      as Set
import           Test.Hspec

import           UTxO

alice :: KeyPair
alice = keyPair (Owner 1)

aliceVali :: Script R
aliceVali = Validator (\_ r -> r == "alice") "alice?"

aliceRede :: Script R
aliceRede = Redeemer (const "alice") "alice"

aliceAddr :: Addr
aliceAddr = Addr $ hash aliceVali

bob :: KeyPair
bob = keyPair (Owner 3)

bobVali :: Script R
bobVali = Validator (\_ r -> r == "bob") "bob?"

bobRede :: Script R
bobRede = Redeemer (const "bob") "bob"

bobAddr :: Addr
bobAddr = Addr $ hash bobVali

type R = String

genesis :: LedgerState R
genesis = genesisState
            [ TxOut aliceAddr (Coin 10)
            , TxOut bobAddr (Coin 1) ]

ledgerState :: Ledger R -> Either [ValidationError] (LedgerState R)
ledgerState = foldM asStateTransition genesis

spec :: Spec
spec = do
  it "Valid Ledger - Alice gives Bob 3 of her 10 coins" $ do
    let
      tx1 = Tx
              (Set.fromList [TxIn genesisId 0 aliceVali aliceRede])
              [ TxOut aliceAddr (Coin 7)
              , TxOut bobAddr (Coin 3) ]
      tx1id = txid tx1
      utxo = Map.fromList
        [ (OutRef genesisId 1, TxOut bobAddr (Coin 1))
        , (OutRef tx1id 0, TxOut aliceAddr (Coin 7))
        , (OutRef tx1id 1, TxOut bobAddr (Coin 3)) ]
    ledgerState [tx1] `shouldBe` Right (LedgerState $ UTxO utxo)

  it "Invalid Ledger - Alice tries to spend a nonexistent input" $ do
    let
      tx1 = Tx
              (Set.fromList [TxIn genesisId 42 aliceVali aliceRede])
              [ TxOut aliceAddr (Coin 0) ]
    ledgerState [tx1] `shouldBe` Left [BadInputs, WrongAddress]
    -- Note that BadInputs implies WrongAddress

  it "Invalid Ledger - Alice tries to spend too much" $ do
    let
      tx1 = Tx
              (Set.fromList [TxIn genesisId 0 aliceVali aliceRede]) [ TxOut bobAddr (Coin 11) ]
    ledgerState [tx1] `shouldBe` Left [IncreasedTotalBalance]

  it "Invalid Ledger - Alice uses a bad redeemer" $ do
    let
      tx1 = Tx
              (Set.fromList [TxIn genesisId 0 aliceVali bobRede])
              [ TxOut aliceAddr (Coin 7)
              , TxOut bobAddr (Coin 3) ]
    ledgerState [tx1] `shouldBe` Left [Unauthorized]

  it "Invalid Ledger - Bob tries to spend Alice's coins" $ do
    let
      tx1 = Tx
              (Set.fromList [TxIn genesisId 0 bobVali bobRede])
              [ TxOut aliceAddr (Coin 7)
              , TxOut bobAddr (Coin 3) ]
    ledgerState [tx1] `shouldBe` Left [WrongAddress]
