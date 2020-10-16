{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Shelley.Spec.Ledger.MultiSigExamples
  ( applyTxWithScript,
    aliceOnly,
    bobOnly,
    aliceAndBob,
    aliceOrBob,
    aliceAndBobOrCarl,
    aliceAndBobOrCarlAndDaria,
    aliceAndBobOrCarlOrDaria,
  )
where

import qualified Cardano.Crypto.Hash as Hash
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Crypto, Era)
import qualified Cardano.Ledger.Shelley as Shelley
import Control.State.Transition.Extended (BaseM, PredicateFailure, STS, TRC (..))
import Data.Coerce (coerce)
import Data.Foldable (fold)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map (empty, fromList)
import Data.Sequence.Strict (StrictSeq (..))
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set (fromList)
import Shelley.Spec.Ledger.API
  ( ScriptHash,
    UTXOW,
  )
import Shelley.Spec.Ledger.Address
  ( Addr,
    pattern Addr,
  )
import Shelley.Spec.Ledger.BaseTypes
  ( Network (..),
    ShelleyBase,
    StrictMaybe (..),
    maybeToStrictMaybe,
  )
import Shelley.Spec.Ledger.Coin
import Shelley.Spec.Ledger.Credential
  ( pattern KeyHashObj,
    pattern ScriptHashObj,
    pattern StakeRefBase,
  )
import Shelley.Spec.Ledger.Hashing (hashAnnotated)
import Shelley.Spec.Ledger.Keys
  ( GenDelegs (..),
    KeyHash (..),
    KeyPair,
    KeyRole (..),
    asWitness,
  )
import Shelley.Spec.Ledger.LedgerState
  ( LedgerState (..),
    UTxOState,
    genesisState,
  )
import Shelley.Spec.Ledger.MetaData (MetaData)
import Shelley.Spec.Ledger.PParams (PParams, emptyPParams, _maxTxSize)
import Shelley.Spec.Ledger.STS.Utxo (UtxoEnv (..))
import Shelley.Spec.Ledger.Scripts
  ( MultiSig,
    pattern RequireAllOf,
    pattern RequireAnyOf,
    pattern RequireMOf,
    pattern RequireSignature,
    pattern ScriptHash,
  )
import Shelley.Spec.Ledger.Slot (SlotNo (..))
import Shelley.Spec.Ledger.Tx
  ( Tx (..),
    TxId,
    WitnessSetHKD (..),
    hashScript,
  )
import Shelley.Spec.Ledger.TxBody
  ( TxBody (..),
    TxIn (..),
    TxOut (..),
    Wdrl (..),
  )
import Shelley.Spec.Ledger.UTxO (makeWitnessesVKey, txid)
import Test.Shelley.Spec.Ledger.ConcreteCryptoTypes
  ( Mock,
  )
import qualified Test.Shelley.Spec.Ledger.Examples.Cast as Cast
import Test.Shelley.Spec.Ledger.Generator.Core
  ( genesisCoins,
    genesisId,
  )
import Test.Shelley.Spec.Ledger.Utils

-- Multi-Signature tests

-- This compile-time test asserts that the script hash and key hash use the
-- same hash size and indeed hash function. We do this by checking we can
-- type-check the following code that converts between them by using the hash
-- casting function which changes what the hash is of, without changing the
-- hashing algorithm.
--
_assertScriptHashSizeMatchesAddrHashSize ::
  ScriptHash era ->
  KeyHash r (Crypto era)
_assertScriptHashSizeMatchesAddrHashSize (ScriptHash h) =
  KeyHash (Hash.castHash h)

-- Multi-signature scripts
singleKeyOnly :: Era era => Addr era -> MultiSig era
singleKeyOnly (Addr _ (KeyHashObj pk) _) = RequireSignature $ asWitness pk
singleKeyOnly _ = error "use VKey address"

aliceOnly :: Era era => proxy era -> MultiSig era
aliceOnly _ = singleKeyOnly Cast.aliceAddr

bobOnly :: Era era => proxy era -> MultiSig era
bobOnly _ = singleKeyOnly Cast.bobAddr

aliceOrBob :: Era era => proxy era -> MultiSig era
aliceOrBob p = RequireAnyOf [aliceOnly p, singleKeyOnly Cast.bobAddr]

aliceAndBob :: Era era => proxy era -> MultiSig era
aliceAndBob p = RequireAllOf [aliceOnly p, singleKeyOnly Cast.bobAddr]

aliceAndBobOrCarl :: Era era => proxy era -> MultiSig era
aliceAndBobOrCarl p = RequireMOf 1 [aliceAndBob p, singleKeyOnly Cast.carlAddr]

aliceAndBobOrCarlAndDaria :: Era era => proxy era -> MultiSig era
aliceAndBobOrCarlAndDaria p =
  RequireAnyOf
    [ aliceAndBob p,
      RequireAllOf [singleKeyOnly Cast.carlAddr, singleKeyOnly Cast.dariaAddr]
    ]

aliceAndBobOrCarlOrDaria :: Era era => proxy era -> MultiSig era
aliceAndBobOrCarlOrDaria p =
  RequireMOf
    1
    [ aliceAndBob p,
      RequireAnyOf [singleKeyOnly Cast.carlAddr, singleKeyOnly Cast.dariaAddr]
    ]

initTxBody :: ShelleyTest era => [(Addr era, Coin)] -> TxBody era
initTxBody addrs =
  TxBody
    (Set.fromList [TxIn genesisId 0, TxIn genesisId 1])
    (StrictSeq.fromList $ map (uncurry TxOut) addrs)
    Empty
    (Wdrl Map.empty)
    (Coin 0)
    (SlotNo 0)
    SNothing
    SNothing

makeTxBody ::
  ShelleyTest era =>
  [TxIn era] ->
  [(Addr era, Coin)] ->
  Wdrl era ->
  TxBody era
makeTxBody inp addrCs wdrl =
  TxBody
    (Set.fromList inp)
    (StrictSeq.fromList [uncurry TxOut addrC | addrC <- addrCs])
    Empty
    wdrl
    (Coin 0)
    (SlotNo 10)
    SNothing
    SNothing

makeTx ::
  forall era.
  (Mock (Crypto era), Shelley.TxBodyConstraints era) =>
  Core.TxBody era ->
  [KeyPair 'Witness (Crypto era)] ->
  Map (ScriptHash era) (MultiSig era) ->
  Maybe MetaData ->
  Tx era
makeTx txBody keyPairs msigs = Tx txBody wits . maybeToStrictMaybe
  where
    wits =
      mempty
        { addrWits = makeWitnessesVKey (coerce . hashAnnotated $ txBody) keyPairs,
          msigWits = msigs
        }

aliceInitCoin :: Coin
aliceInitCoin = Coin 10000

bobInitCoin :: Coin
bobInitCoin = Coin 1000

genesis :: forall era. ShelleyTest era => LedgerState era
genesis = genesisState genDelegs0 utxo0
  where
    genDelegs0 = Map.empty
    utxo0 =
      genesisCoins @era
        [ TxOut Cast.aliceAddr aliceInitCoin,
          TxOut Cast.bobAddr bobInitCoin
        ]

initPParams :: PParams era
initPParams = emptyPParams {_maxTxSize = 1000}

-- | Create an initial UTxO state where Alice has 'aliceInitCoin' and Bob
-- 'bobInitCoin' to spend. Then create and apply a transaction which, if
-- 'aliceKeep' is greater than 0, gives that amount to Alice and creates outputs
-- locked by a script for each pair of script, coin value in 'msigs'.
initialUTxOState ::
  forall era.
  ( ShelleyTest era,
    STS (UTXOW era),
    BaseM (UTXOW era) ~ ShelleyBase,
    Mock (Crypto era)
  ) =>
  Coin ->
  [(MultiSig era, Coin)] ->
  (TxId era, Either [[PredicateFailure (UTXOW era)]] (UTxOState era))
initialUTxOState aliceKeep msigs =
  let addresses =
        [(Cast.aliceAddr, aliceKeep) | aliceKeep > mempty]
          ++ map
            ( \(msig, era) ->
                ( Addr
                    Testnet
                    (ScriptHashObj $ hashScript msig)
                    (StakeRefBase $ ScriptHashObj $ hashScript msig),
                  era
                )
            )
            msigs
   in let tx =
            makeTx
              (initTxBody addresses)
              (asWitness <$> [Cast.alicePay, Cast.bobPay])
              Map.empty
              Nothing
       in ( txid $ _body tx,
            runShelleyBase $
              applySTSTest @(UTXOW era)
                ( TRC
                    ( UtxoEnv
                        (SlotNo 0)
                        initPParams
                        Map.empty
                        (GenDelegs Map.empty),
                      _utxoState genesis,
                      tx
                    )
                )
          )

-- | Start from genesis, consume Alice's and Bob's coins, create an output
-- spendable by Alice if 'aliceKeep' is greater than 0. For each pair of script
-- and coin value in 'lockScripts' create an output of that value locked by the
-- script. Sign the transaction with keys in 'signers'. Then create an
-- transaction that uses the scripts in 'unlockScripts' (and the output for
-- 'aliceKeep' in the case of it being non-zero) to spend all funds back to
-- Alice. Return resulting UTxO state or collected errors
applyTxWithScript ::
  forall proxy era.
  ( ShelleyTest era,
    STS (UTXOW era),
    BaseM (UTXOW era) ~ ShelleyBase,
    Mock (Crypto era)
  ) =>
  proxy era ->
  [(MultiSig era, Coin)] ->
  [MultiSig era] ->
  Wdrl era ->
  Coin ->
  [KeyPair 'Witness (Crypto era)] ->
  Either [[PredicateFailure (UTXOW era)]] (UTxOState era)
applyTxWithScript _ lockScripts unlockScripts wdrl aliceKeep signers = utxoSt'
  where
    (txId, initUtxo) = initialUTxOState aliceKeep lockScripts
    utxoSt = case initUtxo of
      Right utxoSt'' -> utxoSt''
      _ ->
        error
          ( "must fail test before: "
              ++ show initUtxo
          )
    txbody =
      makeTxBody
        inputs'
        [(Cast.aliceAddr, aliceInitCoin <> bobInitCoin <> fold (unWdrl wdrl))]
        wdrl
    inputs' =
      [ TxIn txId (fromIntegral n)
        | n <-
            [0 .. length lockScripts - (if aliceKeep > mempty then 0 else 1)]
      ]
    -- alice? + scripts
    tx =
      makeTx
        txbody
        signers
        (Map.fromList $ map (\scr -> (hashScript scr, scr)) unlockScripts)
        Nothing
    utxoSt' =
      runShelleyBase $
        applySTSTest @(UTXOW era)
          ( TRC
              ( UtxoEnv
                  (SlotNo 0)
                  initPParams
                  Map.empty
                  (GenDelegs Map.empty),
                utxoSt,
                tx
              )
          )
