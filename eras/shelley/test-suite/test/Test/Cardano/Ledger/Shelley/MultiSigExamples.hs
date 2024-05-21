{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Test.Cardano.Ledger.Shelley.MultiSigExamples (
  applyTxWithScript,
  aliceOnly,
  bobOnly,
  aliceAndBob,
  aliceOrBob,
  aliceAndBobOrCarl,
  aliceAndBobOrCarlAndDaria,
  aliceAndBobOrCarlOrDaria,
) where

import qualified Cardano.Crypto.Hash as Hash
import Cardano.Ledger.Address (Addr (Addr))
import Cardano.Ledger.BaseTypes (
  Network (..),
  StrictMaybe (..),
  maybeToStrictMaybe,
  mkTxIxPartial,
 )
import Cardano.Ledger.Coin
import Cardano.Ledger.Credential (
  pattern KeyHashObj,
  pattern ScriptHashObj,
  pattern StakeRefBase,
 )
import Cardano.Ledger.Keys (
  KeyHash (..),
  KeyRole (..),
  asWitness,
 )
import Cardano.Ledger.SafeHash (hashAnnotated)
import Cardano.Ledger.Shelley (ShelleyEra)
import Cardano.Ledger.Shelley.Core
import Cardano.Ledger.Shelley.LedgerState (
  LedgerState (..),
  UTxOState,
  genesisState,
 )
import Cardano.Ledger.Shelley.Rules (ShelleyUTXOW, UtxoEnv (..))
import Cardano.Ledger.Shelley.Scripts (
  MultiSig,
  ShelleyEraScript,
  pattern RequireAllOf,
  pattern RequireAnyOf,
  pattern RequireMOf,
  pattern RequireSignature,
 )
import Cardano.Ledger.Shelley.Tx (ShelleyTx (..))
import Cardano.Ledger.Shelley.TxAuxData (ShelleyTxAuxData)
import Cardano.Ledger.Shelley.TxBody (ShelleyTxBody (..))
import Cardano.Ledger.Shelley.TxWits (ShelleyTxWits (..))
import Cardano.Ledger.Slot (SlotNo (..))
import Cardano.Ledger.TxIn (TxId, TxIn (..))
import qualified Cardano.Ledger.Val as Val
import Control.State.Transition
import Data.Default.Class (Default (def))
import Data.Foldable (fold)
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map (empty, fromList)
import Data.Sequence.Strict (StrictSeq (..))
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set (fromList)
import Lens.Micro
import Test.Cardano.Ledger.Core.KeyPair (KeyPair (..), mkWitnessesVKey)
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes (
  Mock,
 )
import qualified Test.Cardano.Ledger.Shelley.Examples.Cast as Cast
import Test.Cardano.Ledger.Shelley.Generator.Core (
  genesisCoins,
 )
import Test.Cardano.Ledger.Shelley.Generator.EraGen (genesisId)
import Test.Cardano.Ledger.Shelley.Generator.ShelleyEraGen ()
import Test.Cardano.Ledger.Shelley.Utils

-- Multi-Signature tests

-- This compile-time test asserts that the script hash and key hash use the
-- same hash size and indeed hash function. We do this by checking we can
-- type-check the following code that converts between them by using the hash
-- casting function which changes what the hash is of, without changing the
-- hashing algorithm.
--
_assertScriptHashSizeMatchesAddrHashSize ::
  ScriptHash c ->
  KeyHash r c
_assertScriptHashSizeMatchesAddrHashSize (ScriptHash h) =
  KeyHash (Hash.castHash h)

-- Multi-signature scripts
singleKeyOnly :: ShelleyEraScript era => Addr (EraCrypto era) -> NativeScript era
singleKeyOnly (Addr _ (KeyHashObj pk) _) = RequireSignature $ asWitness pk
singleKeyOnly _ = error "use VKey address"

aliceOnly :: ShelleyEraScript era => NativeScript era
aliceOnly = singleKeyOnly Cast.aliceAddr

bobOnly :: ShelleyEraScript era => NativeScript era
bobOnly = singleKeyOnly Cast.bobAddr

aliceOrBob :: ShelleyEraScript era => NativeScript era
aliceOrBob = RequireAnyOf (StrictSeq.fromList [aliceOnly, singleKeyOnly Cast.bobAddr])

aliceAndBob :: ShelleyEraScript era => NativeScript era
aliceAndBob = RequireAllOf (StrictSeq.fromList [aliceOnly, singleKeyOnly Cast.bobAddr])

aliceAndBobOrCarl :: ShelleyEraScript era => NativeScript era
aliceAndBobOrCarl = RequireMOf 1 (StrictSeq.fromList [aliceAndBob, singleKeyOnly Cast.carlAddr])

aliceAndBobOrCarlAndDaria :: ShelleyEraScript era => NativeScript era
aliceAndBobOrCarlAndDaria =
  RequireAnyOf $
    StrictSeq.fromList
      [ aliceAndBob
      , RequireAllOf (StrictSeq.fromList [singleKeyOnly Cast.carlAddr, singleKeyOnly Cast.dariaAddr])
      ]

aliceAndBobOrCarlOrDaria ::
  ShelleyEraScript era =>
  NativeScript era
aliceAndBobOrCarlOrDaria =
  RequireMOf
    1
    $ StrictSeq.fromList
      [ aliceAndBob
      , RequireAnyOf (StrictSeq.fromList [singleKeyOnly Cast.carlAddr, singleKeyOnly Cast.dariaAddr])
      ]

initTxBody ::
  ( EraTxOut era
  , EraTxCert era
  ) =>
  [(Addr (EraCrypto era), Value era)] ->
  ShelleyTxBody era
initTxBody addrs =
  ShelleyTxBody
    (Set.fromList [TxIn genesisId minBound, TxIn genesisId (mkTxIxPartial 1)])
    (StrictSeq.fromList $ map (uncurry mkBasicTxOut) addrs)
    Empty
    (Withdrawals Map.empty)
    (Coin 0)
    (SlotNo 0)
    SNothing
    SNothing

makeTxBody ::
  ( EraTxOut era
  , EraTxCert era
  ) =>
  [TxIn (EraCrypto era)] ->
  [(Addr (EraCrypto era), Value era)] ->
  Withdrawals (EraCrypto era) ->
  ShelleyTxBody era
makeTxBody inp addrCs wdrl =
  ShelleyTxBody
    (Set.fromList inp)
    (StrictSeq.fromList [uncurry mkBasicTxOut addrC | addrC <- addrCs])
    Empty
    wdrl
    (Coin 0)
    (SlotNo 10)
    SNothing
    SNothing

makeTx ::
  forall c.
  Mock c =>
  TxBody (ShelleyEra c) ->
  [KeyPair 'Witness c] ->
  Map (ScriptHash c) (MultiSig (ShelleyEra c)) ->
  Maybe (ShelleyTxAuxData (ShelleyEra c)) ->
  ShelleyTx (ShelleyEra c)
makeTx txBody keyPairs msigs = ShelleyTx txBody txWits . maybeToStrictMaybe
  where
    txWits :: ShelleyTxWits (ShelleyEra c)
    txWits =
      mkBasicTxWits
        & addrTxWitsL .~ mkWitnessesVKey (hashAnnotated txBody) keyPairs
        & scriptTxWitsL .~ msigs

aliceInitCoin :: Coin
aliceInitCoin = Coin 10000

bobInitCoin :: Coin
bobInitCoin = Coin 1000

genesis ::
  forall era.
  ( EraTxOut era
  , EraGov era
  ) =>
  LedgerState era
genesis = genesisState genDelegs0 utxo0
  where
    genDelegs0 = Map.empty
    utxo0 =
      genesisCoins @era
        genesisId
        [ mkBasicTxOut Cast.aliceAddr (Val.inject aliceInitCoin)
        , mkBasicTxOut Cast.bobAddr (Val.inject bobInitCoin)
        ]

initPParams :: EraPParams era => PParams era
initPParams =
  emptyPParams
    & ppMaxTxSizeL .~ 1000

-- | Create an initial UTxO state where Alice has 'aliceInitCoin' and Bob
-- 'bobInitCoin' to spend. Then create and apply a transaction which, if
-- 'aliceKeep' is greater than 0, gives that amount to Alice and creates outputs
-- locked by a script for each pair of script, coin value in 'msigs'.
initialUTxOState ::
  forall c.
  Mock c =>
  Coin ->
  [(MultiSig (ShelleyEra c), Coin)] ->
  ( TxId c
  , Either
      (NonEmpty (PredicateFailure (ShelleyUTXOW (ShelleyEra c))))
      (UTxOState (ShelleyEra c))
  )
initialUTxOState aliceKeep msigs =
  let addresses =
        [(Cast.aliceAddr, aliceKeep) | Val.pointwise (>) aliceKeep mempty]
          ++ map
            ( \(msig, era) ->
                ( Addr
                    Testnet
                    (ScriptHashObj $ hashScript @(ShelleyEra c) msig)
                    (StakeRefBase $ ScriptHashObj $ hashScript @(ShelleyEra c) msig)
                , era
                )
            )
            msigs
   in let tx =
            makeTx
              (initTxBody addresses)
              (asWitness <$> [Cast.alicePay, Cast.bobPay])
              Map.empty
              Nothing
       in ( txIdTx tx
          , runShelleyBase $
              applySTSTest @(ShelleyUTXOW (ShelleyEra c))
                ( TRC
                    ( UtxoEnv
                        (SlotNo 0)
                        initPParams
                        def
                    , lsUTxOState genesis
                    , tx
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
  forall c.
  Mock c =>
  [(MultiSig (ShelleyEra c), Coin)] ->
  [MultiSig (ShelleyEra c)] ->
  Withdrawals c ->
  Coin ->
  [KeyPair 'Witness c] ->
  Either (NonEmpty (PredicateFailure (ShelleyUTXOW (ShelleyEra c)))) (UTxOState (ShelleyEra c))
applyTxWithScript lockScripts unlockScripts wdrl aliceKeep signers = utxoSt'
  where
    (txId, initUtxo) = initialUTxOState aliceKeep lockScripts
    utxoSt = case initUtxo of
      Right utxoSt'' -> utxoSt''
      _ -> error $ "must fail test before: " ++ show initUtxo
    txbody =
      makeTxBody
        inputs'
        [(Cast.aliceAddr, Val.inject $ aliceInitCoin <> bobInitCoin <> fold (unWithdrawals wdrl))]
        wdrl
    inputs' =
      [ TxIn txId (mkTxIxPartial (toInteger n))
      | n <-
          [0 .. length lockScripts - if Val.pointwise (>) aliceKeep mempty then 0 else 1]
      ]
    -- alice? + scripts
    tx =
      makeTx
        txbody
        signers
        (Map.fromList $ map (\scr -> (hashScript @(ShelleyEra c) scr, scr)) unlockScripts)
        Nothing
    utxoSt' =
      runShelleyBase $
        applySTSTest @(ShelleyUTXOW (ShelleyEra c))
          ( TRC
              ( UtxoEnv
                  (SlotNo 0)
                  initPParams
                  def
              , utxoSt
              , tx
              )
          )
