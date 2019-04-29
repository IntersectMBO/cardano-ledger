{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Cardano.Chain.Txp.Model
  ( tests
  )
where

import Cardano.Prelude

import qualified Text.Show
import Data.IORef
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import Unsafe.Coerce

import qualified Cardano.Chain.Txp as Concrete
import qualified Control.State.Transition as STS
import qualified Ledger.Core as Abstract
import qualified Ledger.UTxO as Abstract

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Test.Cardano.Chain.Elaboration.UTxO (ConcreteResult(..))
import qualified Test.Cardano.Chain.Elaboration.UTxO as E


--------------------------------------------------------------------------------
-- UTXOW Property
--------------------------------------------------------------------------------

prop_commandUTXOW :: Property
prop_commandUTXOW =
  withDiscards 1000
    . withTests 100
    . property
    $ do

        -- Generate initial abstract TxOuts for the initial UTxO
        initialBalances <- forAll genInitialTxOuts

        let
          -- Elaborate the initial abstract UTxO to get the concrete state
          initialConcreteState =
            E.elaborateUTxO $ mkInitialAbstractUTxO initialBalances

        concreteRef <- liftIO $ newIORef initialConcreteState

        let
          env :: forall v . STS.Environment (UTXOW v)
          env = Abstract.UTxOEnv
            { Abstract.utxo0 = mkInitialAbstractUTxO initialBalances
            , Abstract.pps = Abstract.ProtocolConstants (\_ -> Abstract.Value 0)
            }

        -- Generate a sequence of UTxO update actions
        actions <- forAll $ Gen.sequential
          (Range.linear 1 50)
          (initialStateUTXOW env)
          [commandUTXOW concreteRef env]

        liftIO $ writeIORef concreteRef initialConcreteState

        -- Run the sequence of actions
        executeSequential (initialStateUTXOW env) actions
 where
  -- We have to use `unsafeCoerce` here to get an initial state for all @v@, but
  -- it is safe because the initial 'TxIn's don't use any 'Var' values
  mkInitialAbstractUTxO :: [Abstract.TxOut] -> E.UTxO v
  mkInitialAbstractUTxO = unsafeCoerce . mkInitialAbstractUTxO'

  mkInitialAbstractUTxO' :: [Abstract.TxOut] -> E.UTxO Symbolic
  mkInitialAbstractUTxO' = Abstract.UTxO . M.fromList . fmap mkUTxOEntry

  -- We emulate the concrete genesis UTxO logic that uses the output address to
  -- create a 'TxId'
  mkUTxOEntry :: Abstract.TxOut -> (E.TxIn Symbolic, Abstract.TxOut)
  mkUTxOEntry o@(Abstract.TxOut a _) = (Abstract.TxIn (Left a) 0, o)


--------------------------------------------------------------------------------
-- UTXOW State
--------------------------------------------------------------------------------

-- | Instantiate the UTXOW system at our special 'TxId' type
type UTXOW v = Abstract.UTXOW (E.TxId v)

-- | StateUTXOW keeps the underlying abstract state, as well as the last result
--   for comparison with errors in the concrete system
data StateUTXOW (v :: Type -> Type) = StateUTXOW
  { abstractState :: E.UTxO v
  , lastResult    :: Either [STS.PredicateFailure (UTXOW v)] (E.UTxO v)
  } deriving (Eq, Show)

initialStateUTXOW :: STS.Environment (UTXOW v) -> StateUTXOW v
initialStateUTXOW (Abstract.UTxOEnv initialUTxO _) =
  StateUTXOW {abstractState = initialUTxO, lastResult = Right initialUTxO}


--------------------------------------------------------------------------------
-- UTXOW Signal
--------------------------------------------------------------------------------

-- | The signal is the underlying signal type, a transactions with witnesses
newtype SignalUTXOW (v :: Type -> Type)
  = SignalUTXOW (STS.Signal (UTXOW v))
  deriving (Show)

instance HTraversable SignalUTXOW where
  htraverse
    :: forall f g h
    . Applicative f
    => (forall a . g a -> f (h a))
    -> SignalUTXOW g
    -> f (SignalUTXOW h)
  htraverse f (SignalUTXOW t) = SignalUTXOW <$> htraverseTxWits t
   where
    htraverseTxWits :: E.TxWits g -> f (E.TxWits h)
    htraverseTxWits (Abstract.TxWits body witnesses) =
      Abstract.TxWits
        <$> htraverseTx body
        <*> traverse htraverseWitness witnesses

    htraverseTx :: E.Tx g -> f (E.Tx h)
    htraverseTx (Abstract.Tx id inputs outputs) =
      Abstract.Tx
        <$> htraverseTxId id
        <*> traverse htraverseInput inputs
        <*> pure outputs

    htraverseInput :: E.TxIn g -> f (E.TxIn h)
    htraverseInput (Abstract.TxIn txId index) =
      Abstract.TxIn <$> htraverseTxId txId <*> pure index

    htraverseTxId :: E.TxId g -> f (E.TxId h)
    htraverseTxId (Left a)     = pure $ Left a
    htraverseTxId (Right txId) = Right <$> htraverse f txId

    htraverseWitness :: E.Wit g -> f (E.Wit h)
    htraverseWitness (Abstract.Wit key (Abstract.Sig tx owner)) =
      Abstract.Wit key <$> (Abstract.Sig <$> htraverseTx tx <*> pure owner)


--------------------------------------------------------------------------------
-- UTXOW Command
--------------------------------------------------------------------------------

commandUTXOW
  :: forall m
   . MonadIO m
  => IORef Concrete.UTxO
  -> (forall v . STS.Environment (UTXOW v))
  -> Command Gen m StateUTXOW
commandUTXOW concreteRef env = Command gen execute callbacks
 where
  -- Generate a transaction by selecting inputs from the `UTxO` and dividing the
  -- 'Value' equally among a number of outputs
  gen :: StateUTXOW Symbolic -> Maybe (Gen (SignalUTXOW Symbolic))
  gen StateUTXOW { abstractState } =
    Just
    .   fmap SignalUTXOW
    $   Abstract.makeTxWits abstractState
    <$> genAbstractTx
   where
    genAbstractTx :: Gen (E.Tx Symbolic)
    genAbstractTx = do
      ins  <- Gen.filter (not . null) (Gen.subsequence (M.keys m))
      outs <- genEqualTxOuts abstractState ins
      let placeholderId = Abstract.Addr (Abstract.VKey (Abstract.Owner 0))
      pure $ Abstract.Tx (Left placeholderId) ins outs
    Abstract.UTxO m = abstractState

  -- Elaborate the signal and apply it to the concrete 'UTxO' update function
  execute :: SignalUTXOW Concrete -> m ConcreteResult
  execute (SignalUTXOW txWits) = do
    concreteState <- liftIO $ readIORef concreteRef

    let
      concreteTxAux = E.elaborateTxWitsBS (Abstract.utxo0 env) txWits

      result :: Either Concrete.UTxOValidationError Concrete.UTxO
      result = Concrete.updateUTxOTxWitness
        (E.elaborateUTxOEnv env)
        concreteState
        concreteTxAux

    liftIO . writeIORef concreteRef $ fromRight concreteState result

    -- Return the result and the concrete transaction that was used
    pure $ ConcreteResult
      { crResult    = result
      , crAppliedTx = Concrete.taTx concreteTxAux
      }

  callbacks :: [Callback SignalUTXOW ConcreteResult StateUTXOW]
  callbacks =
    [ Require pre
    , Update update
    , Ensure $ \_ StateUTXOW { lastResult } _ concreteResult -> do
      annotateShow lastResult
      isRight lastResult === isRight (crResult concreteResult)
    ]

  -- Call through the 'applySTS', but updating the signal with the right 'TxId'
  update
    :: forall v
     . Ord1 v
    => StateUTXOW v
    -> SignalUTXOW v
    -> Var ConcreteResult v
    -> StateUTXOW v
  update StateUTXOW { abstractState } (SignalUTXOW txWits) concreteResult =
    let
      Abstract.TxWits { Abstract.body } = txWits

      txWits' = txWits
        { Abstract.body = body { Abstract.txid = Right concreteResult }
        }

      result =
        STS.applySTS @(UTXOW v) (STS.TRC (env, abstractState, txWits'))
    in StateUTXOW (fromRight abstractState result) result

  pre :: StateUTXOW Symbolic -> SignalUTXOW Symbolic -> Bool
  pre StateUTXOW { abstractState } (SignalUTXOW (Abstract.TxWits tx _)) =
    all (`elem` M.keys m) (Abstract.txins tx)
      && Abstract.balance (Abstract.txouts tx)
      == Abstract.balance
          (Set.fromList (Abstract.txins tx) Abstract.<| abstractState)
    where Abstract.UTxO m = abstractState


instance Show (Abstract.ProtocolConstants id) where
  show _ = "ProtocolConstants { pcMinFee = <function> }"


--------------------------------------------------------------------------------
-- UTXOW Generators
--------------------------------------------------------------------------------

genAbstractValue :: MonadGen m => m Abstract.Value
genAbstractValue =
  Abstract.Value . fromIntegral <$> Gen.word32 (Range.linear 1 1000000)

-- | Each address should only appear once in the starting balances
genInitialTxOuts :: MonadGen m => m [Abstract.TxOut]
genInitialTxOuts = do
  addrs <- Gen.filter (not . null) $ Gen.subsequence modelAddresses
  traverse (\a -> Abstract.TxOut a <$> genAbstractValue) addrs

-- | Divide the `Value` from a set of inputs equally between a number of
--   outputs, adding any remainder to the first output
genEqualTxOuts
  :: MonadGen m => E.UTxO Symbolic -> [E.TxIn Symbolic] -> m [Abstract.TxOut]
genEqualTxOuts utxo ins = do
  -- Choose a number of outputs
  noOuts <- Gen.int (Range.linear 1 (min (fromIntegral inputBalance) 10))

  -- Split the input balance equally between the outputs
  let
    outputBalance = Abstract.Value $ inputBalance `div` fromIntegral noOuts
    Abstract.Value ob = outputBalance
    remainder = Abstract.Value $ inputBalance - (ob * fromIntegral noOuts)

  let
    balances =
      (outputBalance <> remainder) : replicate (noOuts - 1) outputBalance

  -- Generate the right number of addresses
  addrs <- replicateM noOuts genModelAddr

  -- Convert to TxOuts
  pure $ uncurry Abstract.TxOut <$> zip addrs balances
 where
  Abstract.Value inputBalance =
    Abstract.balance (Set.fromList ins Abstract.<| utxo)

genModelAddr :: MonadGen m => m Abstract.Addr
genModelAddr = Gen.element modelAddresses

modelAddresses :: [Abstract.Addr]
modelAddresses = Abstract.Addr . Abstract.VKey . Abstract.Owner <$> [0 .. 5]


--------------------------------------------------------------------------------
-- Main Test Export
--------------------------------------------------------------------------------

tests :: IO Bool
tests = checkSequential $$discover
