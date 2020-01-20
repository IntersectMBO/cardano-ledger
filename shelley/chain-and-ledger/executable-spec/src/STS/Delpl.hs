{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module STS.Delpl
  ( DELPL
  , DelplEnv (..)
  , PredicateFailure(..)
  )
where

import           BaseTypes
import           Cardano.Binary (FromCBOR (..), ToCBOR (..), decodeListLen, decodeWord,
                     encodeListLen, matchSize)
import           Cardano.Ledger.Shelley.Crypto
import           Cardano.Prelude (NoUnexpectedThunks (..))
import           Coin (Coin)
import           Control.State.Transition
import           Data.Typeable (Typeable)
import           Data.Word (Word8)
import           Delegation.Certificates
import           GHC.Generics (Generic)
import           LedgerState (DPState, emptyDelegation, _dstate, _pstate)
import           PParams hiding (d)
import           Slot
import           STS.Deleg (DELEG, DelegEnv (..))
import           STS.Pool
import           TxData

data DELPL crypto

data DelplEnv
  = DelplEnv
    { delplSlotNo   :: SlotNo
    , delPlPtr      :: Ptr
    , delPlPp       :: PParams
    , delPlReserves :: Coin
    }

instance
  Crypto crypto
  => STS (DELPL crypto)
 where
  type State (DELPL crypto)       = DPState crypto
  type Signal (DELPL crypto)      = DCert crypto
  type Environment (DELPL crypto) = DelplEnv
  type BaseM (DELPL crypto) = ShelleyBase
  data PredicateFailure (DELPL crypto)
    = PoolFailure (PredicateFailure (POOL crypto))
    | DelegFailure (PredicateFailure (DELEG crypto))
    | ScriptNotInWitnessDELPL
    | ScriptHashNotMatchDELPL
    | ScriptDoesNotValidateDELPL
    deriving (Show, Eq, Generic)

  initialRules    = [ pure emptyDelegation ]
  transitionRules = [ delplTransition      ]

instance NoUnexpectedThunks (PredicateFailure (DELPL crypto))

instance
  (Typeable crypto, Crypto crypto)
  => ToCBOR (PredicateFailure (DELPL crypto))
 where
   toCBOR = \case
      (PoolFailure a)            -> encodeListLen 2 <> toCBOR (0 :: Word8)
                                      <> toCBOR a
      (DelegFailure a)           -> encodeListLen 2 <> toCBOR (1 :: Word8)
                                      <> toCBOR a
      ScriptNotInWitnessDELPL    -> encodeListLen 1 <> toCBOR (2 :: Word8)
      ScriptHashNotMatchDELPL    -> encodeListLen 1 <> toCBOR (3 :: Word8)
      ScriptDoesNotValidateDELPL -> encodeListLen 1 <> toCBOR (4 :: Word8)

instance
  (Crypto crypto)
  => FromCBOR (PredicateFailure (DELPL crypto))
 where
  fromCBOR = do
    n <- decodeListLen
    decodeWord >>= \case
      0 -> do
        matchSize "PoolFailure" 2 n
        a <- fromCBOR
        pure $ PoolFailure a
      1 -> do
        matchSize "DelegFailure" 2 n
        a <- fromCBOR
        pure $ DelegFailure a
      2 -> matchSize "ScriptNotInWitnessDELPL" 1 n >>
             pure ScriptNotInWitnessDELPL
      3 -> matchSize "ScriptHashNotMatchDELPL" 1 n >>
             pure ScriptHashNotMatchDELPL
      4 -> matchSize "ScriptDoesNotValidateDELPL" 1 n >>
             pure ScriptDoesNotValidateDELPL
      k -> invalidKey k


delplTransition
  :: forall crypto
   . Crypto crypto
  => TransitionRule (DELPL crypto)
delplTransition = do
  TRC (DelplEnv slot ptr pp reserves, d, c) <- judgmentContext
  case c of
    DCertPool (RegPool _) -> do
      ps <-
        trans @(POOL crypto) $ TRC (PoolEnv slot pp, _pstate d, c)
      pure $ d { _pstate = ps }
    DCertPool (RetirePool _ _) -> do
      ps <-
        trans @(POOL crypto) $ TRC (PoolEnv slot pp, _pstate d, c)
      pure $ d { _pstate = ps }
    DCertGenesis (GenesisDelegate _) -> do
      ds <-
        trans @(DELEG crypto) $ TRC (DelegEnv slot ptr reserves, _dstate d, c)
      pure $ d { _dstate = ds }

    DCertDeleg (RegKey _) -> do
      ds <-
        trans @(DELEG crypto) $ TRC (DelegEnv slot ptr reserves, _dstate d, c)
      pure $ d { _dstate = ds }

    DCertDeleg (DeRegKey _) -> do
      ds <-
        trans @(DELEG crypto) $ TRC (DelegEnv slot ptr reserves, _dstate d, c)
      pure $ d { _dstate = ds }

    DCertDeleg (Delegate _) -> do
      ds <-
        trans @(DELEG crypto) $ TRC (DelegEnv slot ptr reserves , _dstate d, c)
      pure $ d { _dstate = ds }

    DCertMir _ -> do
      ds <- trans @(DELEG crypto) $ TRC (DelegEnv slot ptr reserves , _dstate d, c)
      pure $ d { _dstate = ds }


instance
  Crypto crypto
  => Embed (POOL crypto) (DELPL crypto)
 where
  wrapFailed = PoolFailure

instance
  Crypto crypto
  => Embed (DELEG crypto) (DELPL crypto)
 where
  wrapFailed = DelegFailure
