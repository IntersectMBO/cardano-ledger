{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

-- | Base for a modular STS example
module Control.State.Transition.Examples.Modular.Base where

import Control.State.Transition
import Data.Group (Group, invert)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.Monoid (Sum (..))
import Optics

newtype Coin = Coin Int
  deriving newtype (Eq, Ord, Show)
  deriving
    (Group, Semigroup, Monoid)
    via Sum Int

newtype AccountName = AccountName String
  deriving newtype (Eq, Ord, Show)

data Account = Account
  { name :: AccountName,
    balance :: Coin
  }
  deriving stock (Eq, Show)

data Tx = Tx
  { fromAcnt :: AccountName,
    toAcnt :: AccountName,
    amount :: Coin
  }

data Admin
  = OpenAccount AccountName Coin
  | Withdraw AccountName Coin
  | CloseAccount AccountName

data Op
  = OpTx Tx
  | OpAdmin Admin

newtype Accounting = Accounting {unAccounting :: Map AccountName Account}

newtype BlockId = BlockId String

data Block = Block
  { blockId :: BlockId,
    blockOps :: [Op]
  }

data ChainState = ChainState
  { accountState :: Accounting,
    lastBlock :: BlockId
  }

-------------------------------------------------------------------------------
-- STS plumbing
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Transaction processing
-------------------------------------------------------------------------------
data TX

instance STS TX where
  type Environment TX = ()
  type State TX = Accounting
  type Signal TX = Tx

  data PredicateFailure TX
    = InsufficientMoney Account Coin
    | NoSuchAccount AccountName
    deriving stock (Eq, Show)

  initialRules = []

  transitionRules = [processTx]

processTx :: TransitionRule TX
processTx = do
  TRC
    ( (),
      unAccounting -> accounting,
      Tx {fromAcnt, toAcnt, amount}
      ) <-
    judgmentContext

  accounting' <- case ( Map.lookup fromAcnt accounting,
                        Map.lookup toAcnt accounting
                      ) of
    (Just f, Just t) -> do
      balance f >= amount ?! InsufficientMoney f amount
      pure $
        Map.insert fromAcnt (f {balance = balance f <> invert amount})
          . Map.insert toAcnt (t {balance = balance t <> amount})
          $ accounting
    (mf, mt) -> do
      isJust mf ?! NoSuchAccount fromAcnt
      isJust mt ?! NoSuchAccount toAcnt
      pure accounting

  pure $! Accounting accounting'

data ADMIN

instance STS ADMIN where
  type Environment ADMIN = ()
  type State ADMIN = Accounting
  type Signal ADMIN = Admin

  data PredicateFailure ADMIN
    = AccountAlreadyExists AccountName
    | CloseNonEmptyAccount Account
    | NonExistingAccount AccountName
    | WithdrawInsufficientBalance Account Coin
    deriving stock (Eq, Show)

  initialRules = []
  transitionRules = [processAdmin]

processAdmin :: TransitionRule ADMIN
processAdmin = do
  TRC ((), unAccounting -> accounting, admin) <- judgmentContext
  case admin of
    OpenAccount withName withBalance -> do
      Map.notMember withName accounting ?! AccountAlreadyExists withName
      pure . Accounting $
        Map.insert
          withName
          (Account withName withBalance)
          accounting
    CloseAccount withName -> do
      accounting' <- case Map.lookup withName accounting of
        Nothing ->
          (failBecause $ NonExistingAccount withName)
            >> pure accounting
        Just x
          | balance x /= mempty ->
            (failBecause $ CloseNonEmptyAccount x) >> pure accounting
        _ -> pure $ Map.delete withName accounting

      pure $! Accounting accounting'
    Withdraw fromAcnt amt ->
      Accounting <$> case Map.lookup fromAcnt accounting of
        Nothing ->
          (failBecause $ NonExistingAccount fromAcnt)
            >> pure accounting
        Just x
          | balance x < amt ->
            (failBecause $ WithdrawInsufficientBalance x amt)
              >> pure accounting
        Just x ->
          pure $
            Map.insert
              fromAcnt
              (x {balance = balance x <> invert amt})
              accounting

data OPS

instance STS OPS where
  type Environment OPS = ()
  type State OPS = Accounting
  type Signal OPS = [Op]

  data PredicateFailure OPS
    = TXFailure (PredicateFailure TX)
    | ADMINFailure (PredicateFailure ADMIN)
    deriving stock (Eq, Show)

  initialRules = []

  transitionRules = [processOps @OPS @TX @ADMIN]

instance Embed TX OPS where
  wrapFailed = TXFailure

instance Embed ADMIN OPS where
  wrapFailed = ADMINFailure

class (HasGetter a b, HasSetter a b) => HasLens a b where
  lensy :: Lens' a b
  lensy = lens (view gettery) (flip $ set settery)

extract :: HasGetter a b => a -> b
extract = view gettery

inject :: HasSetter a b => b -> a -> a
inject = set settery

class HasGetter a b where
  gettery :: Getter a b

class HasSetter a b where
  settery :: Setter' a b

instance HasGetter a a where
  gettery = castOptic equality

instance HasSetter a a where
  settery = castOptic equality

instance HasLens a a where
  lensy = castOptic equality

processOps ::
  forall ops tx admin.
  ( HasLens (Signal ops) [Op],
    HasLens (State ops) Accounting,
    HasGetter Tx (Signal tx),
    HasGetter Accounting (State tx),
    HasGetter (Environment ops) (Environment tx),
    HasGetter Admin (Signal admin),
    HasGetter Accounting (State admin),
    HasGetter (Environment ops) (Environment admin),
    HasSetter (State ops) (State tx),
    HasSetter (State ops) (State admin),
    Embed tx ops,
    Embed admin ops
  ) =>
  TransitionRule ops
processOps = do
  TRC (env, st, ops) <-
    judgmentContext

  case extract ops of
    x : xs -> case x of
      OpTx tx ->
        do
          txs <- trans @tx (TRC (extract env, extract $ extract @_ @Accounting st, extract tx))
          trans @ops (TRC (env, inject txs st, inject xs ops))
      OpAdmin admin ->
        do
          st' <- trans @admin (TRC (extract env, extract $ extract @_ @Accounting st, extract admin))
          trans @ops (TRC (env, inject st' st, inject xs ops))
    [] -> pure st

data CHAIN

instance STS CHAIN where
  type Environment CHAIN = ()
  type State CHAIN = ChainState
  type Signal CHAIN = Block

  data PredicateFailure CHAIN
    = OPFailure (PredicateFailure OPS)
    deriving stock (Eq, Show)

  initialRules = []

  transitionRules = []
