{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

-- | Base for a modular STS example
module Control.State.Transition.Examples.Modular.Base where

import Control.State.Transition
import qualified Control.State.Transition.Examples.Modular.Base.Types as BT
import Data.Group (Group, invert)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.Monoid (Sum (..))
import Optics

data Era_0

newtype Coin = Coin Int
  deriving newtype (Eq, Ord, Show)
  deriving
    (Group, Semigroup, Monoid)
    via Sum Int

type instance BT.Coin Era_0 = Coin

newtype AccountName = AccountName String
  deriving newtype (Eq, Ord, Show)

type instance BT.AccountName Era_0 = AccountName

data Account e = Account
  { name :: BT.AccountName e,
    balance :: BT.Coin e
  }

deriving stock instance
  ( Eq (BT.AccountName e),
    Eq (BT.Coin e)
  ) =>
  Eq (Account e)

deriving stock instance
  ( Show (BT.AccountName e),
    Show (BT.Coin e)
  ) =>
  Show (Account e)

type instance BT.Account Era_0 = Account Era_0

data Tx e = Tx
  { fromAcnt :: BT.AccountName e,
    toAcnt :: BT.AccountName e,
    amount :: BT.Coin e
  }

type instance BT.Tx Era_0 = Tx Era_0

data Admin e
  = OpenAccount (BT.AccountName e) (BT.Coin e)
  | Withdraw (BT.AccountName e) (BT.Coin e)
  | CloseAccount (BT.AccountName e)

type instance BT.Admin Era_0 = Admin Era_0

data Op e
  = OpTx (BT.Tx e)
  | OpAdmin (BT.Admin e)

type instance BT.Op Era_0 = Op Era_0

newtype Accounting e = Accounting
  { unAccounting :: Map (BT.AccountName e) (BT.Account e)
  }

type instance BT.Accounting Era_0 = Accounting Era_0

newtype BlockId = BlockId String

type instance BT.BlockId Era_0 = BlockId

data Block e = Block
  { blockId :: BT.BlockId e,
    blockOps :: [BT.Op e]
  }

type instance BT.Block Era_0 = Block Era_0

data ChainState e = ChainState
  { accountState :: BT.Accounting e,
    lastBlock :: BT.BlockId e
  }

type instance BT.ChainState Era_0 = ChainState Era_0

-------------------------------------------------------------------------------
-- STS plumbing
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Transaction processing
-------------------------------------------------------------------------------
data TX e

instance STS (TX Era_0) where
  type Environment (TX Era_0) = ()
  type State (TX Era_0) = BT.Accounting Era_0
  type Signal (TX Era_0) = BT.Tx Era_0

  data PredicateFailure (TX Era_0)
    = InsufficientMoney (BT.Account Era_0) (BT.Coin Era_0)
    | NoSuchAccount (BT.AccountName Era_0)
    deriving stock (Eq, Show)

  initialRules = []

  transitionRules = [processTx]

processTx :: TransitionRule (TX Era_0)
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

data ADMIN e

instance STS (ADMIN Era_0) where
  type Environment (ADMIN Era_0) = ()
  type State (ADMIN Era_0) = (BT.Accounting Era_0)
  type Signal (ADMIN Era_0) = (BT.Admin Era_0)

  data PredicateFailure (ADMIN Era_0)
    = AccountAlreadyExists (BT.AccountName Era_0)
    | CloseNonEmptyAccount (BT.Account Era_0)
    | NonExistingAccount (BT.AccountName Era_0)
    | WithdrawInsufficientBalance (BT.Account Era_0) (BT.Coin Era_0)
    deriving stock (Eq, Show)

  initialRules = []
  transitionRules = [processAdmin]

processAdmin :: TransitionRule (ADMIN Era_0)
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

data OPS e

instance STS (OPS Era_0) where
  type Environment (OPS Era_0) = ()
  type State (OPS Era_0) = (BT.Accounting Era_0)
  type Signal (OPS Era_0) = [BT.Op Era_0]

  data PredicateFailure (OPS Era_0)
    = TXFailure (PredicateFailure (TX Era_0))
    | ADMINFailure (PredicateFailure (ADMIN Era_0))
    deriving stock (Eq, Show)

  initialRules = []

  transitionRules =
    [processOps @(OPS Era_0) @(TX Era_0) @(ADMIN Era_0) @Era_0]

instance Embed (TX Era_0) (OPS Era_0) where
  wrapFailed = TXFailure

instance Embed (ADMIN Era_0) (OPS Era_0) where
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
  forall ops tx admin e.
  ( HasLens (Signal ops) [Op e],
    HasLens (State ops) (BT.Accounting e),
    HasGetter (BT.Tx e) (Signal tx),
    HasGetter (BT.Accounting e) (State tx),
    HasGetter (Environment ops) (Environment tx),
    HasGetter (BT.Admin e) (Signal admin),
    HasGetter (BT.Accounting e) (State admin),
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

  case extract @_ @([Op e]) ops of
    x : xs -> case x of
      OpTx tx ->
        do
          txs <-
            trans @tx
              ( TRC
                  ( extract env,
                    extract $
                      extract @_ @(BT.Accounting e)
                        st,
                    extract tx
                  )
              )
          trans @ops (TRC (env, inject txs st, inject xs ops))
      OpAdmin admin ->
        do
          st' <-
            trans @admin
              ( TRC
                  ( extract env,
                    extract $
                      extract @_ @(BT.Accounting e)
                        st,
                    extract admin
                  )
              )
          trans @ops (TRC (env, inject st' st, inject xs ops))
    [] -> pure st

data CHAIN e

instance STS (CHAIN Era_0) where
  type Environment (CHAIN Era_0) = ()
  type State (CHAIN Era_0) = BT.ChainState Era_0
  type Signal (CHAIN Era_0) = BT.Block Era_0

  data PredicateFailure (CHAIN Era_0)
    = OPFailure (PredicateFailure (OPS Era_0))
    deriving stock (Eq, Show)

  initialRules = []

  transitionRules = []
