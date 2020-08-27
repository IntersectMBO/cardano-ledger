{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
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
import GHC.Records
import GHC.TypeLits
import Optics

-------------------------------------------------------------------------------
-- Lens stuff
-------------------------------------------------------------------------------
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

-- | For use with `DerivingVia`, when we have the common case of a given field.
newtype NamedOptic (name :: Symbol) s a = NamedOptic {unNamedOptic :: s}

instance LabelOptic name A_Lens s s a a => HasGetter (NamedOptic name s a) a where
  gettery = to unNamedOptic % labelOptic @name

instance LabelOptic name A_Lens s s a a => HasSetter (NamedOptic name s a) a where
  settery = castOptic $ coercedTo @s % labelOptic @name % coercedTo

instance LabelOptic name A_Lens s s a a => HasLens (NamedOptic name s a) a

-------------------------------------------------------------------------------
-- Era_0
-------------------------------------------------------------------------------

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
-- Transaction processing
-------------------------------------------------------------------------------
data TX

data TXPredicateFailure e
  = InsufficientMoney (BT.Account e) (BT.Coin e)
  | NoSuchAccount (BT.AccountName e)

deriving stock instance Eq (TXPredicateFailure Era_0)

deriving stock instance Show (TXPredicateFailure Era_0)

instance STS (TX) where
  type Environment (TX) = ()
  type State (TX) = BT.Accounting Era_0
  type Signal (TX) = BT.Tx Era_0

  type
    PredicateFailure (TX) =
      TXPredicateFailure Era_0

  initialRules = []

  transitionRules = [processTx @TX @Era_0]

processTx ::
  forall tx e.
  ( HasField "fromAcnt" (BT.Tx e) (BT.AccountName e),
    HasField "toAcnt" (BT.Tx e) (BT.AccountName e),
    HasField "amount" (BT.Tx e) (BT.Coin e),
    HasLens (State tx) (Accounting e),
    PredicateFailure tx ~ TXPredicateFailure e,
    BT.Account e ~ Account e,
    Signal tx ~ BT.Tx e,
    Ord (BT.AccountName e),
    Ord (BT.Coin e),
    Group (BT.Coin e)
  ) =>
  TransitionRule tx
processTx = do
  TRC (_, st, tx) <- judgmentContext

  let accounting = unAccounting $ extract @_ @(Accounting e) st
      fromAcnt = getField @"fromAcnt" tx
      toAcnt = getField @"toAcnt" tx
      amount = getField @"amount" @_ @(BT.Coin e) tx

  accounting' <- case ( Map.lookup fromAcnt accounting,
                        Map.lookup toAcnt accounting
                      ) of
    (Just f, Just t) -> do
      balance @e f >= amount ?! InsufficientMoney f amount
      pure $
        Map.insert fromAcnt (f {balance = balance f <> invert amount})
          . Map.insert toAcnt (t {balance = balance t <> amount})
          $ accounting
    (mf, mt) -> do
      isJust mf ?! NoSuchAccount fromAcnt
      isJust mt ?! NoSuchAccount toAcnt
      pure accounting

  pure . flip inject st $! Accounting @e accounting'

data ADMIN

data AdminPredicateFailure e
  = AccountAlreadyExists (BT.AccountName e)
  | CloseNonEmptyAccount (BT.Account e)
  | NonExistingAccount (BT.AccountName e)
  | WithdrawInsufficientBalance (BT.Account e) (BT.Coin e)

deriving stock instance Eq (AdminPredicateFailure Era_0)

deriving stock instance Show (AdminPredicateFailure Era_0)

instance STS (ADMIN) where
  type Environment (ADMIN) = ()
  type State (ADMIN) = (BT.Accounting Era_0)
  type Signal (ADMIN) = (BT.Admin Era_0)

  type PredicateFailure (ADMIN) = AdminPredicateFailure Era_0

  initialRules = []
  transitionRules = [processAdmin @ADMIN @Era_0]

processAdmin ::
  forall admin e.
  ( HasLens (State admin) (Accounting e),
    HasGetter (Signal admin) (Admin e),
    PredicateFailure admin ~ AdminPredicateFailure e,
    BT.Account e ~ Account e,
    Ord (BT.AccountName e),
    Ord (BT.Coin e),
    Group (BT.Coin e)
  ) =>
  TransitionRule admin
processAdmin = do
  TRC (_, st, extract @_ @(Admin e) -> admin) <- judgmentContext
  let accounting = unAccounting $ extract @_ @(Accounting e) st
  case admin of
    OpenAccount withName withBalance -> do
      Map.notMember withName accounting ?! AccountAlreadyExists withName
      pure . flip inject st . Accounting @e $
        Map.insert
          withName
          (Account @e withName withBalance)
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

      pure $! inject (Accounting @e accounting') st
    Withdraw fromAcnt amt ->
      flip inject st . Accounting @e <$> case Map.lookup fromAcnt accounting of
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

data OPSPredicateFailure e
  = TXFailure (TXPredicateFailure e)
  | ADMINFailure (AdminPredicateFailure e)

deriving stock instance Eq (OPSPredicateFailure Era_0)

deriving stock instance Show (OPSPredicateFailure Era_0)

instance STS (OPS) where
  type Environment (OPS) = ()
  type State (OPS) = (BT.Accounting Era_0)
  type Signal (OPS) = [BT.Op Era_0]

  type PredicateFailure (OPS) = OPSPredicateFailure Era_0

  initialRules = []

  transitionRules =
    [processOps @(OPS) @TX @(ADMIN) @Era_0]

instance Embed (TX) (OPS) where
  wrapFailed = TXFailure

instance Embed (ADMIN) (OPS) where
  wrapFailed = ADMINFailure

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

data CHAIN

data CHAINPredicateFailure
  = OPFailure (PredicateFailure (OPS))
  deriving stock (Eq, Show)

instance STS (CHAIN) where
  type Environment (CHAIN) = ()
  type State (CHAIN) = BT.ChainState Era_0
  type Signal (CHAIN) = BT.Block Era_0

  type
    PredicateFailure (CHAIN) =
      CHAINPredicateFailure

  initialRules = []

  transitionRules = []
