\documentclass[11pt,a4paper]{article}
%include polycode.fmt

\usepackage{xspace}

\newcommand{\lovelace}{\ensuremath{\mathsf{lovelace}}\xspace}
\newcommand{\ada}{\ensuremath{\mathsf{ada}}\xspace}

\begin{document}

\title{Accounting in Cardano}
\maketitle

\begin{abstract}
A description of the accounting model for Cardano, whereby every
one of the 45 billion \ada, or 45 quadrillion \lovelace,
is accounted for at all times.
\end{abstract}

\tableofcontents

\section{Introduction}

At any given moment in time (i.e. slot), every \lovelace will be
accounted for as a part of one of the following categories:
\begin{itemize}
  \item Circulation (UTxO)
  \item Deposits
  \item Fees
  \item Reserves (monetary expansion)
  \item Rewards (account addresses)
  \item Reward Pool (undistributed)
  \item Treasury
\end{itemize}

\section{Code}

%if style == code
\begin{code}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}

module Accounting where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid (Sum(..))
import Lens.Micro ((^.), (&), (.~), (+~), (-~))
import Lens.Micro.TH (makeLenses)
import Numeric.Natural (Natural)

\end{code}
%endif

\subsection{Basic Types}

First we define some basic types, which are just wrappers around $\mathbb{N}$.

\begin{code}
newtype Slot = Slot Natural
\end{code}
%if style == code
\begin{code}
  deriving (Eq, Ord, Show, Num, Integral, Real, Enum)
  deriving (Semigroup, Monoid) via (Sum Natural)

\end{code}
%endif
\begin{code}
newtype Duration = Duration Natural
\end{code}
%if style == code
\begin{code}
  deriving (Eq, Ord, Show, Num, Integral, Real, Enum)
  deriving (Semigroup, Monoid) via (Sum Natural)

\end{code}
%endif
\begin{code}
newtype Coin = Coin Natural
\end{code}
%if style == code
\begin{code}
  deriving (Eq, Ord, Show, Num, Integral, Real, Enum)
  deriving (Semigroup, Monoid) via (Sum Natural)

\end{code}
%endif

A duration is the difference between two slots:
\begin{code}
(-*) :: Slot -> Slot -> Duration
(Slot s) -* (Slot t) = Duration (s - t)

(+*) :: Slot -> Duration -> Slot
(Slot s) +* (Duration d) = Slot (s + d)

\end{code}

There are two types of certificates which create
resources on the ledger, key registration certificates
and pool registration certificates.
Each of these has a corresponding certificate
to remove the resource.
Key deregistration happens as soon as the ledger
proccesses the certificate, but pool retirement
happens after a specified duration.
We give the certificates an ID in order to distinguish them.

\begin{code}
type CertID = Natural

data Cert = KeyReg CertID
          | KeyDeReg CertID
          | PoolReg CertID
          | PoolRetire CertID Duration
\end{code}
%if style == code
\begin{code}
  deriving (Show, Eq, Ord)
\end{code}
%endif

The state transitions depend on some protocol constants.
The two certificate deposit amounts are given by
$\mathsf{\_keyRegDep}$ and $\mathsf{\_poolRegDep}$.
The minimum portion of the deposit that will always
be returned is given by $\mathsf{\_minDep}$.
The decay constant is given by $\mathsf{\_decayRate}$.
The portion of rewards that go to the treasury is $\mathsf{\_tau}$,
and the expansion proportion is given by $\mathsf{\_rho}$.


\begin{code}

data PrtclConsts =
  PrtclConsts
  { _keyRegDep :: Coin
  , _poolRegDep :: Coin
  , _minDep :: Float
  , _decayRate :: Float
  , _tau :: Float
  , _rho :: Float }
\end{code}
%if style == code
\begin{code}
  deriving (Show, Eq)

\end{code}
%endif
The ledger state is defined as:
\begin{code}
data LedgerState =
  LedgerState
  { _circulation :: Coin
  , _deposits :: Coin
  , _treasury :: Coin
  , _reserves :: Coin
  , _rewards :: Coin
  , _rewardPool :: Coin
  , _fees :: Coin
  , _obligations :: Map Cert Slot
  , _retiring :: Map CertID Slot
  , _pconsts :: PrtclConsts
  , _slot :: Slot
  , _lastEpoch :: Slot }
\end{code}
%if style == code
\begin{code}
  deriving (Show, Eq)

makeLenses ''PrtclConsts
makeLenses ''LedgerState

\end{code}
%endif

The $\mathsf{\_lastEpoch}$ variable is used only
to track when decayed portions of a deposit
are moved from the deposit pool to the reward pool,
but for the purposes of this model we do not need to
assume a certain number of slots per epoch, etc.

\subsection{Note on lens notation}

We use the lens operators for getting and setting items
in the ledger state and protocol constants.
For example, to get the $\mathsf{\_minDep}$ from
a protocol constants record, we write

< pc ^. minDep

And to get the $\mathsf{\_minDep}$ from a ledger state
record, we write

< ls ^. pconsts . minDep

To set an element of a record, we write

< ls & slot .~ Slot 42

And to set multiple elements we write

< ls & deposits .~ Coin 7
<    & slot .~ Slot 42

Finally, to increment or decrement a field, we write

< ls & deposits +~ Coin 7
<    & slot -~ Slot 42

\subsection{Methods}

The total amount of \lovelace in a given ledger state is
given by the following calculation. It should always be
equal to forty-five quadrillion \lovelace.
\begin{code}
total :: LedgerState -> Coin
total ls = ls^.circulation + ls^.deposits + ls^.treasury
            + ls^.reserves + ls^.rewards + ls^.rewardPool + ls^.fees

\end{code}

The deposit amounts are determined by the protocol constants.
\begin{code}
deposit :: Cert -> PrtclConsts -> Coin
deposit (KeyReg _) = _keyRegDep
deposit (PoolReg _) = _poolRegDep
deposit (KeyDeReg _) = const $ Coin 0
deposit (PoolRetire _ _) = const $ Coin 0

\end{code}

The refund of a certificate is determined by an exponential decay.
\begin{code}
refund :: Cert -> PrtclConsts -> Duration -> Coin
refund cert pc dur = floor (dep * (dmin + (1-dmin) * exp pow))
  where
    dep = fromIntegral $ deposit cert pc
    dmin = pc^.minDep
    pow = - pc^.decayRate * fromIntegral dur

\end{code}

For a given ledger state, we can calculate the total amount
of \lovelace needed to refund everyone at the current slot.
\begin{code}
obligation :: LedgerState -> Coin
obligation ls = sum
  [refund cert (ls^.pconsts) ((ls^.slot) -* start)
    | (cert, start) <- Map.toList (ls^.obligations)]

\end{code}

\subsection{Actions}

There are seven types of actions that cause \lovelace
to move between the seven categories.

\begin{code}
data Action = ActTxBody Coin
            | ActCert Cert
            | ActWithdrawal Coin
            | ActEpochNoVote Float
            | ActEpochWithVote PrtclConsts Float
            | ActNextSlot
\end{code}
%if style == code
\begin{code}
            deriving (Show, Eq)

\end{code}
%endif

We will describe how each of these actions changes the ledger state.
First we define a few methods which will be used by the actions.

Both key registration and pool registration certificates will be
added to the ledger state with the following method:

\begin{code}
addCert :: LedgerState -> Cert -> LedgerState
addCert ls cert =
  case Map.lookup cert (ls^.obligations) of
    (Just _) -> ls -- invalid transition, cert already registered
    Nothing ->
      ls & deposits +~ d
         & circulation -~ d
         & obligations .~ Map.insert cert (ls^.slot) (ls^.obligations)
      where d = deposit cert (ls^.pconsts)

\end{code}

We can split the amount of a deposit still remaning in the deposit
pool into the amount that is left for a refund and the amount
that has decayed.

\begin{code}
refundPartition :: Cert -> Slot -> LedgerState -> (Coin, Coin)
refundPartition cert s ls = (refundNow, decayed)
  where
    lastSettlement = max (ls^.lastEpoch) s
    refundNow = refund cert (ls^.pconsts) ((ls^.slot) -* s)
    refundAtSettlement = refund cert (ls^.pconsts) (lastSettlement -* s)
    decayed = refundAtSettlement - refundNow

\end{code}

Pools are only retired at epoch boundaries.
When a retirement certificate is posted to the ledger,
the state records the upcomming retiriment in $\mathsf{\_retiring}$.
During the epoch boundary, the pools in this map which
are ready to retire are refunded and removed with:

\begin{code}
retirePool :: LedgerState -> CertID -> LedgerState
retirePool ls p =
  case Map.lookup (PoolReg p) (ls^.obligations) of
    Nothing -> ls -- invalid transition, unknown cert
    (Just s) ->
      ls & deposits -~ (refundNow + decayed)
         & rewards +~ refundNow
         & fees +~ decayed
         & obligations .~ Map.delete cert (ls^.obligations)
         & retiring .~ Map.delete p (ls^.retiring)
      where
        cert = PoolReg p
        (refundNow, decayed) = refundPartition cert s ls

\end{code}

We now define the ledger actions.
\begin{code}
applyAction :: LedgerState -> Action -> LedgerState

\end{code}

The first actions is a UTxO transaction, which
moves \lovelace out of circulation into the fee pool.
\begin{code}
applyAction ls (ActTxBody fee) =
  if fee > ls^.circulation
  then
    ls -- invalid transition, fee too high
  else
    ls & circulation -~ fee
       & fees +~ fee

\end{code}

New certificates can be registered, which removes \lovelace
from circulation to the deposit pool.

\begin{code}
applyAction ls (ActCert cert@(KeyReg _)) = addCert ls cert

applyAction ls (ActCert cert@(PoolReg _)) = addCert ls cert

\end{code}

Existing certificates can be removed, which removes \lovelace
from the deposit pool. The amount removed is equal
to the total amount of of the deposit which had not
decayed by the previous epoch. The portion of this which
has since decayed this epoch is given to the fee pool,
and all else is moved to cirtulation.

\begin{code}
applyAction ls (ActCert (KeyDeReg i)) =
  case Map.lookup (KeyReg i) (ls^.obligations) of
    Nothing -> ls -- invalid transition, unknown cert
    (Just s) ->
      ls & deposits -~ (refundNow + decayed)
         & circulation +~ refundNow
         & fees +~ decayed
         & obligations .~ Map.delete cert (ls^.obligations)
      where
        cert = KeyReg i
        (refundNow, decayed) = refundPartition cert s ls

applyAction ls (ActCert (PoolRetire i d)) =
  case Map.lookup (PoolReg i) (ls^.obligations) of
    Nothing -> ls -- invalid transition, unknown pool
    (Just _) -> ls & retiring .~ Map.insert i ((ls^.slot) +* d) (ls ^. retiring)

\end{code}

Distributed rewards can be moved back into circulation.

\begin{code}
applyAction ls (ActWithdrawal amt) =
  if ls^.rewards < amt
  then
    ls -- invalid transition, not enough rewards
  else
    ls & circulation +~ amt
       & rewards -~ amt

\end{code}

At epoch boundaries a lot happens.
First we consider the case where no vote happens.
The deposit pool is set to the current refund obligations.
This number always results in a smaller deposit pool, and the
difference is given to the total pool from which rewards
will be calculated for this epoch.
The total pool is a combination of this difference,
the fees, the current reward pool, and the monetary expansion.
Some portion of this total pool goes to the treasury, as
given by the protocol constant $\tau$.
What is left represents the \lovelace available for
the leader election rewards. In the real implementation,
this is determined by a lot of factors, but here we
can simplify the situation by having the epoch action simply
pass in a proportion which is realized.
Finally, the reward pool is set to the amount of \lovelace
that was available for rewards but was not realized,
the fee pool is set to zero, and the last epoch is set
to the current slot. The real implementation has epochs
at regular intervals, but here we are only interested
in preserving ada and our rules are more general.

\begin{code}
applyAction ls (ActEpochNoVote realized) =
  foldl retirePool ls' retiringIds
  where
    ls' = ls & deposits .~ oblg
              & treasury +~ newTreasury
              & reserves -~ expansion
              & rewards +~ paidRewards
              & rewardPool .~ availablePool - paidRewards
              & fees .~ Coin 0
              & lastEpoch .~ ls^.slot
    oblg = obligation ls
    decayed = ls^.deposits - oblg
    expansion = floor $ ls^.pconsts.rho * fromIntegral (ls^.reserves)
    totalPool = ls^.fees + decayed + ls^.rewardPool + expansion
    newTreasury = floor $ ls^.pconsts.tau * fromIntegral totalPool
    availablePool = totalPool - newTreasury
    paidRewards = floor $ realized * fromIntegral availablePool
    retiringIds = Map.keys $ Map.filter (>= ls^.slot) (ls^.retiring)

\end{code}

Now we consider an epoch boundary change where the protocol
constants have changed due to a vote.
The ledger state is first updated according to the `ActEpochNoVote` action.
Next, when the protocol consts change, we set the deposit pool to the
current refund obligations, and make up the difference with the reserves.

\begin{code}
applyAction ls (ActEpochWithVote pc realized)
  -- lower obligation, increase reserves
  | newOblg < epochLs ^. deposits =
    lsWithNewPC & deposits .~ newOblg
                & reserves +~ (epochLs ^. deposits - newOblg)

  -- invalid transition, not enough reserves
  | epochLs ^. reserves < (newOblg - epochLs ^. deposits) = epochLs

  -- higher obligation, decrease reserves
  | otherwise = lsWithNewPC & deposits .~ newOblg
                            & reserves -~ (newOblg - epochLs ^. deposits)
  where
    epochLs = applyAction ls (ActEpochNoVote realized)
    lsWithNewPC = epochLs & pconsts .~ pc
    newOblg = obligation lsWithNewPC

\end{code}

Lastly, there is an action which increases the slot number.

\begin{code}
applyAction ls ActNextSlot =
  ls & slot +~ 1

\end{code}

\end{document}
