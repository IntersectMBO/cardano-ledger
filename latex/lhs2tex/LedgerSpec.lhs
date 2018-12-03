\documentclass[a4paper,11pt]{scrartcl}

%include polycode.fmt

% The following is the general setup of the machinery. As this
% gets longer, it's probably useful to extract this into its own
% fmt file.
%
% There are some helpful TeX macros, but primarily we do formatting
% tricks via formatting directives. Several of these have different
% expansions depending on whether we extract code (in "newcode" mode)
% or TeX (in "poly" mode).

\newcommand\named[1]{\mathsf{#1}}
\renewcommand\Conid[1]{\mathsf{#1}}
\renewcommand\Varid[1]{\mathit{#1}}

%if style /= newcode
%format :: = "\in"
%format Pair (a) (b) = a "\times " b
%format Set = "\mathbb{P}"
%format `from` = "\in"
%format |-> = "\mapsto"
%format SET (x) (y) = "\left\{" x "\middle|" y "\right\}"
%format SUM (x) (y) = "\sum\limits_{" x "}" y
%format VSTACK (x) (y) = "\begin{array}{l}" x "\cr " y "\end{array}"
%format LET =
%else
%format SET (x) (y) = "Data.Map.fromList [ " x " | " y "]"
%format SUM (x) (y) = "sum [ " y " | " x "]"
%format `from` = " <- Data.Map.toList $ "
%format |-> = "`Pair`"
%format VSTACK (x) (y) = x ", " y
%format LET = "let"

%endif

% The following is the module header and some basic definitions
% needed for the demo, to be included only in the code, but not
% currently in the LaTeX output.
%
% Very few of the definitions are also added to help the formatting,
% such as the Pair type and pattern synonym.

%if style == newcode

> {-# LANGUAGE PatternSynonyms #-}
> {-# LANGUAGE StandaloneDeriving #-}
> {-# OPTIONS_GHC -Wno-missing-methods #-}
> module LedgerSpec where
>
> import Data.Map
> import Data.Set
>
> data TxId
> instance Eq TxId
> instance Ord TxId
> data Ix
> instance Eq Ix
> instance Ord Ix
> data Addr
> data Coin
> instance Num Coin

> type TxIn = Pair TxId Ix
> type TxOut = Pair Addr Coin
> type UTxO = Map TxIn TxOut
> data Tx
>
> txbody :: Tx -> Pair (Set TxIn) (Map Ix TxOut)
> txbody = undefined
>
> txid :: Tx -> TxId
> txid = undefined
>
> type Pair a b = (a, b)
> pattern Pair x y = (x, y)

%endif

% The following are some more specific formatting directives.
% The spec code currently formats top-level functions differently
% from local names / arguments, so we have formatting directives
% for each of these top-level functions, which only apply when
% producing LaTeX output.

%if style /= newcode
%format txins = "\named{txins}"
%format txouts = "\named{txouts}"
%format balance = "\named{balance}"
%format txbody = "\named{txbody}"
%endif

\begin{document}

Functions used in UTxO rules:

> txins :: Tx -> Set TxIn
> txins tx = inputs where (inputs, _) = txbody tx

> txouts :: Tx -> UTxO
> txouts tx = SET ((txid tx, ix) |-> txout) (VSTACK (LET (_, outputs) = txbody tx) (ix |-> txout `from` outputs))

> balance :: UTxO -> Coin
> balance utxo = SUM ((_ |-> (_, c)) `from` utxo) c

\end{document}
