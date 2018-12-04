\documentclass[a4paper,11pt]{scrartcl}

%include polycode.fmt
%include spacing.fmt
%if style /= newcode
%subst newline = "\nextline'n"
%subst blankline = "\nextline[1ex]'n"
%endif

\usepackage{iohk}
\usepackage{mathpazo}
\usepackage{semantic}

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
%format List a = a "^{*}"
%format Pair (a) (b) = a "\times " b
%format Set = "\mathbb{P}"
%format `from` = "\in"
%format `Set.from` = "\in"
%format `List.from` = "\in"
%format (Set.singleton (x)) = "\{" x "\}"
%format `Map.union` = "\cup "
%format `Set.union` = "\cup "
%format Map.! = ^^
%format Set.empty = "\emptyset"
%format keysSet = "\dom "
%format `isSubsetOf` = "\subseteq "
%format |-> = "\mapsto"
%format /= = "\neq "
%format == = "="
%format `implies` = "\Rightarrow "
%format restrictDom (x) (y) = x "\vartriangleleft " y
%format subtractDom (x) (y) = x "\ntriangleleft " y
%format SET (x) (y) = "\left\{" x "\middle|" y "\right\}"
%format SET' (x) (y) = "\left\{" x "\middle|" y "\right\}"
%format SUM (x) (y) = "\sum\limits_{" x "}" y
%format ALL (x) (y) = "\forall " x ", " y
%format VSTACK (x) (y) = "\begin{array}{l}" x "\cr " y "\end{array}"
%format LET x = x
%format Relation3 (a) (b) (c) = Set (a `Pair` b `Pair` c)
%format Relation4 (a) (b) (c) (d) = Set (a `Pair` b `Pair` c `Pair` d)
%format (serialised (a)) = "\llbracket" a "\rrbracket"
%format GUARDED =
%format AND     =
%format WITHGUARDED =
%else
%format SET (x) (y) = "Map.fromList [ " x " | " y "]"
%format SET' (x) (y) = "Set.fromList [ " x " | " y "]"
%format SUM (x) (y) = "sum [ " y " | " x "]"
%format ALL (x) (y) = "and [ " y " | " x "]"
%format dcerts = "dcerts "
%format `from` = " <- Map.toList $ "
%format `Set.from` = " <- Set.toList $ "
%format `List.from` = " <- "
%format |-> = "`Pair`"
%format VSTACK (x) (y) = x ", " y
%format LET x = "let " x
%format Relation3 (a) (b) (c) = a -> b -> c -> Bool
%format Relation4 (a) (b) (c) (d) = a -> b -> c -> d -> Bool
%format GUARDED = "guarded :: Bool; guarded = "
%format AND     = " && "
%format WITHGUARDED = " && guarded"
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
> import Data.Map as Map
> import Data.Set as Set
>
> type List a = [a]
> type Pair a b = (a, b)
> pattern Pair x y = (x, y)

> implies :: Bool -> Bool -> Bool
> x `implies` y = not x || y
> infixr 1 `implies`

> data TxId
> instance Eq TxId
> instance Ord TxId

> data Ix
> instance Eq Ix
> instance Ord Ix

> data Addr

> data Coin
> instance Eq Coin
> instance Ord Coin
> instance Num Coin

> type TxIn = Pair TxId Ix
> type TxOut = Pair Addr Coin
> type UTxO = Map TxIn TxOut
> data Tx
>
> data Slot
> instance Eq Slot
> instance Ord Slot

> data Epoch

> data PrtclConsts

> data Allocs

> type Wdrl = Set (Pair TxId AddrRwd) -- deviates from the spec

> data UTxOEnv =
>   MkUTxOEnv Slot PrtclConsts Allocs Allocs

> data UTxOState =
>   MkUTxOState UTxO Coin Coin Wdrl

> data DWEnv =
>   MkDWEnv Tx Slot

> data DWState =
>   MkDWState DState PState

> data DState
> data PState

> data DCert
> instance Eq DCert
> instance Ord DCert

> data VKey
> data Sig
> data Data
> data DCertBody

> class Serialisable a
> instance Serialisable DCertBody
> instance Serialisable Tx
> instance Serialisable TxId
> instance Serialisable Ix
> instance Serialisable Addr
> instance Serialisable Coin
> instance (Serialisable a, Serialisable b) => Serialisable (a, b)
> instance Serialisable a => Serialisable (Set a)
> instance (Serialisable k, Serialisable v) => Serialisable (Map k v)

> serialised :: Serialisable a => a -> Data
> serialised = undefined

> txbody :: Tx -> Pair (Set TxIn) (Map Ix TxOut)
> txbody = undefined
>
> txid :: Tx -> TxId
> txid = undefined
>
> epochToSlot :: Epoch -> Slot
> epochToSlot = undefined

> restrictDom :: Set k -> Map k v -> Map k v
> restrictDom = undefined

> subtractDom :: Set k -> Map k v -> Map k v
> subtractDom = undefined

> utxoRel :: Relation4 UTxOEnv UTxOState Tx UTxOState
> utxoRel = undefined

> dcerts :: Tx -> List DCert
> dcerts = undefined

> dCertRetirePool :: [DCert]
> dCertRetirePool = undefined

> retire :: DCert -> Epoch -- more permissive than in spec
> retire = undefined

> deleg :: Relation4 Slot DState DCert DState
> deleg = undefined

> delegW :: Relation4 DWEnv DWState DCert DWState
> delegW = undefined

> dbody :: Map DCert DCertBody
> dbody = undefined

> dwit :: Map DCert (Pair VKey Sig)
> dwit = undefined

> verify :: Relation3 VKey Data Sig
> verify = undefined

> deposits :: PrtclConsts -> Allocs -> Tx -> Coin
> deposits = undefined

> keyRefunds :: PrtclConsts -> Allocs -> Tx -> Coin
> keyRefunds = undefined

> decayedTx :: PrtclConsts -> Allocs -> Tx -> Coin
> decayedTx = undefined

> txfee :: Tx -> Coin
> txfee = undefined

> txwdrls :: Tx -> Set AddrRwd
> txwdrls = undefined

> ttl :: Tx -> Slot
> ttl = undefined

> minfee :: PrtclConsts -> Tx -> Coin
> minfee = undefined

> data AddrRwd
> instance Eq AddrRwd
> instance Ord AddrRwd

%endif

% The following are some more specific formatting directives.
% The spec code currently formats top-level functions differently
% from local names / arguments, so we have formatting directives
% for each of these top-level functions, which only apply when
% producing LaTeX output.

%if style /= newcode
%format txins = "\named{txins}"
%format txouts = "\named{txouts}"
%format txfee = "\named{txfee}"
%format txid = "\named{txid}"
%format txwdrls = "\named{txwdrls}"
%format balance = "\named{balance}"
%format txbody = "\named{txbody}"
%format deposits = "\named{deposits}"
%format vardeposits = "\Varid{deposits}"
%format keyRefunds = "\named{keyRefunds}"
%format decayedTx = "\named{decayedTx}"
%format txfee = "\named{txfee}"
%format ttl = "\named{ttl}"
%format minfee = "\named{minfee}"
%format dbody = "\named{dbody}"
%format dwit = "\named{dwit}"
%format verify = "\named{verify}"
%format created = "\named{created}"
%format destroyed = "\named{destroyed}"
%format dcerts = "\named{dcerts}"
%format dCertRetirePool = "\named{DCert_{retirepool}}"
%format vk_s
%format sigma = "\sigma"
%format MkUTxOEnv (slot) (consts) (allocs1) (allocs2) = "\left(\begin{array}{c}" slot "\cr " consts "\cr " allocs1 "\cr " allocs2 "\end{array}\right)"
%format MkUTxOState (utxo) (coin1) (coin2) (wdrl) = "\left(\begin{array}{c}" utxo "\cr " coin1 "\cr " coin2 "\cr " wdrl "\end{array}\right)"
%format utxoRel (env) (state1) (tx) (state2) = env "\vdash" state1 "\trans{utxo}{" tx "}" state2
%format MkDWEnv (tx) (slot) = "\left(\begin{array}{c}" tx "\cr " slot "\end{array}\right)"
%format MkDWState (dstate) (pstate) = "\left(\begin{array}{c}" dstate "\cr " pstate "\end{array}\right)"
%format deleg (env) (state1) (cert) (state2) = env "\vdash" state1 "\trans{deleg}{" cert "}" state2
%format delegW (env) (state1) (cert) (state2) = env "\vdash" state1 "\trans{delegw}{" cert "}" state2
%endif

\begin{document}

Functions used in UTxO rules:

> txins :: Tx -> Set TxIn
> txins tx = inputs where (inputs, _) = txbody tx

> txouts :: Tx -> UTxO
> txouts tx = SET ((txid tx, ix) |-> txout) (VSTACK (LET (_, outputs) = txbody tx) (ix |-> txout `from` outputs))

> balance :: UTxO -> Coin
> balance utxo = SUM ((_ |-> (_, c)) `from` utxo) c

> created :: PrtclConsts -> UTxO -> Allocs -> Tx -> Coin
> created pc utxo stkeys tx =
>   balance (restrictDom (txins tx) utxo) + keyRefunds pc stkeys tx

> destroyed :: PrtclConsts -> Allocs -> Tx -> Coin
> destroyed pc stpools tx =
>   balance (txouts tx) + txfee tx + deposits pc stpools tx

\begin{figure}[p]
\mathhs
%if style == newcode

> delegWitKeys :: Tx -> Slot -> DState -> PState -> DCert -> DState -> PState -> Bool
> delegWitKeys tx slot dstate pstate c dstate' pstate' =
>   let

%endif
\inference[Deleg-wit-keys]
{%

>     GUARDED^  deleg slot dstate c dstate'
>
>       AND^    verify vk_s (serialised ((dbody Map.! c, txbody tx))) sigma
>
>     (vk_s, sigma) = dwit Map.! c
>

}{%
%if style == newcode

>   in

%endif

>     delegW (MkDWEnv tx slot) (MkDWState dstate pstate) c (MkDWState dstate' pstate')

}

\caption{Delegation witnesses inference rules}
\end{figure}

\begin{figure}[p]
\mathhs

%if style == newcode

> utxoInductive :: Slot -> PrtclConsts -> Allocs -> Allocs -> UTxO -> Coin -> Coin -> Wdrl -> Tx -> Bool
> utxoInductive slot pc stkeys stpools utxo vardeposits fees wdrls tx =
>   let

%endif

\begin{equation}\label{eq:utxo-inductive}
\inference[UTxO-inductive]
{%

>     GUARDED^  ttl tx <= slot
>       AND^    txins tx /= Set.empty
>       AND^    minfee pc tx <= txfee tx
>       AND^    txins tx `isSubsetOf` keysSet utxo
>       AND^    ALL (c `List.from` dcerts tx) (c `elem` dCertRetirePool `implies` ttl tx <= epochToSlot (retire c))
>       AND^    created pc utxo stkeys tx == destroyed pc stpools tx
>
>     refunded       =  keyRefunds pc stkeys tx
>     decayed        =  decayedTx pc stkeys tx
>     depositChange  =  (deposits pc stpools tx) - (refunded + decayed)

}{%
%if style == newcode

>   in

%endif

>     utxoRel (MkUTxOEnv slot pc stkeys stpools) (MkUTxOState utxo vardeposits fees wdrls) tx (MkUTxOState ((subtractDom (txins tx) utxo) `Map.union` txouts tx) (vardeposits + depositChange) (fees + (txfee tx) + decayed) (wdrls `Set.union` SET' ((txid tx, a)) (a `Set.from` txwdrls tx))) WITHGUARDED

}
\end{equation}
\caption{UTxO inference rules}
\label{fig:rules:utxo}
\end{figure}

\end{document}
