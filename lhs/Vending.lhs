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

\usepackage{float}
\floatstyle{boxed}
\restylefloat{figure}

\newcommand\named[1]{\mathsf{#1}}
\renewcommand\Varid[1]{\mathit{#1}}
\DeclareOldFontCommand{\tt}{\normalfont\ttfamily}{\mathtt}

%format lhs2TeX = "\text{\textrm{lhs}\textsf{2}\TeX}"

%if style /= newcode
%format Relation4 (a) (b) (c) (d) = Set (a `Pair` b `Pair` c `Pair` d)
%format data =
%format DERIVING =
%format GUARDED =
%format CHECK (check) (error) =  check
%format   AND (check) (error) =  check
%format RULE (env) (state1) (tx) (state2) = env "\vdash" state1 "\trans{vend}{" tx "}" state2
%format @?= = "\stackrel{?}{=}"
%format Right = "\text{Valid Transition: }"
%format Left = "\text{Error: }"
%else
%format Relation4 (a) (b) (c) (d) = a -> b -> c -> d -> Bool
%format DERIVING = "deriving (Show, Eq, Ord)"
%format GUARDED = "guarded :: Bool; guarded = "
%format CHECK (check) (error) =  "v  =   runCheck (" check ") " error
%format   AND (check) (error) =  "   <>  runCheck (" check ") " error
%format WITHGUARDED = " && guarded"
%format RULE (env) (state1) (tx) (state2) = "case v of; Invalid errors -> Left errors;  Valid -> Right $" state2 
%endif

%if style == newcode

> module LedgerSpec where
>
> import Test.Tasty
> import Test.Tasty.HUnit
>

%endif

%if style /= newcode
%format runCheck = "\named{runCheck}"
%format valid = "\named{valid}"
%format judge = "\named{judge}"
%format predA = "\named{predA}"
%format predB = "\named{predB}"
%format judgementEx = "\named{judgementEx}"

%format MkEnv (power) (cost) = "\left(\begin{array}{c}" power "\cr " cost "\end{array}\right)"
%format MkSt (tokens) (sodas) = "\left(\begin{array}{c}" tokens "\cr " sodas "\end{array}\right)"
%endif

\begin{document}


%if style == newcode
> data Validity e = Valid | Invalid [e] DERIVING
>
> instance Semigroup (Validity e) where
>   Valid <> b                 = b
>   a <> Valid                 = a
>   (Invalid a) <> (Invalid b) = Invalid (a ++ b)
>
> instance Monoid (Validity e) where
>   mempty = Valid
>   mappend = (<>)
>
> runCheck :: Bool -> e -> Validity e
> runCheck b err = if b then Valid else Invalid [err]

%endif

\section{Simple Vending Machine}

This is a simple example of using the small step sematics with lhs2TeX.

Described here is a vending machine environment, state, signals, and errors.
The environment contains the cost (in tokens) of a soda, and
whether or not the machine is powered on.
The state of the vending machine holds the number unspent tokens
and the number of remaining sodas.
There are two signals, pushing the vending button or depositing some
number of tokens.

> data Env = MkEnv {power :: Bool, cost :: Int} DERIVING
> data St = MkSt {tokens :: Int, sodas :: Int} DERIVING
> data Sig = Push | Deposit Int DERIVING
> data Error = SmallDeposit | OutOfSoda | OutOfOrder DERIVING

\clearpage

\begin{figure}
\mathhs

%if style == newcode

> vend :: Env -> St -> Sig -> Either [Error] St
> vend env st (Deposit t) =
>   let

%endif

\begin{equation}\label{eq:vending-rule-deposit}
\inference[vending-rule-deposit]
{%

>     CHECK (power env) OutOfOrder

}{%
%if style == newcode

>   in

%endif

>     RULE (MkEnv power cost) (MkSt tokens sodas) (Deposit t) (MkSt (tokens st + t) (sodas st))

}
\end{equation}

\nextdef

%if style == newcode

> vend env st Push =
>   let

%endif

\begin{equation}\label{eq:vending-rule-push}
\inference[vending-rule-push]
{%

>     CHECK (power env) OutOfOrder
>     AND   (tokens st >= cost env) SmallDeposit
>     AND   (sodas st /= 0) OutOfSoda
>
>     tokens'  =  tokens st - cost env
>     sodas'   =  sodas st - 1

}{%
%if style == newcode

>   in

%endif

>     RULE (MkEnv power cost) (MkSt tokens sodas) Push (MkSt (tokens') (sodas'))

}
\end{equation}

\caption{vending inference rules}
\label{fig:rules:vending}
\end{figure}

\section{Tests}

> testDeposit :: Assertion
> testDeposit = vend (MkEnv True 1) (MkSt 0 1) (Deposit 2) @?= Right (MkSt 2 1)

> testGetSoda :: Assertion
> testGetSoda = vend (MkEnv True 1) (MkSt 1 1) Push @?= Right (MkSt 0 0)

> testSmallDep :: Assertion
> testSmallDep = vend (MkEnv True 1) (MkSt 0 1) Push @?= Left [SmallDeposit]

> testOutOfSoda :: Assertion
> testOutOfSoda = vend (MkEnv True 1) (MkSt 1 0) Push @?= Left [OutOfSoda]

> testSmallNoSoda :: Assertion
> testSmallNoSoda = vend (MkEnv True 1) (MkSt 0 0) Push @?= Left [SmallDeposit, OutOfSoda]

%if style == newcode

> unitTests :: TestTree
> unitTests = testGroup "Unit Tests"
>   [ testCase "test deposit" testDeposit
>   , testCase "test get soda" testGetSoda
>   , testCase "test deposit too small" testSmallDep
>   , testCase "test out of soda" testOutOfSoda
>   , testCase "test deposit too small and out of soda" testSmallNoSoda
>   ]

> tests :: TestTree
> tests = testGroup "Inference Rules Tests" [unitTests]

%endif

\end{document}
