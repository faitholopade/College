\chapter{Matching}
\input{Copyright}
\begin{code}
module Matching
( Binding
, eMatch
, buildReplacement
)
where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

import AST
import Utilities

import Debug.Trace
dbg msg x = trace (msg ++ show x) x
pdbg what x = dbg (what ++ ":\n") x
mdbg msg x = return $! dbg msg x
\end{code}

\newpage
\section{Bindings}

We bind (variable) names to expressions:
\begin{code}
type Binding = Map String Expr
\end{code}

Our standard lookup is total, taking a name
and searching for it and its associated expression.
If not found, a variable with that name is returned.
\begin{code}
bGet :: Binding -> String -> Expr
bGet bind nm
  = case M.lookup nm bind of
      Nothing  ->  mkName nm
      Just e   ->  e
\end{code}

Update is partial, as redefinition of a name already present
is not allowed:
\begin{code}
bSet :: MonadFail m => String -> Expr -> Binding -> m Binding
bSet nm e bind
  = case M.lookup nm bind of
      Nothing            ->  return $ M.insert nm e bind
      Just e0 | e0 == e  ->  return bind
      _                  ->  fail "conflicting defs."
\end{code}

\section{Known Names}

Some names only match themselves.
Some we hardwire, as (re-)defining them in imported Haskell files
is impossible/awkward:
\begin{code}
hardPrelude :: Set String
hardPrelude = S.fromList
  [ "even","otherwise","ord","minimum","maximum"
  , "+","-","*","/","div","mod"
  , ">",">=","<","<="
  , "==","&&","||"
  , "++","[]",":"
  ]
\end{code}

We may want to extend these with more Prelude names!

\newpage
\section{Matching}

Matching takes candidate and pattern expressions,
along with a list of names defined in the context where the pattern is given,
and establishes if the candidate is an instance of the pattern.
If so, it returns a binding that maps the pattern to the candidate.
\begin{code}
eMatch :: MonadFail m => [String] -> Expr -> Expr -> m Binding
eMatch knwnNms cand patn
 = mExpr (S.fromList knwnNms `S.union` hardPrelude) M.empty cand patn
\end{code}

Here we have an input binding, initially empty,
that grows as matching proceeds.
\begin{code}
mExpr  :: MonadFail m => Set String -> Binding -> Expr -> Expr -> m Binding
\end{code}

Literals only match themselves
\begin{code}
mExpr known bind (LBool c) (LBool  p)
  =  if  c == p  then  return bind  else  fail "diff. bool"
mExpr known bind (LInt  c) (LInt   p)
  =  if  c == p  then  return bind  else  fail "diff. int"
mExpr known bind (LChar c) (LChar  p)
  =  if  c == p  then  return bind  else  fail "diff. char"
\end{code}

Constructors are always considered ``known'',
so only match themselves:
\begin{code}
mExpr known bind (Cons c) (Cons p)
  =  if c == p  then return bind  else  fail "diff. cons"
\end{code}

Variables match themselves, of course,
but will also match anything if not ``known'':
\begin{code}
mExpr known bind (Var c) (Var p)
  | c == p              =  return bind
mExpr known bind cand (Var p)
  | p == "_"            =  return bind
  | p `S.member` known  =  fail "not self."
  | otherwise           =  bSet p cand bind
\end{code}

Applications match if both function and argument expressions do:
\begin{code}
mExpr known bind0 (App c1 c2) (App p1 p2)
  = do { bind1 <- mExpr known bind0 c1 p1
       ;          mExpr known bind1 c2 p2 }
\end{code}

If-then-else match if condition-, then- and else-expressions do.:
\begin{code}
mExpr known bind0 (If c1 c2 c3) (If p1 p2 p3)
  = do { bind1 <- mExpr known bind0 c1 p1
       ; bind2 <- mExpr known bind1 c2 p2
       ;          mExpr known bind2 c3 p3 }
\end{code}

\begin{code}
mExpr known bind cand patn = fail "no match found"
\end{code}

\newpage
\section{Building}

For now, ignore local declarations
\begin{code}
buildReplacement :: Binding -> [Decl] -> Expr -> Expr
buildReplacement bind _   b@(LBool _) = b
buildReplacement bind _   i@(LInt  _) = i
buildReplacement bind _   c@(LChar _) = c
buildReplacement bind _     (Var n)   = bGet bind n
buildReplacement bind _     (Cons n)  = bGet bind n
buildReplacement bind ldcls (App e1 e2)
  = App (buildReplacement bind ldcls e1)
        (buildReplacement bind ldcls e2)
buildReplacement bind ldcls (If e1 e2 e3)
  = If (buildReplacement bind ldcls e1)
       (buildReplacement bind ldcls e2)
       (buildReplacement bind ldcls e3)
buildReplacement bind ldcls (Case e alts)
  = Case (buildReplacement bind ldcls e)
         $ map replaceAlt alts 
  where replaceAlt = buildAltReplacement bind ldcls 
buildReplacement bind ldcls e
  = Var $ unlines
     [ "buildReplacement n.y.f.i."
     , "bind: "++show bind
     , "ldcls: "++show ldcls
     , "e: "++show e
     ]

buildAltReplacement bind ldcls (Alt e grds decls)
  = Alt (buildReplacement bind ldcls e)
        (buildGrdReplacement bind ldcls grds)
        decls

buildGrdReplacement bind ldcls (NoGrd e)
  =  NoGrd $ buildReplacement bind ldcls e
buildGrdReplacement bind ldcls (GrdExpr epairs) 
  = GrdExpr $ mappair (build,build) epairs
  where 
    build = buildReplacement bind ldcls
\end{code}
