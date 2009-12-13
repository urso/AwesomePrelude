First, we will start of with the module header and some imports.

> module Compiler.LambdaLifting where

> import Compiler.Raw
> import qualified Data.Set as S
> import qualified Data.Map as M
> import Control.Monad.State
> import Data.Traversable hiding (mapM)
> import Control.Monad.Writer

Lambda-lifting is done by doing three steps, as defined in 
<a href="http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.30.1125">A modular fully-lazy lambda lifter in Haskell</a>

Lambda-lifting gives us a list of definitions. The |Fix Val| datatype doesn't contain any |Abs| terms.

> lambdaLift :: Fix Val -> [(Name, [Name], Fix Val)] 
> lambdaLift = undefined -- collectSCs . abstract . freeVars

The |freeVars| function will annotate every expression with its variables. The type of such an annotated expression is:

> newtype AnnExpr a = AnnExpr {unAnn :: (a, Val (AnnExpr a))} deriving Show
> type FreeVarExpr  = AnnExpr FreeVars
> type FreeVars     = S.Set Name
> type Name         = String

|freeVars| operates on simple fixpoints of |Val|:

> freeVars :: Fix Val -> AnnExpr (S.Set String)
> freeVars = freeVars' . out

|freeVars'| does the heavy lifting:

> freeVars' :: Val (Fix Val) -> AnnExpr (S.Set String)
> freeVars' (App l r)      =  let l' = freeVars l
>                                 r' = freeVars r
>                             in  ae (S.union (fv l') (fv r')) (App l' r')
> freeVars' (Prim s)       =  ae S.empty (Prim s)
> freeVars' (Lam x expr)   =  let expr' = freeVars expr
>                             in  ae (S.difference (fv expr') (S.fromList x)) (Lam x expr')
> freeVars' (Var v)        =  ae (S.singleton v) (Var v)
> freeVars' (Name nm expr) =  mapVal (Name nm) (freeVars expr)

> ae a b = AnnExpr (a,b)
> fv = fst . unAnn
> e = snd . unAnn

> mapVal :: (AnnExpr t -> Val (AnnExpr t)) -> AnnExpr t -> AnnExpr t
> mapVal f (AnnExpr (a, e)) = ae a (f (AnnExpr (a, e)))

TODO: this can probably be easier.

The function |abstract| changes every lambda expression |e| by adding
abstractions for all free variables in |e| (and applying them)

> abstract :: AnnExpr (S.Set String) -> Fix Val
> abstract = f
>  where
>   f (AnnExpr (a, (App l r)))     = app (abstract l) (abstract r)
>   f (AnnExpr (a, (Prim s)))      = prim s
>   f (AnnExpr (a, (Lam x expr)))  = let freeVars = S.toList a
>                                    in  addVars (In $ Lam (freeVars ++ x) (abstract expr)) freeVars
>   f (AnnExpr (a, (Var v)))       = var v
>   f (AnnExpr (a, (Name x expr))) = name x (abstract expr)
>

> addVars :: Fix Val -> [String] -> Fix Val
> addVars expr = foldl addVar expr
> addVar expr = app expr . var

> data CollectState = CollectState 
>   { freshVariable :: Int
>   , bindings :: [(String, Fix Val)]
> }

> collectSCs e = let (e', st) = runState (collectSCs' e) (CollectState 0 [])
>                in  ("main", e'):(bindings st)

> collectSCs' :: Val (Fix Val) -> State CollectState (Fix Val)
> collectSCs' (App l r)      = do l' <- collectSCs' (out l)
>                                 r' <- collectSCs' (out r)
>                                 return (app l' r')
> collectSCs' (Prim s)       = return (prim s)
> collectSCs' (Lam x expr)   = do expr' <- collectSCs' (out expr)
>                                 nm <- freshName
>                                 write nm (In $ Lam x expr')
>                                 return $ In $ Var nm
> collectSCs' (Var v)        = return $ In (Var v)
> collectSCs' (Name nm expr) = do expr' <- collectSCs' (out expr)
>                                 return $ In $ Name nm expr'

> write :: String -> Fix Val -> State CollectState ()
> write nm expr = modify (\st -> st {bindings = (nm,expr):(bindings st)})

> freshName :: State CollectState String
> freshName = do st <- get
>                put (st {freshVariable = freshVariable st + 1})
>                return $ "__super__" ++ (show $ freshVariable st)

This is some testing code:

> app l r      = In (App l r)
> prim         = In . Prim
> lam l r      = In (Lam [l] r)
> var          = In . Var
> name nm expr = In (Name nm expr)

> example = lam "x" (app (lam "y" (var "x")) (var "x"))
