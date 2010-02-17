{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE EmptyDataDecls, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}

module Lang.JavaScript where

import Prelude ((++))
import qualified Prelude as P

import Generic.Prelude hiding ((++))
import Lang.Value

data JS
type JavaScript a = Val JS a

type Bool = TBool (Val JS)
type Maybe a = TMaybe (Val JS) a
type Either a b = TEither (Val JS) a b

-- * JavaScript instances for AwesomePrelude 'data types'.

instance NameC (Val JS) where
  named s a = s `Name` a

instance FunC (Val JS) where
  type TFun (Val JS) = (->)
  lam f = Lam f
  app f x = App f x

instance RecFunC (Val JS) where
  fix f   = fun1 "fix" (\[v] -> "fix = arguments.callee, " ++ v ++ "(function (i) { return fix(" ++ v ++ ")(i) })") (lam f)

instance BoolC (Val JS) where
  data TBool (Val JS)
  false      = Con "false"
  true       = Con "true"
  bool x y z = fun3 "bool" (\[f, t, b] -> b ++ " ? " ++ t ++ "(/*force*/) : " ++ f ++ "(/*force*/)") (lam (const x)) (lam (const y)) z

instance MaybeC (Val JS) where
  data TMaybe (Val JS) a
  nothing   = Con "{ nothing : 1 }"
  just      = fun1 "just" (\[x] -> "{ just : " ++ x ++ " }")

instance MaybeOp (Val JS) where
  maybe p q = fun3 "maybe" (\[n, j, m] -> m ++ ".nothing ? " ++ n ++ " : " ++ j ++ "(" ++ m ++ ".just)") p (lam q)

instance TupleC (Val JS) where
  data TTuple (Val JS) a b
  mkTuple   = fun2 "mkTuple" (\[a, b] -> "{ fst : " ++ a ++ ", snd : " ++ b ++ "}")
  tuple p q = fun2 "tuple"   (\[f, t] -> f ++ "(" ++ t ++ ".fst, " ++ t ++ ".snd)") (lam2 p) q

instance EitherC (Val JS) where
  data TEither (Val JS) a b
  left       = fun1 "left"   (\[l] -> "{ left  : " ++ l ++ " }")
  right      = fun1 "right"  (\[r] -> "{ right : " ++ r ++ " }")
  either p q = fun3 "either" (\[l, r, e] -> e ++ ".left ? " ++ l ++ "(" ++ e ++ ".left) : " ++ r ++ "(" ++ e ++ ".right)") (lam p) (lam q)

instance ListC (Val JS) where
  nil         = Con "{ nil : 1 }"
  cons        = fun2 "cons" (\[x, xs] -> "{ head : " ++ x ++ ", tail : " ++ xs ++ " }")
  list b f    = fun3 "list" (\[n, c, xs] -> xs ++ ".nil ? " ++ n ++ " : " ++ c ++ "(" ++ xs ++ ".head)(" ++ xs ++ ".tail)") b (lam2 f)


-- * JavaScript instances of AwesomePrelude type classes.

data Number

instance Num (Val JS) Number where
  data TNum (Val JS) Number
  negate x = lam (\(Var v) -> Prim (\[x] -> "(-(" ++ x ++ "))") [v]) `app` x
  abs = fun1 "abs" (\[a] -> "Math.abs(" ++ a ++ ")")
  signum = fun1 "signum" (\[a] -> "Math.signum(" ++ a ++ ")")
  (+) = fun2 "add" (\[a, b] -> a ++ " + " ++ b)
  (-) = fun2 "sub" (\[a, b] -> a ++ " - " ++ b)
  (*) = fun2 "mul" (\[a, b] -> a ++ " * " ++ b)
  fromInteger x = Con (P.show x)

instance Eq (Val JS) Bool where
  (==) = fun2 "eq"  (\[a, b] -> a ++ " == " ++ b)
  (/=) = fun2 "neq" (\[a, b] -> a ++ " /= " ++ b)

instance Eq (Val JS) Number where
  (==) = fun2 "eq"  (\[a, b] -> a ++ " == " ++ b)
  (/=) = fun2 "neq" (\[a, b] -> a ++ " /= " ++ b)

instance (Eq (Val JS) a, Eq (Val JS) b) => Eq (Val JS) (a, b) where
  (==) = fun2 "eq"  (\[a, b] -> a ++ " == " ++ b)
  (/=) = fun2 "neq" (\[a, b] -> a ++ " /= " ++ b)

instance Eq (Val JS) a => Eq (Val JS) [a] where
  (==) = fun2 "eq"  (\[a, b] -> a ++ " == " ++ b)
  (/=) = fun2 "neq" (\[a, b] -> a ++ " /= " ++ b)

