{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeFamilies, UndecidableInstances #-}

module Lang.Haskell where

import qualified Prelude as P

import Generic.Prelude

newtype Haskell a = Hs { runHaskell :: a }

instance NameC Haskell where
  named _ a = a

instance FunC Haskell where
  lam f = Hs (\x -> runHaskell (f (Hs x)))
  app (Hs f) (Hs x) = Hs (f x)

instance RecFunC Haskell where
  fix f = f (fix f)

instance BoolC Haskell where
  data TBool Haskell = True | False
  false = Hs False
  true  = Hs True
  if' (Hs True) x _  = x
  if' (Hs False) _ y = y

instance MaybeC Haskell where
  data TMaybe Haskell a = Nothing | Just a
  nothing     = Hs Nothing
  just (Hs x) = Hs (Just x)

instance TupleC Haskell where
  mkTuple (Hs x) (Hs y) = Hs (x, y)
  tuple f (Hs (x, y)) = f (Hs x) (Hs y)

instance EitherC Haskell where
  data TEither Haskell a b = Left a | Right b
  left (Hs x)  = Hs (Left x)
  right (Hs x) = Hs (Right x)
  either l _ (Hs (Left e))  = l (Hs e)
  either _ r (Hs (Right e)) = r (Hs e)

instance ListC Haskell where
  nil = Hs []
  cons (Hs x) (Hs xs) = Hs (x:xs)
  list n c (Hs xs)    = case xs of { [] -> n; y:ys -> c (Hs y) (Hs ys) }

instance ListOp Haskell

instance (P.Num a) => Num Haskell a where
  (Hs a) + (Hs b) = Hs (a P.+ b)
  (Hs a) - (Hs b) = Hs (a P.- b)
  (Hs a) * (Hs b) = Hs (a P.* b)
  negate (Hs x)   = Hs (P.negate x)
  abs (Hs x)      = Hs (P.abs x)
  signum (Hs x)   = Hs (P.signum x)
  fromInteger x   = Hs (P.fromInteger x)

instance (P.Eq a) => Eq Haskell a where
  x == y = if x P.== y then true else false

