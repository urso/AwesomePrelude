{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}

module Lang.Haskell where

import qualified Prelude as P

import Generic.Prelude

data Haskell a = Hs a

class RunHaskell a where
  type TResult a
  runHaskell :: a -> (TResult a)

type Bool = TBool Haskell
type HsNum a = TNum Haskell a
type Ordering = TOrdering Haskell
type Tuple a b = TTuple Haskell a b
type Maybe a = TMaybe Haskell a
type Either a b = TEither Haskell a b

instance NameC Haskell where
  named _ a = a

instance FunC Haskell where
  lam f = Hs (\x -> unPack (f (Hs x)))
    where unPack (Hs x) = x

  app (Hs f) (Hs x) = Hs (f x)

instance RecFunC Haskell where
  fix f = f (fix f)

instance BoolC Haskell where
  data TBool Haskell = True | False
    deriving(P.Show)

  false = Hs False
  true  = Hs True
  if' (Hs True) x _  = x
  if' (Hs False) _ y = y

instance (P.Num a) => Num Haskell a where
  data TNum Haskell a = HsNum a
    deriving(P.Show)

  (Hs (HsNum a)) + (Hs (HsNum b)) = Hs (HsNum (a P.+ b))
  (Hs (HsNum a)) - (Hs (HsNum b)) = Hs (HsNum (a P.- b))
  (Hs (HsNum a)) * (Hs (HsNum b)) = Hs (HsNum (a P.* b))
  negate (Hs (HsNum x))   = Hs (HsNum (P.negate x))
  abs (Hs (HsNum x))      = Hs (HsNum (P.abs x))
  signum (Hs (HsNum x))   = Hs (HsNum (P.signum x))
  fromInteger x   = Hs (HsNum (P.fromInteger x))

num :: (P.Num a) => a -> Haskell (HsNum a)
num x = Hs (HsNum x)

instance OrderingC Haskell where
  data TOrdering Haskell = LT | EQ | GT deriving (P.Eq)
  lt = Hs LT
  eq = Hs EQ
  gt = Hs GT
  ordering l e g tst = if' (tst == eq) e (if' (tst == lt) l g)

instance (P.Num a, P.Ord a) => OrdC Haskell (HsNum a) where
  (Hs (HsNum x)) <= (Hs (HsNum y)) = if x P.<= y then true else false

instance Eq Haskell Ordering where
  (Hs x) == (Hs y) = if x P.== y then true else false

instance (P.Num a) => Eq Haskell (HsNum a) where
  (Hs (HsNum x)) == (Hs (HsNum y)) = if x P.== y then true else false

instance TupleC Haskell where
  data TTuple Haskell a b = Pair a b
  mkTuple (Hs a) (Hs b) = Hs (Pair a b)
  tuple f (Hs (Pair a b)) = f (Hs a) (Hs b)

instance MaybeC Haskell where
  data TMaybe Haskell a = Just a | Nothing
  nothing     = Hs Nothing
  just (Hs x) = Hs (Just x)

instance MaybeOp Haskell where
  maybe d _ (Hs Nothing) = d
  maybe _ f (Hs (Just x)) = f (Hs x)

instance ListC Haskell where
  nil = Hs []
  cons (Hs x) (Hs xs) = Hs (x:xs)
  list n c (Hs xs)    = case xs of { [] -> n; y:ys -> c (Hs y) (Hs ys) }

instance ListOp Haskell

instance EitherC Haskell where
  data TEither Haskell a b = Left a | Right b
  left (Hs x)  = Hs (Left x)
  right (Hs x) = Hs (Right x)
  either l _ (Hs (Left e))  = l (Hs e)
  either _ r (Hs (Right e)) = r (Hs e)

instance RunHaskell Bool where
  type TResult Bool = Bool
  runHaskell x = x

instance (P.Num a) => RunHaskell (HsNum a) where
  type TResult (HsNum a) = a
  runHaskell (HsNum a) = a

instance (RunHaskell a) => RunHaskell (Haskell a) where
  type TResult (Haskell a) = TResult a
  runHaskell (Hs a) = runHaskell a

instance RunHaskell Ordering where
  type TResult Ordering = P.Ordering
  runHaskell LT = P.LT
  runHaskell GT = P.GT
  runHaskell EQ = P.EQ

instance (RunHaskell a, RunHaskell b) => RunHaskell (Tuple a b) where
  type TResult (Tuple a b) = (TResult a, TResult b)
  runHaskell (Pair a b) = (runHaskell a, runHaskell b)

instance (RunHaskell a) => RunHaskell (Maybe a) where
  type TResult (Maybe a) = P.Maybe (TResult a)
  runHaskell Nothing  = P.Nothing
  runHaskell (Just a) = P.Just (runHaskell a)

instance (RunHaskell a) => RunHaskell [a] where
  type TResult [a] = [TResult a]
  runHaskell = P.map runHaskell

instance (RunHaskell a, RunHaskell b) => RunHaskell (Either a b) where
  type TResult (Either a b) = P.Either (TResult a) (TResult b)
  runHaskell (Left x) = P.Left (runHaskell x)
  runHaskell (Right x) = P.Right (runHaskell x)

