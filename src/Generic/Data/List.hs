{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}

module Generic.Data.List where

import Prelude ()
import Generic.Data.Bool
import Generic.Data.Eq
import Generic.Data.Num
import Generic.Data.Ord
import Generic.Control.Function
import Generic.Control.Functor

-- data List a
-- instead of `List a`, use `[a]`

class ListC j where
  nil  :: j [a]
  cons :: j a -> j [a] -> j [a]
  list :: j r -> (j a -> j [a] -> j r) -> j [a] -> j r

class (ListC j) => ListOp j where
  singleton     :: j a -> j [a]
  map           :: (RecFunC j) => (j a -> j b) -> j [a] -> j [b]
  replicate     :: (Num j a, Eq j a, RecFunC j) => j a -> j b -> j [b]
  (++)          :: (RecFunC j) => j [a] -> j [a] -> j [a]
  genericLength :: (RecFunC j, Num j a) => j [b] -> j a
  sum           :: (RecFunC j, Num j a) => j [a] -> j a
  filter        :: (BoolC j, RecFunC j) => (j a -> j (TBool j)) -> j [a] -> j [a]
  reverse       :: (RecFunC j) => j [a] -> j [a]
  and           :: (BoolC j, RecFunC j) => j [(TBool j)] -> j (TBool j)

  singleton a = a `cons` nil

  map f = foldr (\y ys -> f y `cons` ys) nil

  replicate n a = (fix (\r -> lam (\y -> if' (y == fromInteger 0) nil 
                                             (a `cons` 
                                              (r `app` (y - fromInteger 1)))))) `app` n

  xs ++ ys = foldr cons ys xs

  genericLength = foldr (\_ -> (+ (fromInteger 1))) (fromInteger 0)

  sum = foldr (+) (fromInteger 0)

  filter p = foldr (\x xs -> bool xs (x `cons` xs) (p x)) nil

  reverse l = rev `app` l `app` nil
    where
      rev = fix (\r -> lam (\xs -> lam (\a -> list a (\y ys -> r `app` ys `app` (y `cons` a)) xs)))

  and = foldr (&&) true

  foldr :: (RecFunC j, ListC j) => (j a -> j b -> j b) -> j b -> j [a] -> j b
  foldr f b xs = fix (\r -> lam (list b (\y ys -> f y (r `app` ys)))) `app` xs

instance (BoolC j, ListC j, Eq j a) => Eq j [a] where
  xs == ys = list (list true (\_ _ -> false) ys)
                  (\x xs' -> list false (\y ys' -> x == y && xs' == ys') ys)
                  xs

instance (BoolC j, ListC j, OrdC j a) => OrdC j [a] where
  xs <= ys = list true -- (list true (\_ _ -> true) ys)
                  (\x xs' -> list false (\y ys' -> x <= y || xs' <= ys') ys)
                  xs

instance (RecFunC j, ListC j, ListOp j) => Functor j [] where
  fmap f = foldr (\a r -> f a `cons` r) nil

