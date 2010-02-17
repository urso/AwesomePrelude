{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}

module Generic.Data.Ord where

import Prelude (($))
import qualified Prelude as P
import Generic.Data.Bool
import Generic.Data.Eq

infix  4  <, <=, >=, >

class OrderingC l where
  data TOrdering l
  lt :: l (TOrdering l)
  gt :: l (TOrdering l)
  eq :: l (TOrdering l)
  ordering :: l a -> l a -> l a -> l (TOrdering l) -> l a

class (Eq l a, OrderingC l) => OrdC l a where
  compare :: l a -> l a -> l (TOrdering l)
  (<), (<=), (>), (>=) :: l a -> l a -> l (TBool l)
  max, min             :: l a -> l a -> l a

  compare x y = if' (x == y) eq $ if' (x <= y) lt gt
  
  x <  y = ordering true  false false (compare x y)
  x <= y = ordering true  true  false (compare x y)
  x >  y = ordering false false true  (compare x y)
  x >= y = ordering false true  true  (compare x y)

  max x y = if' (x <= y) y x
  min x y = if' (x <= y) x y

instance (OrderingC l, BoolC l) => OrdC l (TBool l) where
  x <= y = if' x true (if' y false true)

instance (OrderingC l, BoolC l) => Eq l (TOrdering l) where
  x == y = ordering (ordering true  false false y)
                    (ordering false true  false y)
                    (ordering false false true  y)
                    x

instance (OrderingC l, BoolC l) => OrdC l (TOrdering l) where
  x <= y = ordering true -- (ordering true  true  true y)
                    (ordering false true  true y)
                    (ordering false false true y)
                    x

comparing :: (OrdC l a, BoolC l, OrderingC l) => 
             (b -> l a) -> b -> b -> l (TOrdering l)
comparing p x y = compare (p x) (p y)

