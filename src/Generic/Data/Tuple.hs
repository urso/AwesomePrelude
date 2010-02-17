{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Generic.Data.Tuple where

import Prelude ()
import Generic.Data.Bool
import Generic.Data.Eq
import Generic.Data.Ord

class TupleC j where
  data TTuple j :: * -> * -> *

  mkTuple :: j a -> j b -> j (TTuple j a b)
  tuple   :: (j a -> j b -> j r) -> j (TTuple j a b) -> j r

instance (BoolC j, TupleC j, Eq j a, Eq j b) => Eq j (TTuple j a b) where
  x == y = tuple (\xa xb -> tuple (\ya yb -> xa == ya && xb == yb) y) x

instance (BoolC j, TupleC j, Eq j a, Eq j b, OrdC j a, OrdC j b) => OrdC j (TTuple j a b) where
  x <= y = tuple (\xa xb -> tuple (\ya yb -> xa <= ya && xb <= yb) y) x

swap :: TupleC j => j (TTuple j a b) -> j (TTuple j b a)
swap = tuple (\a b -> mkTuple b a)

