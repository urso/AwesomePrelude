{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances #-}

module Generic.Data.Eq where

import Prelude ()
import Generic.Data.Bool

infix  4  ==, /=

class (BoolC l) => Eq l a where
  (==) :: l a -> l a -> l (TBool l)
  (/=) :: l a -> l a -> l (TBool l)

  x /= y = not (x == y)
  x == y = not (x /= y)

instance (BoolC l) => Eq l (TBool l) where
  x == y = if' x y (not y)

