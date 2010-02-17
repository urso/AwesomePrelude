{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE EmptyDataDecls #-}

module Generic.Data.Bool where

import Prelude ()

infixr 3  &&
infixr 2  ||


class BoolC l where
  data TBool l

  true  :: l (TBool l)
  false :: l (TBool l)
  bool  :: l a -> l a -> l (TBool l) -> l a
  if'   :: l (TBool l) -> l a -> l a -> l a
  (&&)  :: l (TBool l) -> l (TBool l) -> l (TBool l)
  (||)  :: l (TBool l) -> l (TBool l) -> l (TBool l)
  not   :: l (TBool l) -> l (TBool l)

  bool x y b = if' b x y
  if' b x y  = bool x y b
  a && b     = if' a b false
  a || b     = if' a true b
  not        = bool false true

