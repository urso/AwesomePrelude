{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE EmptyDataDecls, FlexibleInstances, MultiParamTypeClasses #-}

module Generic.Data.Either where

import Prelude ()
import Generic.Data.Bool
import Generic.Data.Eq
import Generic.Data.Ord
import Generic.Control.Function


class EitherC l where
  data TEither l :: * -> * -> *
  left   :: l a -> l (TEither l a b)
  right  :: l b -> l (TEither l a b)
  either :: (l a -> l r) -> (l b -> l r) -> l (TEither l a b) -> l r

instance (EitherC l, BoolC l, Eq l a, Eq l b, FunC l) => Eq l (TEither l a b) where
  ex == ey = either (\x -> either (\y -> x == y) (const false) ey)
                    (\x -> either (const false) (\y -> x == y) ey)
                    ex

instance (EitherC l, BoolC l, OrdC l a, OrdC l b, FunC l) => OrdC l (TEither l a b) where
  ex <= ey = either (\x -> either (\y -> x <= y) (const true) ey)
                    (\x -> either (const false) (\y -> x <= y) ey)
                    ex

