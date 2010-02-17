{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE EmptyDataDecls, FlexibleInstances, MultiParamTypeClasses #-}

module Generic.Data.Maybe where

import Prelude ()
import Generic.Data.Bool
import Generic.Data.Eq
import Generic.Data.List
import Generic.Data.Ord
import Generic.Control.Function
import Generic.Control.Category
import Generic.Control.Functor


class MaybeC l where
  data TMaybe l :: * -> *
  nothing     :: l (TMaybe l a)
  just        :: l a -> l (TMaybe l a)

class (BoolC l, MaybeC l, FunC l) => MaybeOp l where
  maybe     :: l r -> (l a -> l r) -> l (TMaybe l a) -> l r
  isJust    :: l (TMaybe l a) -> l (TBool l)
  isNothing :: l (TMaybe l a) -> l (TBool l)
  fromMaybe :: l a -> l (TMaybe l a) -> l a

  isJust      = maybe false (const true)
  isNothing   = maybe true (const false)
  fromMaybe d = maybe d (\a -> a) 

instance (Eq l a, MaybeOp l) => Eq l (TMaybe l a) where
  mx == my = maybe (isNothing my) 
                   (\x -> maybe false (\y -> x == y) my)
                   mx

instance (OrdC l a, MaybeOp l) => OrdC l (TMaybe l a) where
  mx <= my = maybe true
             (\x -> maybe false (\y -> x <= y) my)
             mx

instance (MaybeOp l) => Functor l (TMaybe l) where
  fmap f = maybe nothing (\a -> lam just `app` (lam f `app` a))

catMaybes :: (RecFunC j, ListOp j, MaybeOp j) => j [TMaybe j a] -> j [a]
catMaybes = foldr (\a b -> maybe nil singleton a ++ b) nil

