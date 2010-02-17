{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Generic.Control.Function where

import qualified Prelude
import Generic.Control.Category

undefined :: a
undefined = Prelude.undefined

class NameC j where
  named :: Prelude.String -> j a -> j a

class FunC j where
  type TFun j :: * -> * -> *
  lam :: (j a -> j b) -> j (TFun j a b)
  app :: j (TFun j a b) -> j a -> j b
  -- lam :: (j a -> j b) -> j (a -> b)
  -- app :: j (a -> b) -> j a -> j b

class FunC l => RecFunC l where
--   fix :: (l (a -> b) -> l (a -> b)) -> l (a -> b)
  fix :: (l (TFun l a b) -> l (TFun l a b)) -> l (TFun l a b)

instance (NameC j, FunC j) => Category j (->) where
  id a      = "id" `named` (lam (\i -> i) `app` a)
  (.) f g a = lam f `app` (lam g `app` a)

infixr 0 $

($) :: FunC j => (j a -> j b) -> j a -> j b
($) f a = lam f `app` a

const :: FunC j => j a -> j b -> j a
const a b = lam2 (\c _ -> c) `app` a `app` b

-- -- Helper functions.

lam2 :: FunC j => (j a -> j b -> j c) -> j (TFun j a (TFun j b c))
lam2 f = lam (\a -> lam (f a))

lam3 :: (FunC j) => (j a -> j b -> j c -> j d) -> j (TFun j a (TFun j b (TFun j c d)))
lam3 f = lam (\a -> lam2 (f a))

-- app2 :: FunC j => j (a -> b -> c) -> j a -> j b -> j c
app2 :: (FunC j) => j (TFun j a (TFun j b c)) -> j a -> j b -> j c
app2 f x y = (f `app` x) `app` y

-- app3 :: FunC j => j (a -> b -> c -> d) -> j a -> j b -> j c -> j d
app3 :: (FunC j) => j (TFun j a (TFun j b (TFun j c d))) -> j a -> j b -> j c -> j d
app3 f x y z = app2 f x y `app` z

