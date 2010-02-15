{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}

module Generic.Data.Num where

import Prelude ()
import qualified Prelude

infixl 6 +
infixl 7 *
infixl 6 -
infixl 7 /

class Num l a where
  negate :: l a -> l a
  abs    :: l a -> l a
  signum :: l a -> l a
  fromInteger :: Prelude.Integer -> l a
  (+) :: l a -> l a -> l a 
  (-) :: l a -> l a -> l a
  (*) :: l a -> l a -> l a

class Integral l a where
  quot    :: l a -> l a -> l a
  rem     :: l a -> l a -> l a
  div     :: l a -> l a -> l a
  mod     :: l a -> l a -> l a
  quotRem :: l a -> l a -> l (a, a)
  divMod  :: l a -> l a -> l (a, a)

class FractionalOp l a where
  recip :: a -> a

class (FractionalOp l a, FractionalOp l b) => Fractional l a b where
  type TFractionalResult a b
  (/) :: l a -> l b -> l (TFractionalResult a b)

class Floating l a where
  pi    :: l a
  exp   :: l a
  sqrt  :: l a
  log   :: l a
  sin   :: l a
  cos   :: l a
  tan   :: l a
  asin  :: l a
  acos  :: l a
  atan  :: l a
  sinh  :: l a
  cosh  :: l a
  tanh  :: l a
  asinh :: l a
  acosh :: l a
  atanh :: l a

-- class Num j a where
--   (+)         :: j a -> j a -> j a
--   (-)         :: j a -> j a -> j a
--   (*)         :: j a -> j a -> j a
--   fromInteger :: Prelude.Integer -> j a

-- Terrible hack to get number literals working.

instance Prelude.Show (j a) where show _ = "num"
instance Prelude.Eq   (j a) where
-- instance Num j a => Prelude.Num  (j a) where
--   (+)    = Prelude.undefined
--   (*)    = Prelude.undefined
--   abs    = Prelude.undefined
--   signum = Prelude.undefined
--   fromInteger = fromInteger

