{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Control.Applicative.Free
  ( Free (Pure, Lift)
  , lowerFree
  , liftFree
  )
where

import Control.Applicative
import Data.Kind (Type)

-- ---------------------------------------------------------------------------------------------------------------------

data Free :: (Type -> Type) -> Type -> Type where
  Pure :: a -> Free f a
  Lift :: (a -> b -> c) -> f a -> Free f b -> Free f c

instance Functor (Free f) where
  fmap f (Pure x) = Pure (f x)
  fmap f (Lift op x xs) = Lift (\a b -> f (a `op` b)) x xs
  {-# INLINE fmap #-}

instance Applicative f => Applicative (Free f) where
  pure = Pure
  {-# INLINE pure #-}

  liftA2 op (Pure x) ys = fmap (op x) ys
  liftA2 op xs (Pure y) = fmap (`op` y) xs
  liftA2 c (Lift f x xs) (Lift g y ys) = Lift
      (\(x,y) (xs,ys) -> c (f x xs) (g y ys))
      (liftA2 (,) x y)
      (liftA2 (,) xs ys)
  {-# INLINE liftA2 #-}

lowerFree :: Applicative f => Free f a -> f a
lowerFree (Pure x) = pure x
lowerFree (Lift f x xs) = liftA2 f x (lowerFree xs)
{-# INLINE lowerFree #-}

liftFree :: f a -> Free f a
liftFree x = Lift const x (liftFree x)
{-# INLINE liftFree #-}
