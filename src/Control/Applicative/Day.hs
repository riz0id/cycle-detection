{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module Control.Applicative.Day
  ( Day (Day)
  , runDay
  )
where

import Data.Kind (Type)
import Data.Bifunctor (first)
import Control.Applicative (liftA2)

-- ---------------------------------------------------------------------------------------------------------------------

newtype Day :: (Type -> Type) -> Type -> Type where
  Day :: (forall x. f x -> f (a, x)) -> Day f a

runDay :: f x -> Day f a -> f (a, x)
runDay x (Day f) = f x
{-# INLINE runDay #-}

instance Functor f => Functor (Day f) where
  fmap f (Day g) = Day (fmap (first f) . g)
  {-# INLINE fmap #-}

instance Functor f => Applicative (Day f) where
  pure x = Day (fmap (x,))
  {-# INLINE pure #-}

  liftA2 m (Day f) (Day g) = Day (fmap (\(x, (y, z)) -> (m x y, z)) . f . g)
  {-# INLINE liftA2 #-}
