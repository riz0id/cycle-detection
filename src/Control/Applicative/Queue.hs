{-# LANGUAGE BlockArguments           #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE ViewPatterns             #-}

module Control.Applicative.Queue
  ( Queue
  , lowerQueue
  , now
  , later
  )
where

import Control.Applicative ( Applicative(liftA2) )
import Control.Applicative.Day ( Day(Day), runDay )
import Control.Applicative.Free ( Free (Lift, Pure), lowerFree, liftFree )
import Data.Kind (Type)

-- ---------------------------------------------------------------------------------------------------------------------

type Queue :: (Type -> Type) -> Type -> Type
type Queue f = Day (Free f)

lowerQueue :: Applicative f => Queue f a -> f a
lowerQueue = fmap fst . lowerFree . runDay (Pure ())
{-# INLINE lowerQueue #-}

now :: Applicative f => f a -> Queue f a
now xs = Day \case
  Pure x      -> Lift (,) xs (Pure x)
  Lift f y ys -> Lift (\(x,y) z -> (x, f y z)) (liftA2 (,) xs y) ys
{-# INLINE now #-}

later :: Applicative f => Queue f a -> Queue f a
later xs = Day \case
  Pure x      -> Lift (const id) (pure ()) (runDay (Pure x) xs)
  Lift f y ys -> Lift (\x (y,z) -> (y, f x z)) y (runDay ys xs)
{-# INLINE later #-}
