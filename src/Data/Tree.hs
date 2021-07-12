{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Tree
  ( Tree ((:&))
  , singleton
  , root
  , leafs
  , walkTree
  , traverseTree
  , walkMapTree
  , prettyShow
  , prettyShowS
  )
where

import Control.Applicative ( Applicative(liftA2) )
import Control.Applicative.Queue ( Queue, lowerQueue, now, later )
import Data.Functor.Const
import qualified Data.Tree.Show as Show

-- ---------------------------------------------------------------------------------------------------------------------

data Tree a = a :& [Tree a]
  deriving Show

singleton :: a -> Tree a
singleton x = x :& []
{-# INLINE singleton #-}

root :: Tree a -> a
root (x :& _) = x
{-# INLINE root #-}

leafs :: Tree a -> [Tree a]
leafs (_ :& xs) = xs
{-# INLINE leafs #-}

walkTree :: forall f a b. Applicative f => (a -> f b) -> Tree a -> f (Tree b)
walkTree f = lowerQueue . go
  where
    go :: Tree a -> Queue f (Tree b)
    go (x :& xs) = liftA2 (:&) (now (f x)) (later (traverse go xs))
{-# INLINE walkTree #-}

traverseTree :: Applicative f => (a -> f b) -> (Tree a -> f (Tree b)) -> Tree a -> f (Tree b)
traverseTree f g (x :& xs) = liftA2 (:&) (f x) (traverse g xs)
{-# INLINE traverseTree #-}

-- | Map a tree into a monoid.
--
-- @since 0.1.0.0
walkMapTree :: Monoid m => (a -> m) -> (m -> m) -> Tree a -> m
walkMapTree f g = getConst . traverseTree (Const . f) (Const . g . walkMapTree f g)
{-# INLINE walkMapTree #-}

prettyShow :: Show a => Tree a -> String
prettyShow tree = prettyShowS tree ""
{-# INLINE prettyShow #-}

prettyShowS :: Show a => Tree a -> ShowS
prettyShowS = Show.showTreeDiagram . walkMapTree Show.singleton Show.subtree
