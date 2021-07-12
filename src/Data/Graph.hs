{-# LANGUAGE BlockArguments       #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Data.Graph
  ( -- * Graph
    Graph (Graph)
  , runGraph
  , neighbors
  , index
  , emptyGraph
  )
where

import Control.Applicative
import Control.Monad.List (ListT (ListT), runListT)
import Data.Kind (Type)
import Data.Tree (Tree((:&)))
import Data.Set (Set)
import qualified Data.Set as Set

-- ---------------------------------------------------------------------------------------------------------------------

newtype Graph :: (Type -> Type) -> Type -> Type where
  Graph :: (a -> ListT f a) -> Graph f a

runGraph :: a -> Graph f a -> ListT f a
runGraph x (Graph f) = f x
{-# INLINE runGraph #-}

neighbors :: (Applicative f, Ord a) => a -> Graph f a -> f (Set a)
neighbors x (Graph f) = runListT (\y xs -> fmap (Set.insert y) xs) (pure mempty) (f x)
{-# INLINE neighbors #-}

index :: (Monad f, Ord a) => a -> Graph f a -> f (Tree a)
index x graph = do
  xs <- Set.toList <$> neighbors x graph
  subtrees <- mapM (`index` graph) xs
  return (x :& subtrees)
{-# INLINE index #-}

emptyGraph :: Graph f a
emptyGraph = Graph (const empty)
