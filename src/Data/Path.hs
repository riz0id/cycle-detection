{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Path
  ( Path
  , path
  )
where

import Control.Applicative
import Control.Monad.Hyper
import Control.Monad.List
import Control.Monad.Morph
import Data.Graph
import Data.Maybe
import Data.Tree

-- ---------------------------------------------------------------------------------------------------------------------

type Path f a = HyperT Maybe (Graph f a) (Graph f a)

path :: forall f a. (Alternative f, Monad f) => (a -> ListT f a) -> Graph f a -> Graph f a
path m graph = r (f graph e)
  where
    f :: Graph f a -> Path f a -> Path f a
    f (Graph g) fw = HyperT (Just \k -> Graph \x -> g x >>= m)

    e :: Path f a
    e = HyperT Nothing

    r :: Path f a -> Graph f a
    r = maybe emptyGraph (\k -> k r) . invokeT
