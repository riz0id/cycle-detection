{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE BlockArguments   #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE PackageImports   #-}

module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Morph
import Control.Monad.State
import Data.Bool
import Data.Foldable
import Data.Function
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace
import Data.Bifunctor

import "trees" Control.Applicative.Queue
import "trees" Control.Monad.List
import "trees" Control.Monad.Hyper
import "trees" Data.Graph
import "trees" Data.Path
import "trees" Data.Tree

-- ---------------------------------------------------------------------------------------------------------------------

type Search s = StateT s []

type History = (Map Int (Set Int), Set Int)

graph :: Graph (Search History) Int
graph = Graph \n -> do
  runGraph n next

next :: Graph (Search History) Int
next = Graph \n -> do
  gets (Map.lookup n . fst) >>= \case
    Nothing -> do
      ns <- neighbors n generator
      modify (first (Map.insert n ns))
      oneOf ns
    Just ns -> empty

generator :: Monad m => Graph m Int
generator = Graph \n -> do
  i <- oneOf [1, 2]
  let n' = n + i
  if 4 <= n'
    then oneOf [1 .. 5]
    else return n'

terminator :: Alternative m => Int -> m ()
terminator x = guard (x <= 5)

loops :: Int -> Graph (Search History) Int -> Graph (Search History) Int
loops n = path go
  where
    go :: Int -> ListT (Search History) Int
    go x = do
      seen <- gets (Set.member x . snd)
      if | seen && n == x -> return x
         | seen && n /= x -> empty
         | otherwise -> do
             modify (second (Set.insert x))
             return x

-- ---------------------------------------------------------------------------------------------------------------------

main :: IO ()
main = do
  let forest = evalStateT (index 0 graph) (Map.empty, Set.empty)
  putStrLn "Tree:"
  mapM_ (putStrLn . prettyShow) forest
  let cycles = evalStateT (index 2 (loops 2 graph)) (Map.empty, Set.empty)
  putStrLn "Loops:"
  mapM_ (putStrLn . prettyShow) cycles
  return ()
