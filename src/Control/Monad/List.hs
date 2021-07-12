{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}

module Control.Monad.List
  ( ListT (ListT)
  , runListT
  , foldMapA
  , oneOf
  )
where

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Morph
import Control.Monad.State
import Data.Distributive
import Data.Foldable
import Data.Monoid
import Data.Kind
import Data.Functor.Identity

-- ---------------------------------------------------------------------------------------------------------------------

newtype ListT :: (Type -> Type) -> Type -> Type where
  ListT :: (forall x. (a -> m x -> m x) -> m x -> m x) -> ListT m a

runListT :: (a -> m b -> m b) -> m b -> ListT m a -> m b
runListT cons nil (ListT m) = m cons nil
{-# INLINE runListT #-}

foldMapA :: (Foldable t, Alternative m) => (a -> m b) -> t a -> m b
foldMapA f = getAlt . foldMap (Alt . f)
{-# INLINE foldMapA #-}

oneOf :: (Foldable t, Alternative m) => t a -> m a
oneOf = foldMapA pure
{-# INLINE oneOf #-}

instance Functor (ListT m) where
  fmap f (ListT g) = ListT \cons nil -> g (cons . f) nil
  {-# INLINE fmap #-}

instance Applicative (ListT m) where
  pure x = ListT \cons nil -> cons x nil
  {-# INLINE pure #-}

  ListT f <*> ListT g = ListT \cons nil -> f (\m nil' -> g (cons . m) nil') nil
  {-# INLINE (<*>) #-}

instance Monad (ListT m) where
  ListT m >>= f = ListT \cons nil -> m (\x nil' -> runListT cons nil' (f x)) nil
  {-# INLINE (>>=) #-}

instance Alternative (ListT m) where
  empty = ListT \_ nil -> nil
  {-# INLINE empty #-}

  ListT f <|> ListT g = ListT \cons nil -> f cons (g cons nil)
  {-# INLINE (<|>) #-}

instance MonadIO m => MonadIO (ListT m) where
  liftIO m = ListT \cons nil -> liftIO m >>= (`cons` nil)
  {-# INLINE liftIO #-}

instance MonadTrans ListT where
  lift m = ListT \cons nil -> m >>= (`cons` nil)
  {-# INLINE lift #-}

instance MonadState s m => MonadState s (ListT m) where
  get = lift get
  {-# INLINE get #-}

  put = lift . put
  {-# INLINE put #-}
