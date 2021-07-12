{-# LANGUAGE BlockArguments  #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE PatternSynonyms #-}

module Control.Monad.Hyper
  ( -- * HyperT
    HyperT (HyperT)
  , invokeT
  , hyperZipWithM
  , hyperZipM

    -- * Hyper
  , Hyper
  , pattern Hyper
  , invoke
  )
where

import Control.Monad.List (ListT(ListT))
import Control.Monad (join)
import Data.Kind (Type)
import Data.Functor.Identity (Identity(Identity))

-- ---------------------------------------------------------------------------------------------------------------------

newtype HyperT :: (Type -> Type) -> Type -> Type -> Type where
  HyperT :: m ((HyperT m a b -> a) -> b) -> HyperT m a b

invokeT :: HyperT m a b -> m ((HyperT m a b -> a) -> b)
invokeT (HyperT m) = m
{-# INLINE invokeT #-}

hyperZipWithM :: Monad m => (a -> b -> c) -> ListT m a -> ListT m b -> ListT m c
hyperZipWithM op (ListT f) (ListT g) = ListT \c nil ->
  let fs x xs = pure (\k -> k (HyperT xs) x)
      gs y ys = pure (\k x -> c (x `op` y) (join (invokeT k <*> ys)))
  in join (f fs (pure (const nil)) <*> g gs (pure \_ _ -> nil))
{-# INLINE hyperZipWithM #-}

hyperZipM :: Monad m => ListT m a -> ListT m b -> ListT m (a, b)
hyperZipM = hyperZipWithM (,)
{-# INLINE hyperZipM #-}

-- ---------------------------------------------------------------------------------------------------------------------

type Hyper = HyperT Identity

pattern Hyper :: ((Hyper a b -> a) -> b) -> Hyper a b
pattern Hyper f = HyperT (Identity f)
{-# COMPLETE Hyper #-}

invoke :: Hyper a b -> ((Hyper a b -> a) -> b)
invoke (Hyper m) = m
{-# INLINE invoke #-}
