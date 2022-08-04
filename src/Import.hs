{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Import
  ( module RIO,
    module RIO.Lens,
    module Types,
    module Control.Monad.Free,
    module Control.Monad.State,
    module Control.Monad.Cont,
    (:+:) (..),
    (:<:),
    inj,
    inject,
  )
where

import Control.Monad.Cont
import Control.Monad.Free
import Control.Monad.State
import RIO
import RIO.Lens
import Types

data (f :+: g) e = Inl (f e) | Inr (g e)

instance (Functor f, Functor g) => Functor (f :+: g) where
  fmap f (Inl e1) = Inl (fmap f e1)
  fmap f (Inr e2) = Inr (fmap f e2)

class (Functor sub, Functor sup) => sub :<: sup where
  inj :: sub a -> sup a

instance Functor f => f :<: f where
  inj = id

instance {-# OVERLAPPING #-} (Functor f, Functor g) => f :<: (f :+: g) where
  inj = Inl

instance {-# OVERLAPPING #-} (Functor f, Functor g, Functor h, f :<: g) => f :<: (h :+: g) where
  inj = Inr . inj

inject :: (g :<: f) => g (Free f a) -> Free f a
inject = Free . inj