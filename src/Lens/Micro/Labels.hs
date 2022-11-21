{-# LANGUAGE AllowAmbiguousTypes
           , DataKinds
           , FlexibleInstances
           , FunctionalDependencies
           , GADTs
           , KindSignatures
           , ScopedTypeVariables
           , TypeApplications
           , UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Lens.Micro.Labels where

import           GHC.OverloadedLabels
import           GHC.TypeLits



class Label (x :: Symbol) s a | x s -> a where
  label :: Functor f => (a -> f a) -> s -> f s



instance (Functor f, Label x s a, q ~ (->) (a -> f a) (s -> f s)) => IsLabel x q where
  fromLabel = label @x
