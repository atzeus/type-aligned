{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Trustworthy #-}
#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE PolyKinds #-}
#endif
-- We suppress this warning because otherwise GHC complains
-- about the newtype constructor not being used.
#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
#endif

-- | It's safe to coerce /to/ 'Any' as long as you don't
-- coerce back. We define our own 'Any' instead of using
-- the one in "GHC.Exts" directly to ensure that this
-- module doesn't clash with one making the opposite
-- assumption.
module Data.TASequence.Any
  ( Any
  , AnyCat
  , toAnyConsList
  ) where

import Data.TASequence.ConsList
import Unsafe.Coerce

#if __GLASGOW_HASKELL__ >= 800
type family Any :: k where
#elif __GLASGOW_HASKELL__ >= 706
-- Closed type families used to need at least one instance. By hiding the
-- family itself and only exposing the synonym, we prevent instantiation.
-- It's a bit weird that this works even with TypeSynonymInstances, but
-- that's a bit lucky.
type Any = Any'
type family Any' :: k
#else
type Any = Any'
type family Any'
#endif

newtype AnyCat a b = AnyCat Any

-- | Convert a list of anything to a list of 'Any'.
toAnyConsList :: ConsList tc a c -> ConsList AnyCat Any c
toAnyConsList = unsafeCoerce
