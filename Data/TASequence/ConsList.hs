{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE PolyKinds #-}
#endif



-----------------------------------------------------------------------------
-- |
-- Module      :  Data.TASequence.ConsList
-- Copyright   :  (c) Atze van der Ploeg 2014
-- License     :  BSD-style
-- Maintainer  :  atzeus@gmail.org
-- Stability   :  provisional
-- Portability :  portable
--
-- A type aligned sequence, a head-tail list, with worst case constant time: '<|', and 'tviewl'.
--
-----------------------------------------------------------------------------
module Data.TASequence.ConsList(module Data.TASequence,ConsList(..)) where
import Control.Category
import Data.TASequence

data ConsList c x y where
  CNil :: ConsList c x x
  Cons :: c x y -> ConsList c y z -> ConsList c x z

instance TASequence ConsList where
  tempty = CNil
  tsingleton c = Cons c CNil
  (<|) = Cons
  tviewl CNil = TAEmptyL
  tviewl (Cons h t) = h :< t

instance Category (ConsList c) where
  id = tempty
  (.) = flip (><)
