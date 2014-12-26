{-# LANGUAGE GADTs #-}


-----------------------------------------------------------------------------
-- |
-- Module      :  Data.TASequence.ConsList
-- Copyright   :  (c) Atze van der Ploeg 2014
-- License     :  BSD-style
-- Maintainer  :  atzeus@gmail.org
-- Stability   :  provisional
-- Portability :  portable
--
-- A type aligned sequence, a snoc list, with worst case constant time: '|>', and 'tviewr'.
--
-----------------------------------------------------------------------------

module Data.TASequence.SnocList(module Data.TASequence,SnocList(..))  where

import Data.TASequence

data SnocList c x y where
  SNil :: SnocList c x x
  Snoc :: SnocList c x y -> c y z -> SnocList c x z

instance TASequence SnocList where
  tempty = SNil
  tsingleton c = Snoc SNil c 
  (|>) = Snoc
  tviewr SNil = TAEmptyR
  tviewr (Snoc p l) = p :> l

instance Maps SnocList where
  maps phi SNil = SNil
  maps phi (Snoc s c) = Snoc (maps phi s) (phi c)