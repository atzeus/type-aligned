{-# LANGUAGE GADTs, PolyKinds, ScopedTypeVariables #-}



-----------------------------------------------------------------------------
-- |
-- Module      :  Data.TASequence.CatQueue
-- Copyright   :  (c) Atze van der Ploeg 2013
-- License     :  BSD-style
-- Maintainer  :  atzeus@gmail.org
-- Stability   :  provisional
-- Portability :  portable
--
-- A purely functional catenable queue representation with
-- that turns takes a purely functional queue and turns in it into
-- a catenable queue, i.e. with the same complexity for '><' as for '|>'
-- Based on Purely functional data structures by Chris Okasaki 
-- section 7.2: Catenable lists
--
-----------------------------------------------------------------------------

module Data.TASequence.ToCatQueue(module Data.TASequence,ToCatQueue) where


import Control.Category
import Data.TASequence
import Prelude hiding (id)

-- | The catenable queue type. The first type argument is the 
-- type of the queue we use (|>)
data ToCatQueue q c x y where
  C0 :: ToCatQueue q c x x
  CN :: c x y -> !(q (ToCatQueue q c) y z) -> ToCatQueue q c x z

instance TASequence q => TASequence (ToCatQueue q) where
 tempty       = C0
 tsingleton a = CN a tempty
 C0        >< ys  = ys
 xs        >< C0  = xs
 (CN x q)  >< ys  = CN x (q |> ys)

 tviewl C0        = TAEmptyL
 tviewl (CN x q)  = x :< case tviewl q of
   TAEmptyL -> C0
   t :< q'  -> linkAll t q'
   where
   linkAll :: ToCatQueue q c x y -> q (ToCatQueue q c) y z -> ToCatQueue q c x z
   linkAll t@(CN x q) q' = case tviewl q' of
     TAEmptyL -> t
     h :< t'  -> CN x (q |> linkAll h t')

 tmap phi C0 = C0
 tmap phi (CN c q) = CN (phi c) (tmap (tmap phi) q)

 tfoldMap phi C0 = id
 tfoldMap phi (CN c q) = phi c >>> tfoldMap (tfoldMap phi) q

instance TASequence q => Category (ToCatQueue q c) where
  id = tempty
  (.) = flip (><)
