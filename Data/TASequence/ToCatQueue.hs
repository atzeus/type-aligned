{-# LANGUAGE GADTs #-}



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
-- a catenable queue, i.e. with the same complexity for (><) as for (|>)
-- Based on Purely functional data structures by Chris Okasaki 
-- section 7.2: Catenable lists
--
-----------------------------------------------------------------------------

module Data.TASequence.ToCatQueue(ToCatQueue) where


import Data.TASequence.Class

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
 tviewl (CN h t)  = h :< linkAll t
   where 
    linkAll :: TASequence q =>  q (ToCatQueue q c) a b -> ToCatQueue q c a b
    linkAll v = case tviewl v of
     TAEmptyL     -> C0
     CN x q :< t  -> CN x (q `snoc` linkAll t)
    snoc q C0  = q
    snoc q r   = q |> r
