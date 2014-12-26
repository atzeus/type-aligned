{-# LANGUAGE GADTs, ViewPatterns, TypeOperators #-}


-----------------------------------------------------------------------------
-- |
-- Module      :  Data.TASequence.FastQueue
-- Copyright   :  (c) Atze van der Ploeg 2014
-- License     :  BSD-style
-- Maintainer  :  atzeus@gmail.org
-- Stability   :  provisional
-- Portability :  portable
--
-- A type aligned sequence, a queue, with worst case constant time: '|>', and 'tviewl'.
--
-- Based on: "Simple and Efficient Purely Functional Queues and Deques", Chris Okasaki,
-- Journal of Functional Programming 1995
--
-----------------------------------------------------------------------------

module Data.TASequence.FastQueue(module Data.TASequence, FastQueue) where

import Data.TASequence
import Data.TASequence.ConsList
import Data.TASequence.SnocList


revAppend l r = rotate l r CNil
-- precondtion : |a| = |f| - (|r| - 1)
-- postcondition: |a| = |f| - |r|
rotate :: ConsList tc a b -> SnocList tc b c -> ConsList tc c d -> ConsList tc a d
rotate CNil  (SNil `Snoc` y) r = y `Cons` r
rotate (x `Cons` f) (r `Snoc` y) a = x `Cons` rotate f r (y `Cons` a)
rotate f        a     r  = error "Invariant |a| = |f| - (|r| - 1) broken"

data FastQueue tc a b where
  RQ :: !(ConsList tc a b) -> !(SnocList tc b c) -> !(ConsList tc x b) -> FastQueue tc a c

queue :: ConsList tc a b -> SnocList tc b c -> ConsList tc x b -> FastQueue tc a c
queue f r CNil = let f' = revAppend f r 
                 in RQ f' SNil f'
queue f r (h `Cons` t) = RQ f r t

instance TASequence FastQueue where
 tempty = RQ CNil SNil CNil
 tsingleton x = let c = tsingleton x in queue c SNil c
 (RQ f r a) |> x = queue f (r `Snoc` x) a

 tviewl (RQ CNil SNil CNil) = TAEmptyL
 tviewl (RQ (h `Cons` t) f a) = h :< queue t f a

instance Maps FastQueue where
  maps phi (RQ a b c) = RQ (maps phi a) (maps phi b) (maps phi c)