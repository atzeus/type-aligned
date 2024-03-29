{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types,GADTs, DataKinds, TypeOperators #-}
#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE PolyKinds #-}
#endif


-----------------------------------------------------------------------------
-- |
-- Module      :  Data.TASequence.Queue
-- Copyright   :  (c) Atze van der Ploeg 2014
-- License     :  BSD-style
-- Maintainer  :  atzeus@gmail.org
-- Stability   :  provisional
-- Portability :  portable
--
-- A type aligned sequence, a queue, with amortized constant time: '|>', and 'tviewl'.
--
-- A simplified version of Okasaki's implicit recursive
-- slowdown queues. 
-- See purely functional data structures by Chris Okasaki 
-- section 8.4: Queues based on implicit recursive slowdown
--
-----------------------------------------------------------------------------
module Data.TASequence.Queue(module Data.TASequence,Queue)  where

import Control.Category
import Data.TASequence

data P c a b where
  (:*) :: c a w -> c w b -> P c a b

data B c a b where
  B1 :: c a b    -> B c a b
  B2 :: !(P c a b)  -> B c a b

data Queue c a b where
  Q0 :: Queue c a a
  Q1 :: c a b -> Queue c a b
  QN :: !(B c a x) -> Queue (P c) x y -> !(B c y b) -> Queue c a b

instance TASequence Queue where
  tempty = Q0
  tsingleton = Q1 
  q |> b = case q of
    Q0             -> Q1 b
    Q1 a           -> QN (B1 a) Q0 (B1 b)
    QN l m (B1 a)  -> QN l m (B2 (a :* b)) 
    QN l m (B2 r)  -> QN l (m |> r) (B1 b)

  tviewl q = case q of
    Q0                    -> TAEmptyL
    Q1 a                  -> a :< Q0
    QN (B2 (a :* b)) m r  -> a :< QN (B1 b) m r
    QN (B1 a) m r         -> a :< shiftLeft m r
    where  shiftLeft :: Queue (P c) a w -> B c w b -> Queue c a b
           shiftLeft q r = case tviewl q of
               TAEmptyL -> buf2queue r
               l :< m -> QN (B2 l) m r
           buf2queue (B1 a)        = Q1 a
           buf2queue(B2 (a :* b))  = QN (B1 a) Q0 (B1 b)
  tmap f Q0 = Q0
  tmap f (Q1 x) = Q1 (f x)
  tmap f (QN l m r) = QN (tmapb f l) (tmap (tmapp f) m) (tmapb f r)

instance Category (Queue c) where
  id = tempty
  (.) = flip (><)

tmapp :: (forall x y. c x y -> d x y) -> P c x y -> P d x y
tmapp phi (a :* b) = phi a :* phi b
  
tmapb :: (forall x y. c x y -> d x y) -> B c x y -> B d x y
tmapb phi (B1 c) = B1 (phi c)
tmapb phi (B2 p) = B2 (tmapp phi p)
