{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types,GADTs #-}
#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE PolyKinds #-}
#endif




-----------------------------------------------------------------------------
-- |
-- Module      :  Data.TASequence.FingerTree
-- Copyright   :  (c) Atze van der Ploeg 2014
-- License     :  BSD-style
-- Maintainer  :  atzeus@gmail.org
-- Stability   :  provisional
-- Portability :  portable
--
-- A type aligned sequence, a catanable deque, with amortized /O(log n)/  constant time: '><','<|','|>', 'tviewl' and 'tviewr'.
--
-- Based on:  "Finger trees: a simple general-purpose data structure"
--   Ralf Hinze and Ross Paterson. in Journal of Functional Programming16:2 (2006), pages 197-217.
--
-----------------------------------------------------------------------------
module Data.TASequence.FingerTree (module Data.TASequence, FingerTree ) where


import Control.Category
import Data.TASequence


data FingerTree r a b where
  Empty  :: FingerTree r a a
  Single :: r a b -> FingerTree r a b
  Deep   :: !(Digit r a b) -> FingerTree (Node r) b c -> !(Digit r c d) -> FingerTree r a d

data Node r a b where
  Node2 :: r a b -> r b c -> Node r a c
  Node3 :: r a b -> r b c -> r c d -> Node r a d

data Digit r a b where
  One   :: r a b -> Digit r a b
  Two   :: r a b -> r b c -> Digit r a c
  Three :: r a b -> r b c -> r c d -> Digit r a d
  Four  :: r a b -> r b c -> r c d -> r d e -> Digit r a e

instance TASequence FingerTree where
  tempty = Empty
  tsingleton = Single

  Empty                     |> a = Single a
  Single b                  |> a = Deep (One b) Empty (One a)
  Deep pr m (Four e d c b)  |> a = m `seq` Deep pr (m |> Node3 e d c) (Two b a)
  Deep pr m sf              |> a = Deep pr m (appendd sf (One a))

  a <| Empty                     = Single a
  a <| Single b                  = Deep (One a) Empty (One b) 
  a <| Deep (Four b c d e) m sf  = m `seq` Deep (Two a b) (Node3 c d e <| m) sf
  a <| Deep pr m sf              = Deep (appendd (One a) pr) m sf

  tviewl Empty = TAEmptyL
  tviewl (Single a) = a :< Empty
  tviewl (Deep pr m sf) = case toList pr of
              h ::: t -> h :< deepl t m sf

  tviewr Empty = TAEmptyR
  tviewr (Single a) = Empty :> a 
  tviewr (Deep pr m sf) = case toListR sf of
            h :::< t -> deepr pr m t :> h

  xs >< ys = app3 xs ZNil ys

  tmap f Empty = Empty
  tmap f (Single a) = Single (f a)
  tmap f (Deep l m r) = Deep (mapd f l) (tmap (mapn f) m) (mapd f r)

instance Category (FingerTree c) where
  id = tempty
  (.) = flip (><)

toTree :: Digit r a b -> FingerTree r a b
toTree (One a)         = Single a
toTree (Two a b)       = Deep (One a) Empty (One b)
toTree (Three a b c)   = Deep (Two a b) Empty (One c)
toTree (Four a b c d)  = Deep (Two a b) Empty (Two c d)


appendd :: Digit r a b -> Digit r b c -> Digit r a c
appendd (One a)        (One b)        = Two a b
appendd (One a)        (Two b c)      = Three a b c
appendd (Two a b)      (One c)        = Three a b c
appendd (One a)        (Three b c d)  = Four a b c d
appendd (Two a b)      (Two c d)      = Four a b c d
appendd (Three a b c)  (One d)        = Four a b c d






infixr 5 ::: 


data ZList r a b where
  ZNil :: ZList r a a
  (:::) :: r a b -> ZList r b c -> ZList r a c

toList (One a) = a ::: ZNil 
toList (Two a b) = a ::: b ::: ZNil
toList (Three a b c) = a ::: b ::: c ::: ZNil
toList (Four a b c d) = a ::: b ::: c ::: d ::: ZNil




fromList :: ZList r a b -> Digit r a b
fromList (a ::: ZNil) = One a
fromList (a ::: b ::: ZNil) = Two a b
fromList (a ::: b ::: c ::: ZNil) = Three a b c
fromList (a ::: b ::: c ::: d ::: ZNil) = Four a b c d

append :: ZList r a b -> ZList r b c -> ZList r a c
append ZNil t = t
append (h ::: t) r = h ::: append t r


deepl :: ZList r a b -> FingerTree (Node r) b c -> Digit r c d -> FingerTree r a d
deepl ZNil m sf = case tviewl m of
           TAEmptyL -> toTree sf
           a :< m' -> Deep (nodeToDigit a) m' sf 
deepl pr m sf = Deep (fromList pr) m sf

infixr 5 :::< 

data ZListR r a b where
  ZNilR :: ZListR r a a
  (:::<) :: r b c -> ZListR r a b -> ZListR r a c

toListR :: Digit r a b -> ZListR r a b
toListR (One a) = a :::< ZNilR
toListR (Two a b) = b :::< a :::< ZNilR
toListR (Three a b c) = c :::< b :::< a :::< ZNilR
toListR (Four a b c d) = d:::< c :::< b :::< a :::< ZNilR



fromListR :: ZListR r a b -> Digit r a b
fromListR (a :::< ZNilR) = One a
fromListR (b :::< a :::< ZNilR) = Two a b
fromListR (c :::< b :::< a :::< ZNilR) = Three a b c
fromListR (d :::< c :::< b :::< a :::< ZNilR) = Four a b c d


rev = toList Prelude.. fromListR



deepr :: Digit r a b -> FingerTree (Node r) b c -> ZListR r c d -> FingerTree r a d
deepr pr m ZNilR = case tviewr m of
           TAEmptyR -> toTree pr
           m' :> a -> Deep pr m' (nodeToDigit a)
deepr pr m sf = Deep pr m (fromListR sf)


nodeToDigit :: Node r a b -> Digit r a b
nodeToDigit (Node2 a b) = Two a b
nodeToDigit (Node3 a b c) = Three a b c



addAlll :: ZList r a b -> FingerTree r b c -> FingerTree r a c
addAlll ZNil m = m
addAlll (h ::: t) m = h <| addAlll t m

addAllr :: FingerTree r a b -> ZList r b c  -> FingerTree r a c
addAllr m ZNil  = m
addAllr m (h ::: t) = addAllr (m |> h) t

  

app3 :: FingerTree r a b -> ZList r b c -> FingerTree r c d -> FingerTree r a d
app3 Empty ts xs = addAlll ts xs
app3 xs ts Empty = addAllr xs ts
app3 (Single x) ts xs = x <| (addAlll ts xs)
app3 xs ts (Single x) = (addAllr xs ts) |> x
app3 (Deep pr1 m1 sf1) ts (Deep pr2 m2 sf2) =
    Deep pr1 
        (app3 m1 (nodes (append (toList sf1) (append ts (toList pr2)))) m2) sf2


nodes :: ZList r a b -> ZList (Node r) a b
nodes (a ::: b ::: ZNil) = Node2 a b ::: ZNil
nodes (a ::: b ::: c ::: ZNil) = Node3 a b c ::: ZNil
nodes (a ::: b ::: c ::: d ::: ZNil) = Node2 a b ::: Node2 c d ::: ZNil
nodes (a ::: b ::: c ::: xs) = Node3 a b c ::: nodes xs

mapn :: (forall x y. c x y -> d x y) -> Node c x y -> Node d x y
mapn phi (Node2 r s) = Node2 (phi r) (phi s)
mapn phi (Node3 r s t) = Node3 (phi r) (phi s) (phi t)
  
mapd :: (forall x y. c x y -> d x y) -> Digit c x y -> Digit d x y
mapd phi (One r) = One (phi r) 
mapd phi (Two r s) = Two (phi r) (phi s)
mapd phi (Three r s t) = Three (phi r) (phi s) (phi t)
mapd phi (Four r s t u) = Four (phi r) (phi s) (phi t) (phi u)


