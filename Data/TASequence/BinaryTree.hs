{-# LANGUAGE GADTs #-}



-----------------------------------------------------------------------------
-- |
-- Module      :  Data.TASequence.BinaryTree
-- Copyright   :  (c) Atze van der Ploeg 2014
-- License     :  BSD-style
-- Maintainer  :  atzeus@gmail.org
-- Stability   :  provisional
-- Portability :  portable
--
-- A type aligned sequence which uses a binary tree, where the leaves are
-- elements and then nodes are '><'.
--
-----------------------------------------------------------------------------
module Data.TASequence.BinaryTree(module Data.TASequence, BinaryTree) where

import  Data.TASequence

data BinaryTree c x y where
  Empty :: BinaryTree c x x
  Leaf  :: c x y -> BinaryTree c x y
  Node  :: BinaryTree c x y -> BinaryTree c y z -> BinaryTree c x z

instance TASequence BinaryTree where
  tempty = Empty
  tsingleton c = Leaf c 
  (><) = Node
  tviewl Empty = TAEmptyL
  tviewl (Leaf c) = c :< Empty
  tviewl (Node (Node l m) r) = tviewl (Node l (Node m r))
  tviewl (Node (Leaf c) r)   = c :< r
  tviewl (Node Empty r)      = tviewl r
                        
instance Maps BinaryTree where
  maps phi Empty = Empty
  maps phi (Leaf c) = Leaf (phi c)
  maps phi (Node b b') = Node (maps phi b) (maps phi b')