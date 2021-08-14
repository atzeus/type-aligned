{-# LANGUAGE PolyKinds #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.TASequence.FastCatQueue
-- Copyright   :  (c) Atze van der Ploeg 2014
-- License     :  BSD-style
-- Maintainer  :  atzeus@gmail.org
-- Stability   :  provisional
-- Portability :  portable
--
-- A type aligned sequence, a catanable queue, with worst case constant time: '><', '|>', '<|' and 'tviewl'.
--
-----------------------------------------------------------------------------
module Data.TASequence.FastCatQueue(module Data.TASequence, FastTCQueue) where

import Data.TASequence
import Data.TASequence.FastQueue
import Data.TASequence.ToCatQueue

type FastTCQueue =  ToCatQueue FastQueue
