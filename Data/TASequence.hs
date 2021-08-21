{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs,TypeSynonymInstances,FlexibleInstances,Rank2Types, KindSignatures #-}
#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE PolyKinds #-}
#endif


-----------------------------------------------------------------------------
-- |
-- Module      :  Data.TASequence
-- Copyright   :  (c) Atze van der Ploeg 2014
-- License     :  BSD-style
-- Maintainer  :  atzeus@gmail.org
-- Stability   :  provisional
-- Portability :  portable
-- A type class for type aligned sequences: heterogeneous
-- sequences where the types enforce the element order.
--
-- Type aligned sequences are best explained by an example: a type
-- aligned sequence of functions is a sequence f 1 , f 2 , f 3 ... f n such that
-- the composition of these functions f 1 ◦ f 2 ◦ f 3 ◦ ... ◦ f n is well typed.
-- In other words: the result type of each function in the sequence
-- must be the same as the argument type of the next function (if any).
-- In general, the elements of a type aligned sequence do not have to
-- be functions, i.e. values of type a → b, but can be values of type
-- (c a b), for some binary type constructor c. Hence, we define a type
-- aligned sequence to be a sequence of elements of the type (c a_i b_i )
-- with the side-condition b_i−1 = a_i . If s is the type of a type aligned
-- sequence data structure, then (s c a b) is the type of a type aligned
-- sequence where the first element has type (c a x), for some x, and
-- the last element has type (c y b), for some y.
--
-- The simplest type aligned sequence data structure is a list, see "Data.TASequence.ConsList". The other modules
-- give various other type aligned sequence data structures. The data structure "Data.TASequence.FastCatQueue" supports the most operations in worst case constant time.
--
--
-- See the paper Reflection without Remorse: Revealing a hidden sequence to speed up Monadic Reflection, Atze van der Ploeg and Oleg Kiselyov, Haskell Symposium 2014
-- for more details.
-- 
-- Paper: <http://homepages.cwi.nl/~ploeg/zseq.pdf>
-- Talk : <http://www.youtube.com/watch?v=_XoI65Rxmss>
-----------------------------------------------------------------------------
module Data.TASequence(TASequence(..), TAViewL(..), TAViewR(..)) where

import Control.Category
import Prelude hiding ((.),id)

infixr 5 <|
infixl 5 |>
infix 5 ><
{- | A type class for type aligned sequences
 
Minimal complete defention: 'tempty' and 'tsingleton' and ('tviewl' or 'tviewr') and ('><' or '|>' or '<|')

Instances should satisfy the following laws:

Category laws:

> tempty >< x == x
> x >< tempty == x
> (x >< y) >< z = x >< (y >< z)

Observation laws:

> tviewl (tsingleton e >< s) == e :< s
> tviewl tempty == TAEmptyL

The behaviour of '<|','|>', 'tmap' and 'tviewr' is implied by the above laws and their default definitions.
-}
#if __GLASGOW_HASKELL__ >= 706
class TASequence (s :: (k -> k -> *) -> k -> k -> *) where
#else
class TASequence s where
#endif

  {-# MINIMAL tempty, tsingleton, (tviewl | tviewr), ((><) | (|>) | (<|)) #-}

  tempty     :: s c x x
  tsingleton :: c x y -> s c x y
  -- | Append two type aligned sequences
  (><)       :: s c x y -> s c y z  -> s c x z
  -- | View a type aligned sequence from the left
  tviewl     :: s c x y -> TAViewL s c x y
  -- | View a type aligned sequence from the right
  --       
  -- Default definition:
  -- 
  -- > tviewr q = case tviewl q of 
  -- >   TAEmptyL -> TAEmptyR
  -- >   h :< t -> case tviewr t of
  -- >        TAEmptyR -> tempty   :> h
  -- >        p :> l   -> (h <| p) :> l
  tviewr     :: s c x y -> TAViewR s c x y
  -- | Append a single element to the right
  --
  -- Default definition:
  -- 
  -- > l |> r = l >< tsingleton r

  (|>)       :: s c x y -> c y z -> s c x z
  -- | Append a single element to the left
  -- 
  -- Default definition:
  --
  -- > l <| r = tsingleton l >< r

  (<|)       :: c x y -> s c y z -> s c x z
  -- | Apply a function to all elements in a type aligned sequence
  -- 
  -- Default definition:
  -- 
  -- > tmap f q = case tviewl q of
  -- >    TAEmptyL -> tempty
  -- >    h :< t -> f h <| tmap f t
  tmap       :: (forall x y. c x y -> d x y) -> s c x y -> s d x y

  -- | Apply a function to all elements in a type aligned sequence, and combine them using a 'Category' instance.
  --
  -- Default definition:
  --
  -- > tfoldMap f q = case tviewl q of
  -- >   TAEmptyL -> id
  -- >   h :< t -> f h >>> tfoldMap f t
  tfoldMap   :: Category d => (forall x y. c x y -> d x y) -> s c x y -> d x y

  -- | Combine all elements in a type aligned sequence using a 'Category' instance.
  --
  -- Default definition:
  --
  -- > tfold = tfoldMap id
  tfold      :: Category c => s c x y -> c x y

  -- | Right-associative fold of a type aligned sequence.
  tfoldr     :: (forall x y z . c x y -> d y z -> d x z) -> d q r -> s c p q -> d p r

  -- | Left-associative fold of a type aligned sequence.
  tfoldl     :: (forall x y z . d x y -> c y z -> d x z) -> d p q -> s c q r -> d p r

  l |> r = l >< tsingleton r
  l <| r = tsingleton l >< r
  l >< r = case tviewl l of
    TAEmptyL -> r
    h :< t  -> h <| (t >< r)

  tviewl q = case tviewr q of 
    TAEmptyR -> TAEmptyL
    p :> l -> case tviewl p of
        TAEmptyL -> l :< tempty
        h :< t   -> h :< (t |> l)

  tviewr q = case tviewl q of 
    TAEmptyL -> TAEmptyR
    h :< t -> case tviewr t of
        TAEmptyR -> tempty   :> h
        p :> l   -> (h <| p) :> l

  tmap f q = case tviewl q of
    TAEmptyL -> tempty
    h :< t -> f h <| tmap f t

  tfoldMap f q = case tviewl q of
    TAEmptyL -> id
    h :< t -> f h >>> tfoldMap f t

  tfold = tfoldMap id

  tfoldr f z t = appEndoR (tfoldMap (\ x -> EndoR (\ y -> f x y)) t) z

  tfoldl f z t = appEndoL (tfoldMap (\ x -> EndoL (\ y -> f y x)) t) z

data TAViewL s c x y where
   TAEmptyL  :: TAViewL s c x x
   (:<)     :: c x y -> s c y z -> TAViewL s c x z

data TAViewR s c x y where
   TAEmptyR  :: TAViewR s c x x
   (:>)     :: s c x y -> c y z -> TAViewR s c x z


-- Approach adapted from Joachim Breitner: https://stackoverflow.com/a/30986119/88018
newtype EndoR h c d = EndoR { appEndoR :: forall b. h d b -> h c b  }

instance Category (EndoR h) where
  id = EndoR id
  EndoR f1 . EndoR f2 = EndoR (f2 . f1)

newtype EndoL h c d = EndoL { appEndoL :: forall b . h b c -> h b d }

instance Category (EndoL h) where
  id = EndoL id
  EndoL f1 . EndoL f2 = EndoL (f1 . f2)
