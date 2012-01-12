-----------------------------------------------------------------------------
-- |
-- Module      :  Generics.Pointless.Combinators
-- Copyright   :  (c) 2008 University of Minho
-- License     :  BSD3
--
-- Maintainer  :  hpacheco@di.uminho.pt
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Pointless Haskell:
-- point-free programming with recursion patterns as hylomorphisms
-- 
-- This module defines many standard combinators used for point-free programming.
--
-----------------------------------------------------------------------------

-- pointless-haskell doesn't compile for ghc 7.2,so...
{-# LANGUAGE EmptyDataDecls #-}

--module Generics.Pointless.Combinators where
module Combinators where

import Prelude hiding (or,and)
import Data.Function(on)

-- * Terminal object

-- | The bottom value for any type.
-- It is many times used just for type annotations.
_L :: a
_L = undefined

-- | The final object.
-- The only possible value of type 'One' is '_L'.
data One

instance Show One where
    show _ = "_L"

instance Eq One where
    (==) _ _ = True

-- * Points

-- | Creates a point to the terminal object.
bang :: a -> One
bang = const _L

-- | Converts elements into points.
pnt :: a -> One -> a
pnt = const

-- * Products

infix 6  /\
-- | The infix split combinator.
(/\) :: (a -> b) -> (a -> c) -> a -> (b,c)
(/\) f g x = (f x, g x)

infix 7  ><
-- The infix product combinator.
(><) :: (a -> b) -> (c -> d) -> (a,c) -> (b,d)
f >< g = f . fst /\ g . snd

-- * Sums

-- | Injects a value to the left of a sum.
inl :: a -> Either a b
inl = Left

-- | Injects a value to the right of a sum.
inr :: b -> Either a b
inr = Right

infix 4 \/
-- | The infix either combinator.
(\/) :: (b -> a) -> (c -> a) -> Either b c -> a
(\/) = either

infix 5 -|-
-- | The infix sum combinator.
(-|-) :: (a -> b) -> (c -> d) -> Either a c -> Either b d
f -|- g = inl . f \/ inr . g

infix 5 <>
-- | Alias for the infix sum combinator.
(<>) :: (a -> b) -> (c -> d) -> Either a c -> Either b d
(<>) = (-|-)

-- * Exponentials

-- | The application combinator.
app :: (a -> b,a) -> b
app (f,x) = f x

-- | The left exponentiation combinator.
lexp :: (a -> b) -> (b -> c) -> (a -> c)
lexp f = curry (app . (id >< f))

-- | The right exponentiation combinator.
rexp :: (b -> c) -> (a -> b) -> (a -> c)
rexp f = curry (f . app)

infix 0 !
-- | The infix combinator for a constant point.
(!) :: a -> b -> a
(!) = const
 
-- * Guards

-- | Guard combinator that operates on Haskell booleans.
grd :: (a -> Bool) -> a -> Either a a
grd p x = if p x then inl x else inr x

-- | Infix guard combinator that simulates the postfix syntax.
(?) :: (a -> Bool) -> a -> Either a a
(?) = grd

infix 1 ??
(??) :: (a -> Either One One) -> a -> Either a a
(??) p = (snd -|- snd) . distl . (p /\ id)
-- * Point-free definitions of uncurried versions of the basic combinators

-- | The uncurried split combinator.
split :: (a -> b, a -> c) -> (a -> (b,c))
split = curry ((app >< app) . ((fst >< id) /\ (snd >< id)))

-- | The uncurried either combinator.
eithr :: (a -> c, b -> c) -> Either a b -> c
eithr = curry ((app \/ app) . (fst >< id -|- snd >< id) . distr)

-- | The uncurried composition combinator.
comp :: (b -> c, a -> b) -> (a -> c)
comp = curry (app . (id >< app) . assocr)

-- | Binary @or@ of boolean functions.
orf :: (a -> Bool,a -> Bool) -> (a -> Bool)
orf = curry (or . (app . (fst >< id) /\ app . (snd >< id)))

-- | Binary @and@ of boolean functions.
andf :: (a -> Bool,a -> Bool) -> (a -> Bool)
andf = curry (and . (app . (fst >< id) /\ app . (snd >< id)))


-- | Binary @or@ point-free combinator.
or :: (Bool,Bool) -> Bool
or = uncurry (||)

-- | Binary @and@ point-free combinator.
and :: (Bool,Bool) -> Bool
and = uncurry (&&)

-- | Binary equality point-free combinator.
eq :: Eq a => (a,a) -> Bool
eq = uncurry (==)

-- | Binary inequality point-free combinator.
neq :: Eq a => (a,a) -> Bool
neq = not . eq

-- * Point-free isomorphic combinators

-- | Swap the elements of a product.
swap :: (a,b) -> (b,a)
swap = snd /\ fst

-- | Swap the elements of a sum.
coswap :: Either a b -> Either b a
coswap = inr \/ inl

-- | Distribute products over the left of sums.
distl :: (Either a b,c) -> Either (a,c) (b,c)
distl = app . ((curry inl \/ curry inr) >< id)

-- | Distribute sums over the left of products.
undistl :: Either (a,c) (b,c) -> (Either a b, c)
undistl = inl >< id \/ inr >< id

-- | Distribute products over the right of sums.
distr :: (c, Either a b) -> Either (c,a) (c,b)
distr = (swap -|- swap) . distl . swap

-- | Distribute sums over the right of products.
undistr :: Either (c,a) (c,b) -> (c, Either a b)
undistr = (id >< inl) \/ (id >< inr)

-- | Associate nested products to the left.
assocl :: (a,(b,c)) -> ((a,b),c)
assocl = (id >< fst) /\ snd . snd

-- | Associates nested products to the right.
assocr :: ((a,b),c) -> (a,(b,c))
assocr = fst . fst  /\  (snd >< id)

-- | Associates nested sums to the left.
coassocl :: Either a (Either b c) -> Either (Either a b) c
coassocl = (inl . inl) \/ (inr -|- id)

-- | Associates nested sums to the right.
coassocr :: Either (Either a b) c -> Either a (Either b c)
coassocr = (id -|- inl) \/ (inr . inr)

-- | Shifts the an element to the right of a nested pair.
subr :: (a,(b,c)) -> (b,(a,c))
subr = assocr . (swap >< id) . assocl

-- | Shifts the an element to the left of a nested pair.
subl :: ((a,b),c) -> ((a,c),b)
subl = assocl . (id >< swap) . assocr

-- | Shifts an option to the right of a nested sum.
cosubr :: Either a (Either b c) -> Either b (Either a c)
cosubr = coassocr . (coswap -|- id) . coassocl

-- | Shifts an option to the left of a nested sum.
cosubl :: Either (Either a b) c -> Either (Either a c) b
cosubl = coassocl . (id -|- coswap) . coassocr

-- | The product distribution combinator
distp :: ((c,d),(a,b)) -> ((c,a),(d,b))
distp = fst >< fst /\ snd >< snd

-- | The sum distribution combinator.
dists :: (Either a b,Either c d) -> Either (Either (a,c) (a,d)) (Either (b,c) (b,d))
dists = (distr -|- distr) . distl

-- Curried variants,these are quite handy
orF = curry orf
andF = curry andf

orF2 :: (a -> b -> Bool) -> (a -> b -> Bool) -> (a -> b -> Bool)
orF2 f g x y = orF (uncurry f) (uncurry g) (x,y)
prop_orF2 x y  = (orF2 (<) (>)) x y == (x < y || x > y)

andF2 :: (a -> b -> Bool) -> (a -> b -> Bool) -> (a -> b -> Bool)
andF2 f g x y = andF (uncurry f) (uncurry g) (x,y)
prop_andF2 x y = (andF2 (<) (>)) x y == (x < y && x > y)
