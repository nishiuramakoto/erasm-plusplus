--    Copyright (C) 2011,2012 Makoto Nishiura.

--    This file is part of ERASM++.

--    ERASM++ is free software; you can redistribute it and/or modify it under
--    the terms of the GNU General Public License as published by the Free
--    Software Foundation; either version 3, or (at your option) any later
--    version.

--    ERASM++ is distributed in the hope that it will be useful, but WITHOUT ANY
--    WARRANTY; without even the implied warranty of MERCHANTABILITY or
--    FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
--    for more details.

--    You should have received a copy of the GNU General Public License
--    along with ERASM++; see the file COPYING3.  If not see
--    <http://www.gnu.org/licenses/>.  

{-# OPTIONS_GHC -F -pgmF preprocess  #-}
--m4_include(my_ghc_testframework.m4)
module Equivalence
       ( Equivalence
       , fromList
       , equivalentBy
       , uniqueBy
       , runTests
       , (<||>)
       )
where
import Test.HUnit
import Test.QuickCheck
import Data.Function
import Data.Map(Map)
import Data.List
import qualified Data.Map as Map

infixl 2 <||>

type Characteristic = Integer
newtype Equivalence a  = EQV (Map a Characteristic)

fromList :: (Ord a,Show a) => [[a]] -> Equivalence a
fromList xss = EQV $ Map.fromListWithKey err $ concat $ zipWith (\xs i -> zip xs $ repeat i) xss [0..]
  where err :: Show a => a -> b
        err x = ERROR(("not disjoint sets " ++ show x ++ "\n" ++show xss ))

equivalentBy :: Ord a => Equivalence a -> a -> a -> Bool
equivalentBy (EQV m)  = (eqJust) `on` flip Map.lookup m

eqJust :: Eq a => Maybe a -> Maybe a -> Bool
eqJust (Just x) (Just y) = x == y
eqJust _ _ = False

uniqueBy :: (a->a->Bool) -> [a] -> Bool
uniqueBy e xs = (not $ null gs) && (null $ tail gs)
  where gs = groupBy e xs


(<||>) :: (a -> a -> Bool) -> (a -> a -> Bool) -> a -> a -> Bool
e <||> e' = \x y -> e x y || e' x y


t1 = fromList [[1,2,3],[3,4,5]] -- Should result in error
t2 = fromList [[1,2,3],[4,5]]


ASSERT_EQ((equivalentBy t2 1 2),(True))
ASSERT_EQ((equivalentBy t2 4 5),(True))
ASSERT_EQ((equivalentBy t2 1 5),(False))
ASSERT_EQ((equivalentBy t2 6 7),(False))
ASSERT_EQ((equivalentBy t2 1 7),(False))


