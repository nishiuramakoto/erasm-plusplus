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
--    along with ERASM++; see the file COPYING.  If not see
--    <http://www.gnu.org/licenses/>.  

{-# OPTIONS_GHC -F -pgmF preprocess  #-}
--m4_include(my_ghc_testframework.m4)
module ErasmCommon( Mode(..)
                 , Byte
                 , OperandSize(..)
                 , AddressSize(..)
                 , Doc
                 , Pretty(..)
                 , (<>)
                 , (<+>)
                 , (<$>)
                 , (<$$>)
                 , (@?=)
                 , on
                 , the
                 , uniq
                 , uniqBy
                 , equivalenceClasses
                 , cartesianProduct
                 , cartesianProductBy
                 , distinct
                 , mutually
                 , combi2
                 , Named , name
                 , (-->) , (<-->)
                 , toInt
                 , prettyAssertPostCondition
                 , assertPostCondition
                 , ErasmCommon.runTests
                 , module Control.Monad
                 , module Data.Char
                 , module Data.List
                 , module Data.Maybe
                 , module Combinators
                 ) where

import qualified Test.HUnit
import Test.HUnit((@?=))
import Test.QuickCheck hiding((><))
import Test.QuickCheck.Gen

import UU.PPrint(Doc,Pretty(..),(<>),(<+>),(<$$>),(<$>))

import Control.Monad 
import Data.Char
import Data.Word
import Data.List
import Data.Maybe
import Data.Function
import Combinators hiding(and,or,(<>),(!))

infixr 0 --> , <-->
(-->)   :: Bool -> Bool -> Bool
a --> b = not a || b

(<-->)   :: Bool -> Bool -> Bool
a <--> b = a == b


toInt :: (Num a , Integral a) => a -> Int
toInt = fromInteger . toInteger


data Mode = X64 | X86
          deriving(Eq,Ord,Enum,Bounded,Show)

instance Arbitrary Mode where
    arbitrary = elements [minBound..maxBound]

type Byte = Word8                      
data OperandSize = Op16 | Op32 | Op64
          deriving(Eq,Ord,Enum,Bounded,Show)

instance Arbitrary OperandSize where
    arbitrary = elements [minBound..maxBound]

data AddressSize = Addr16 | Addr32 | Addr64 
          deriving(Eq,Ord,Enum,Bounded,Show)                         

instance Arbitrary AddressSize where
    arbitrary = elements [minBound..maxBound]


class Named a where
  name :: a -> String
  

the :: (Eq a) => [a] -> a
the (x:xs)
  | all (x==) xs = x
  | otherwise    = ERROR((show "list does not consist of unique element"))
the [] = ERROR(("empty list"))                   


equivalenceClasses :: (a -> a -> Bool) -> [a] -> [[a]]
equivalenceClasses r [] = []
equivalenceClasses r (x:xs) = 
  let (ys,zs) = partition (r x) xs
  in
   (x:ys) : equivalenceClasses r zs
   
ASSERT_EQ((equivalenceClasses (==) [1,2,1,3,4]),([[1,1],[2],[3],[4]]))   

ASSERT_EQ((uniq [1,1,2,3,4,4]),([1,2,3,4]))

uniq :: Eq a => [a] -> [a]
uniq xs = map head $ group xs

uniqBy :: (a -> a -> Bool) ->  [a] -> [a]
uniqBy cmp xs = map head $ groupBy cmp xs


ASSERT_EQ((cartesianProductBy (+) [1,2] [3,4]),([4,5,5,6]))
cartesianProduct :: [a] -> [b] -> [(a,b)]
cartesianProduct xs ys = [(x,y) | x <- xs , y <- ys ]

cartesianProductBy :: (a -> b -> c) -> [a] -> [b] -> [c]
cartesianProductBy f xs ys = [f x y | x<- xs , y <- ys]


distinct :: Ord a => [a] -> Bool
distinct xs = length xs == length (uniq $ sort xs )


ASSERT_EQ((distinct [3,4,5,2]),(True))
ASSERT_EQ((distinct [3,4,3,2]),(False))



mutually :: (a -> a -> Bool) -> [a] -> Bool
mutually p xs = all (uncurry p) $ combi2 xs

combi2 :: [a] -> [(a,a)]
combi2 [] = []
combi2 [x] = []
combi2 (x:xs) = [(x,y) | y <- xs ] ++ combi2 xs


assertPostCondition :: (a -> Bool) -> a -> a
assertPostCondition c x 
    | c x = x
    | otherwise = ERROR(("check failed"))

prettyAssertPostCondition :: Pretty a => String -> (a -> Bool) -> a -> a
prettyAssertPostCondition info c x 
    | c x = x
    | otherwise = ERROR((show $ 
                              pretty "check failed" <$> 
                              pretty info <$> 
                              pretty x ))

