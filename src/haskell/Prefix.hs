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
module Prefix ( Prefix(..)
              , Prefix.Set  
              , code
              , isPrefix
              , fromList
              , fromCode
              , fromCodeList
              , toCodeList      
              , toList
              , hasRex
              , isRex
              , insert
              , empty
              , singleton
              , member
              , null
              , union
              , unions
              , intersection
              , difference
              , rex , rexw , addr , op , rep ,repe,repz,repne,repnz 
              , isMandatoryPrefix
              , isSubsetOf
              , isProperSubsetOf
              , Prefix.runTests
              )
       where
import Prelude hiding(null)
import qualified Test.HUnit
import Test.QuickCheck
import Control.Monad
import ErasmCommon(Byte,(@?=))
import qualified Data.Set as Set

import Data.Map(Map)
import qualified Data.Map as Map
import Data.Function

newtype Set = PSet { pset :: Set.Set Prefix }
              deriving (Eq,Ord,Show)

instance Arbitrary Set where
    arbitrary = liftM PSet $ liftM Set.fromList arbitrary


data Prefix = LOCK  -- F0
            | REPNE -- F2
            | REP   -- F3
            | CS    -- 2E
            | SS    -- 36
            | DS    -- 3E
            | ES    -- 26
            | FS    -- 64
            | GS    -- 65
            | NOT_BRANCH -- 2E
            | BRANCH -- 3E
            | OPERAND_SIZE -- 66
            | ADDRESS_SIZE -- 67
            | Rex -- 40
            | RexW 
            | RexR
            | RexX
            | RexB
            deriving (Eq,Ord,Enum,Bounded,Show)

instance Arbitrary Prefix where
    arbitrary = elements [minBound .. maxBound ]

                     
isMandatoryPrefix :: Prefix -> Bool
isMandatoryPrefix p = elem p [ REPNE ,REP , OPERAND_SIZE ]


prefixList :: [ (Prefix,Byte) ]
prefixList =  [ ( LOCK , 0xF0)
              , ( REPNE, 0xF2)
              , ( REP  ,  0xF3)
              , ( CS   ,  0x2E)
              , ( SS   ,  0x36)
              , ( DS   ,  0x3E)
              , ( ES   ,  0x26)
              , ( FS   ,  0x64)
              , ( GS   ,  0x65)
              , ( NOT_BRANCH,  0x2E)
              , ( BRANCH,  0x3E)
              , ( OPERAND_SIZE,  0x66)
              , ( ADDRESS_SIZE,  0x67) ]

prefixByteSet :: Set.Set Byte
prefixByteSet = Set.fromList $ map snd prefixList

prefixToCodeMap :: Map Prefix Byte
prefixToCodeMap = Map.fromList prefixList

codeToPrefixMap :: Map Byte Prefix
codeToPrefixMap = Map.fromList $ Prelude.map swap prefixList

isPrefix :: Byte -> Bool
isPrefix b = Set.member b prefixByteSet

swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

code :: Prefix -> Maybe Byte 
code p = Map.lookup p prefixToCodeMap                     

fromCode :: Byte -> Maybe Prefix
fromCode x = Map.lookup x codeToPrefixMap

fromCodeList :: [ Byte ] -> Prefix.Set
fromCodeList cs = PSet $ Set.fromList $ Prelude.map (fromJustE . fromCode) cs
    where fromJustE (Just x) = x
          fromJustE Nothing  = ERROR((show cs))

toCodeList :: Prefix.Set -> [ Byte ]
toCodeList (PSet s) = Prelude.map (fromJustE . code) $ Set.toList s
    where fromJustE (Just x) = x
          fromJustE Nothing  = ERROR((show s))

toList :: Prefix.Set -> [ Prefix ]
toList s = Set.toList $ pset s

fromList :: [Prefix] -> Prefix.Set
fromList = PSet . Set.fromList

rexSet :: Set.Set Prefix
rexSet = Set.fromList [ Rex , RexW , RexR, RexX, RexB ]

isRex :: Prefix -> Bool
isRex x = Set.member x rexSet
  
hasRex :: Prefix.Set -> Bool
hasRex (PSet s) = not $ Set.null $ Set.intersection s rexSet

insert :: Prefix -> Prefix.Set -> Prefix.Set
insert x  = PSet .  Set.insert x . pset

empty :: Prefix.Set 
empty = PSet Set.empty

singleton :: Prefix -> Prefix.Set
singleton x = PSet $ Set.singleton x

member :: Prefix -> Prefix.Set -> Bool
member x s = Set.member x $ pset s 


null :: Prefix.Set -> Bool
null = Set.null . pset

union :: Prefix.Set -> Prefix.Set -> Prefix.Set
union  x y = PSet $ (Set.union `on` pset) x y

unions :: [Prefix.Set] -> Prefix.Set
unions  xs  = PSet $ Set.unions $ map pset xs


intersection :: Prefix.Set -> Prefix.Set -> Prefix.Set
intersection x y = PSet $ (Set.intersection `on` pset) x y

difference :: Prefix.Set -> Prefix.Set -> Prefix.Set
difference x y = PSet $ (Set.difference `on` pset) x y

rex,rexw,addr,op,rep,repe,repz,repne,repnz :: Set
rex    = singleton Rex
rexw   = singleton RexW
addr   = singleton ADDRESS_SIZE
op     = singleton OPERAND_SIZE
rep    = singleton REP
repe   = rep
repz   = rep
repne  = singleton REPNE
repnz  = repne


ASSERT_EQ((isSubsetOf empty rex),(True))
ASSERT_EQ((isProperSubsetOf empty rex),(True))
ASSERT_EQ((isProperSubsetOf rex rex),(False))

isSubsetOf :: Prefix.Set -> Prefix.Set -> Bool
isSubsetOf = Set.isSubsetOf `on` pset

isProperSubsetOf :: Prefix.Set -> Prefix.Set -> Bool
isProperSubsetOf = Set.isProperSubsetOf `on`  pset
