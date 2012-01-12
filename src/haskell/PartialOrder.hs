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
{-# LANGUAGE FlexibleInstances,UndecidableInstances #-}
--m4_include(my_ghc_testframework.m4)
module PartialOrder
       ( PartialOrdering
       , PartialOrd(..)
       , propPartialOrdWellDefined
       , propPartialOrd
       , propReflexive
       , propTransitive
       , propAntiSymmetric
       , propMeetSemiLattice
       , cmpBy
       , leBy
       , leByCmp
       , geBy
       , geByCmp
       , minimumBy
       , maximumBy
       , totallyOrdered
       , totallyOrderedBy
       , co
       , dual
       , RelationGraph
       , makePreorderGraph
       , makeRelationGraph
       , insertBottom
       , root
       , meet
       , PartialOrder.runTests
       )
where       
  

import ErasmCommon hiding(minimum,maximum,minimumBy,maximumBy)
import Data.Graph.Inductive
import qualified Data.Graph.Inductive as Graph
import Combinators hiding(and,or)

import Data.Maybe
import Prelude hiding(minimum,maximum)
import Test.HUnit
import Test.QuickCheck


-- The most important instance of partial ordering
import qualified Data.Set as Set
import qualified Data.List


type PartialOrdering = Maybe Ordering  

-- The same precedence as prelude's ==,<,etc.
infix 4 `lt` , `le` , `gt` , `ge`

class Eq a => PartialOrd a where
    -- This definition is correct,but
    cmp :: a -> a -> Maybe Ordering 
    cmp x y = if x == y then Just EQ
              else if x `le` y then Just LT
                   else if x `ge` y then Just GT
                        else Nothing

    -- this should be a boolean predicate.
    lt  :: a -> a -> Bool
    lt x y = cmp x y == Just LT

    le  :: a -> a -> Bool
    le x y = x == y || x `lt` y
    -- The following code can be faster,but looks too ugly
    -- case cmp x y of {Just LT -> True;Just EQ -> True;_ -> False }

    gt  :: a -> a -> Bool
    gt  x y = cmp x y == Just GT

    ge  :: a -> a -> Bool
    ge x y = x == y || x `gt` y

instance Ord a => PartialOrd a where
    cmp = curry $ Just . uncurry compare


propPartialOrdWellDefined :: PartialOrd a => a -> a -> Bool
propPartialOrdWellDefined x y
    = (x `le` y) == (y `ge` x) &&
      (x `lt` y) == (y `gt` x) 


-- A reflexive,transitive relation is a preorder
propReflexive :: (a -> a -> Bool) ->  a -> Bool
propReflexive le x = x `le` x

propTransitive :: (a -> a -> Bool) ->  a -> a -> a -> Bool
propTransitive le x y z 
    = (x `le` y) && (y `le` z) --> (x `le` z)

-- Furthermore,a preorder is a partial order if it is anti-symmetric
propAntiSymmetric :: Eq a =>(a -> a -> Bool) -> a -> a -> Bool
propAntiSymmetric le x y =
    x `le` y && y `le` x --> x == y

propPartialOrd ::Eq a => (a -> a -> Bool) -> a -> a -> a -> Bool
propPartialOrd le x y z = propReflexive le x &&
                          propTransitive le x y z &&
                          propAntiSymmetric le x y


prop_minimum :: Ord a => a -> [a] -> Bool
prop_minimum x xs = minimum xs' == Just (Data.List.minimum xs')
    where xs' = x:xs
prop_maximum :: Ord a => a -> [a] -> Bool
prop_maximum x xs = maximum xs' == Just (Data.List.maximum xs')
    where xs' = x:xs


check :: IO ()
check = quickCheck (prop_minimum :: Int -> [Int] -> Bool)

minimum :: (PartialOrd a) => [a] -> Maybe a
minimum  = minimumBy le

maximum :: (PartialOrd a) => [a] -> Maybe a
maximum  = maximumBy ge


-- I really hate Prelude's Ord a => a -> a -> Ordering interface 
-- because that sacrifices clarity and generality for some possible
-- (and usually minor) speed up which is so often not what we want.
-- The following code,for example,is perfectly applicable and gives valid 
-- results both for partial orders and total orders at the same time.

minimumBy  :: (a -> a -> Bool) -> [a] -> Maybe a
minimumBy  le xs = checkMin le (foldl minBy Nothing xs) xs
    where
      minBy Nothing  y             = Just y
      minBy (Just x) y | y `le` x  = Just y
                       | otherwise = Just x

checkMin :: (a -> a -> Bool) ->  Maybe a -> [a] -> Maybe a
checkMin _  Nothing  _                    = Nothing
checkMin le (Just x) xs | all (x `le`) xs = Just x
                        | otherwise       = Nothing

propMinimumBy :: Ord a => NonEmptyList a -> Bool
propMinimumBy (NonEmpty xs) = minimumBy (<=) xs == Just (Data.List.minimum xs)
QUICKCHECK((propMinimumBy :: NonEmptyList Int -> Bool))

maximumBy  :: (a -> a -> Bool) -> [a] -> Maybe a
maximumBy ge xs = minimumBy ge xs

propMaximumBy :: Ord a => NonEmptyList a -> Bool
propMaximumBy (NonEmpty xs) = maximumBy (>=) xs == Just (Data.List.maximum xs)
QUICKCHECK((propMaximumBy :: NonEmptyList Int -> Bool))

co :: (a -> a -> PartialOrdering) -> (a -> a -> PartialOrdering)
co cmp x y = dual $ cmp x y

dual :: Maybe Ordering -> Maybe Ordering
dual (Just LT) = Just GT
dual (Just EQ) = Just EQ
dual (Just GT) = Just LT
dual Nothing   = Nothing       


totallyOrdered :: PartialOrd a => [a] -> Bool
totallyOrdered = totallyOrderedBy le

totallyOrderedBy :: (a -> a -> Bool) -> [a] -> Bool
totallyOrderedBy le xs = mutually ordered xs
    where 
      ordered x y = x `le` y || y `le` x

propTotallyOrderedBy :: Ord a => [a] -> Bool
propTotallyOrderedBy xs = totallyOrderedBy (<=) xs

QUICKCHECK((propTotallyOrderedBy :: [Int] -> Bool))


type RelationGraph a = Gr a ()

insertBottom :: (Enum a, Bounded a)=> a -> RelationGraph a -> RelationGraph a
insertBottom x g 
    = trc $ insEdges [(y,x',()) | 
                      y <- map fromEnum all_a ] g'
    where 
      all_a = map snd ns
      ns = map (fromEnum /\ id) [minBound .. maxBound]
      g' = insNodes (filter (not . gelem') ns) g
      x' = fromEnum x
      gelem' (x,l) = gelem x g

makeRelationGraph :: Enum a =>  [(a,a)] -> RelationGraph a
makeRelationGraph xs = mkGraph nodes edges
    where
      nodes = uniqBy ( (==) `on` fst) $ sortBy (compare `on` fst) $ 
              map (\x -> (fromEnum x,x)) $ map fst xs ++ map snd xs
      edges = map (\(x,y) -> (fromEnum x,fromEnum y,())) xs

makePreorderGraph :: Enum a => [ (a,a) ] -> RelationGraph a
makePreorderGraph =  trc . makeRelationGraph

isAntiSymmetricGraph :: RelationGraph a -> Bool
isAntiSymmetricGraph g = no non_trivial_component (scc g)
    where
      no p xs = not $ any p xs
      non_trivial_component ns = length ns > 1


cmpBy :: (Eq a,Enum a) => RelationGraph a -> (a -> a -> PartialOrdering)
cmpBy g x y 
    | x == y 
        = Just EQ
    | not (gelem x' g) || not (gelem y' g) 
        = Nothing
    | elem x' (neighbors g y') &&  elem y' (neighbors g x')
      = Nothing
    | elem x' $ neighbors g y'
        = Just LT
    | elem y' $ neighbors g x'
        = Just GT
    | otherwise 
        = Nothing
    where
      x' = fromEnum x
      y' = fromEnum y

leByCmp :: (a -> a -> PartialOrdering) -> (a -> a -> Bool)
leByCmp cmp x y = case cmp x y of {Just EQ-> True;Just LT-> True;_->False}

geByCmp :: (a -> a -> PartialOrdering) -> (a -> a -> Bool)
geByCmp cmp x y = case cmp x y of {Just EQ-> True;Just GT-> True;_->False}

leBy :: (Eq a,Enum a) => RelationGraph a -> (a -> a -> Bool)
leBy g x y 
    | x == y 
        = True
    | not (gelem x' g) || not (gelem y' g) 
        = False
    | elem x' (suc g y' ) && elem y' (suc g x')
        = ERROR(("not well-defined order"))
    | elem x' $ suc g y'
        = True
    | otherwise 
        = False
    where
      x' = fromEnum x
      y' = fromEnum y

geBy :: (Eq a,Enum a) => RelationGraph a -> (a -> a -> Bool)
geBy g x y = leBy g y x

propMeetSemiLattice :: (Eq a,Enum a) => RelationGraph a -> a -> a -> Bool
propMeetSemiLattice g x y  = x == y || hasMeet g x y

propMeet :: (Enum a,Eq a) => RelationGraph a -> a -> a -> Bool
propMeet g x y = 
    (leBy g x y --> meet g x y == Just x) &&
    (leBy g y x --> meet g x y == Just y) 

meet :: Enum a => RelationGraph a -> a -> a -> Maybe a
meet g x y 
    | gelem x' g  && gelem y' g  
      = root g $ suc g x' `intersect` suc g y'
    | otherwise = Nothing
    where
      x' = fromEnum x
      y' = fromEnum y

hasMeet :: Enum a => RelationGraph a -> a -> a -> Bool
hasMeet g x y = isJust $ meet g x y

root :: Enum a => RelationGraph a -> [Graph.Node] -> Maybe a
root g ns 
    | null $ nodes g' \\ reachable x0 g'
      = Just (toEnum x0)
    | otherwise
      = Nothing
    where
      x0 = head $ topsort g'
      g' = makeSubgraph g ns

hasRoot :: Enum a => RelationGraph a -> [Graph.Node] -> Bool
hasRoot g ns = isJust $ root g ns

makeSubgraph :: RelationGraph a -> [Graph.Node] -> RelationGraph a
makeSubgraph g ns = ufold addContext Graph.empty g
    where
      addContext :: Context a () -> RelationGraph a -> RelationGraph a
      addContext (inEdges,n,x,outEdges) g 
          | elem n ns = (filter f inEdges,n,x,filter f outEdges) & g
          | otherwise = g
      f :: (b,Graph.Node) -> Bool
      f (l,n) = elem n ns

-- display :: (Graph gr,Show a,Show b) => gr a b -> IO ExitCode
-- display g = 
--   do writeFile "tmp.dot" (graphviz' g) -- "tmp" (5,6) (1,1) Landscape)
--      system "dot  -Tpng tmp.dot -o tmp.png && feh tmp.png"

-- 
-- Tests
data PO = Zero | One | Two | Three | Four | Five
          deriving (Eq,Ord,Show,Enum,Bounded)

instance Arbitrary PO where
    arbitrary = elements [minBound .. maxBound]

a,b,c,d :: RelationGraph PO
a = insertBottom Zero $ makePreorderGraph []
le_a = leBy a

QUICKCHECK((propPartialOrd le_a))
QUICKCHECK((propMeet a))
QUICKCHECK((propMeetSemiLattice a ))

b = insertBottom Zero $ makePreorderGraph [(One,One)]
QUICKCHECK((propPartialOrd (leBy b)))
QUICKCHECK((propMeet b))
QUICKCHECK((propMeetSemiLattice b))

c = insertBottom Zero $ makePreorderGraph [(Two,One),(Three,One)]
QUICKCHECK((propPartialOrd (leBy c)))
QUICKCHECK((propMeet c))
QUICKCHECK((propMeetSemiLattice c))

prop_d_partial_order x y z = propPartialOrd (leBy d) x y z

d = insertBottom Zero $ 
    makePreorderGraph [(One,Two),(One,Three),(Four,Two),(Four,Three)]
QUICKCHECK((propPartialOrd (leBy d)))
QUICKCHECK((propMeet d))
ASSERT_EQ((hasMeet d One Four),(False))

