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
module Operands(OperandType(..)
               ,setOperandMode
               ,operandIntersection
               ,disjointOperands
               ,Operands.le
               ,operandIntersectionX64
               ,disjointOperandsX64
               ,leX64
               ,operandIntersectionX86
               ,disjointOperandsX86
               ,leX86
               ,isModrm
               ,isImm
               ,RMType(..)
               ,rmType
               ,opSize
               ,moreSpecificOperand
               ,guessOperandSizeByOperands
               ,OperandEnc(..)
               ,isImmEnc
               ,operandEncSetMap
               ,Operands.runTests
               )
                

where

import qualified Test.HUnit
import Test.HUnit((@?=))
import Test.QuickCheck hiding((><))
import Test.QuickCheck.Gen
import ErasmCommon
import PartialOrder

import qualified Data.Set as Set
import Data.Set(Set)
import qualified Data.Map as Map
import Data.Map(Map)


--  Each operand type is identified as a mapping from the current operand size
--  to a set of arguments.Furthermore,the set of operand types should form
-- a meet-semilattice (or a lower-semilattice) 
-- with the bottom element OpNothing.

data OperandType = OpRel8  |OpRel16 |OpRel32 |OpRel64
                 | OpIMM8  |OpIMM16 |OpIMM32 |OpIMM64
                 | OpFar16 |OpFar32 |OpFar64

                 | OpR8  | OpR8_86  | OpR8Low  | OpR8High | OpR8_Rex 
                 | OpR16 | OpR16_86 | OpR16_Rex | OpR16_m_AX  | OpR16_86_m_AX
                 | OpR32 | OpR32_86 | OpR32_Rex | OpR32_m_EAX | OpR32_86_m_EAX
                 | OpR64 | OpR64_m_RAX

                 | OpST | OpST_m_ST0
                 | OpMM | OpXMM
                 | OpCR | OpDR
                 | OpSReg

                 | OpRUnused  -- used in LFENCE and SWAPGS instructions

                 | OpAL  | OpCL | OpAX | OpDX | OpEAX |OpRAX 
                 | OpST0 | OpXMM0
                 | OpDS  | OpES | OpSS | OpFS | OpGS | OpCS

                 | OpRM8  | OpR8_Rex_M8
                 | OpRM16 | OpRM32 |OpRM64
                 | OpXMM_M16 | OpXMM_M32 | OpXMM_M64 | OpXMM_M128
                 | OpMM_M32  | OpMM_M64
                 | OpR32_M8  | OpR32_M16  
                 | OpR64_M8  | OpR64_M16 | OpR64_M32

                 | OpM  -- Mainly for void pointers
                        -- (pointers to  unspecified data types)
                 | OpM8 | OpM8_86 | OpM8_Rex

                 | OpM16 | OpM32 | OpM64 | OpM128 | OpM128Xmm
                 | OpMFar16 | OpMFar32  | OpMFar64
                 | OpM16_16 | OpM16_32  | OpM16_64  |  OpM32_32
                 | OpMOffs8 | OpMOffs16 | OpMOffs32 |  OpMOffs64
                 | OpM32FP  | OpM64FP   | OpM80FP
                 | OpM16Int | OpM32Int  | OpM64Int
                 | OpM80Dec | OpM80BCD
                 | OpM2byte | OpM512byte | OpM94_108byte | OpM14_28byte

                 | Op1 | Op0    --  constants

                 | OpM8_ES_DI_EDI_RDI  | OpM8_DS_SI_ESI_RSI
                 | OpM16_ES_DI_EDI_RDI | OpM16_DS_SI_ESI_RSI
                 | OpM32_ES_DI_EDI_RDI | OpM32_DS_SI_ESI_RSI
                 | OpM64_ES_DI_EDI_RDI | OpM64_DS_SI_ESI_RSI
                 | OpM8_DS_BX_EBX_RBX

                 -- modal operand types
                 -- RDefault means R64 if x64 or R32 if x86 mode
                 | OpRDefault
                 | OpRDefault_M8 | OpRDefault_M16 | OpRDefault_M32
                 | OpR64_R32_M32

                 -- the bottom element
                 | OpNothing
                   deriving (Eq,Enum,Bounded,Ord,Show)

instance Arbitrary OperandType where
    arbitrary = elements [minBound .. maxBound]

isGroundOperand :: OperandType -> Bool
isGroundOperand x = Set.member x groundOperandSet

groundOperandSet :: Set OperandType
groundOperandSet = 
    Set.fromList [OpAL,OpAX,OpEAX,OpRAX,OpDX,OpCL,OpST0,OpXMM0
                 ,OpDS,OpES, OpSS,OpFS,OpGS ]


moreSpecificOperand :: Mode -> OperandType -> OperandType -> Bool
moreSpecificOperand mode x y 
    = x == y || (Operands.le mode x y 
                 && (isGroundOperand x || builtinConvertible x y) )

builtinConvertible :: OperandType -> OperandType -> Bool
builtinConvertible OpIMM8 OpIMM16 = True
builtinConvertible OpIMM8 OpIMM32 = True
builtinConvertible OpIMM8 OpIMM64 = True

builtinConvertible OpIMM16 OpIMM32 = True
builtinConvertible OpIMM16 OpIMM64 = True

builtinConvertible OpIMM32 OpIMM64 = True

builtinConvertible OpRel8 OpRel16 = True
builtinConvertible OpRel8 OpRel32 = True
builtinConvertible OpRel8 OpRel64 = True
builtinConvertible OpRel8 OpIMM8 = True
builtinConvertible OpRel8 OpIMM16 = True
builtinConvertible OpRel8 OpIMM32 = True
builtinConvertible OpRel8 OpIMM64 = True

builtinConvertible OpRel16 OpRel32 = True
builtinConvertible OpRel16 OpRel64 = True
builtinConvertible OpRel16 OpIMM16 = True
builtinConvertible OpRel16 OpIMM32 = True
builtinConvertible OpRel16 OpIMM64 = True

builtinConvertible OpRel32 OpRel64 = True
builtinConvertible OpRel32 OpIMM32 = True
builtinConvertible OpRel32 OpIMM64 = True

builtinConvertible OpRel64 OpIMM64 = True

builtinConvertible _ _ = False

setOperandMode :: Mode -> OperandType -> OperandType
setOperandMode X64 OpRDefault     = OpR64
setOperandMode X64 OpRDefault_M8  = OpR64_M8
setOperandMode X64 OpRDefault_M16 = OpR64_M16
setOperandMode X64 OpRDefault_M32 = OpR64_M32
setOperandMode X86 OpRDefault     = OpR32
setOperandMode X86 OpRDefault_M8  = OpR32_M8
setOperandMode X86 OpRDefault_M16 = OpR32_M16
setOperandMode X86 OpRDefault_M32 = OpRM32
setOperandMode _   x = x


disjointOperands :: Mode -> OperandType -> OperandType -> Bool
disjointOperands X64 = disjointOperandsX64
disjointOperands X86 = disjointOperandsX86

operandIntersectionX64 :: OperandType -> OperandType ->  OperandType
operandIntersectionX64 x y = fromJustE $ meet operandGraphX64 x y
    where fromJustE (Just m) = m
          fromJustE Nothing  = ERROR((show  (x,y)))

disjointOperandsX64 :: OperandType -> OperandType -> Bool
disjointOperandsX64 x y = operandIntersectionX64 x y == OpNothing

operandIntersectionX86 :: OperandType -> OperandType ->  OperandType
operandIntersectionX86 x y = fromJustE $ meet operandGraphX86 x y
    where fromJustE (Just m) = m
          fromJustE Nothing  = ERROR((show $  (x,y)))

disjointOperandsX86 :: OperandType -> OperandType -> Bool
disjointOperandsX86 x y = operandIntersectionX86 x y  == OpNothing

makeOperandGraph :: [(OperandType,[OperandType])] -> RelationGraph OperandType
makeOperandGraph ds = insertBottom OpNothing $ makeRelationGraph ds'
    where 
      ds' = concat $ map f ds
      f (o,os) = map (pair o) os
      pair x y = (x,y)


-- The graph of the partial order induced by 
-- the relation `properly and directly contains'
-- (we have to check anti-symmetricity seperately)
operandGraphX64 :: RelationGraph OperandType
QUICKCHECK((propMeetSemiLattice operandGraphX64))
operandGraphX64 = makeOperandGraph
    [ 
      -- We need to take the possible ambuigity 
      -- arising from the standard conversion sequence in mind.
      -- It is perhaps better to have a equivalence class for these built-in
      -- types? (in which case we would have a preorder)
      (OpRel64,[OpRel32])
    , (OpRel32,[OpRel16])
    , (OpRel16,[OpRel8])
    , (OpIMM8,[OpRel8,Op1,Op0])
    , (OpIMM64,[OpRel64,OpIMM32])
    , (OpIMM32,[OpRel32,OpIMM16])
    , (OpIMM16,[OpRel16,OpIMM8])

    , (OpR8 ,[OpR8_86,OpR8_Rex])
    , (OpR8_86   , [ OpR8Low,OpR8High])
    , (OpR8Low   , [ OpAL,OpCL])
    , (OpR16     , [ OpAX,OpDX,OpR16_86,OpR16_Rex,OpR16_m_AX,OpR16_86_m_AX])
    , (OpR16_86  , [ OpAX,OpDX,OpR16_86_m_AX])
    , (OpR16_Rex  , [])
    , (OpR16_m_AX  , [OpDX,OpR16_Rex,OpR16_86_m_AX])
    , (OpR16_86_m_AX , [OpDX])
    , (OpR32,[OpEAX,OpR32_86,OpR32_Rex,OpR32_m_EAX,OpR32_86_m_EAX])
    , (OpR32_86  ,[OpEAX,OpR32_86_m_EAX])
    , (OpR32_Rex ,[])
    , (OpR32_m_EAX , [ OpR32_Rex , OpR32_86_m_EAX ])
    , (OpR32_86_m_EAX , [])
      
    , (OpR64,[OpRAX,OpR64_m_RAX])
    , (OpST ,[OpST0,OpST_m_ST0])
    , (OpXMM,[OpXMM0])
    , (OpSReg,[OpDS,OpES,OpSS,OpFS,OpGS,OpCL])
    , (OpR8_Rex_M8,[OpR8_Rex,OpM8])
    , (OpRM8,[OpR8,OpR8_Rex_M8])
    , (OpRM16,[OpR16,OpM16])
    , (OpRM32,[OpR32,OpM32])
    , (OpRM64,[OpR64,OpM64])
    , (OpXMM_M16 , [ OpXMM , OpM16 ])
    , (OpXMM_M32 , [ OpXMM , OpM32 ])
    , (OpXMM_M64 , [ OpXMM , OpM64 ])
    , (OpXMM_M128, [ OpXMM , OpM128Xmm ])
    , (OpMM_M32  , [ OpMM  , OpM32])
    , (OpMM_M64  , [ OpMM  , OpM64])
    , (OpR32_M8  , [ OpR32 , OpM8])
    , (OpR32_M16 , [ OpR32 , OpM16 ])
    , (OpR64_M8  , [ OpR64 , OpM8 ])
    , (OpR64_M16 , [ OpR64 , OpM16 ])
    , (OpR64_M32 , [ OpR64 , OpM32 ])
    , (OpM8 , [ OpM8_86,OpM8_Rex, OpM8_ES_DI_EDI_RDI , OpM8_DS_SI_ESI_RSI ,OpM8_DS_BX_EBX_RBX ])
    , (OpM16, [ OpM16_ES_DI_EDI_RDI , OpM16_DS_SI_ESI_RSI ] )
    , (OpM32, [ OpM32_ES_DI_EDI_RDI , OpM32_DS_SI_ESI_RSI ] )
    , (OpM64, [ OpM64_ES_DI_EDI_RDI , OpM64_DS_SI_ESI_RSI ] )
        -- modal operand types
        -- RDefault means R64 if x64 or R32 if x86 mode
    , (OpRDefault, [ OpR64 ] )
    , (OpRDefault_M8  , [ OpRDefault , OpM8 ])
    , (OpRDefault_M16 , [ OpRDefault , OpM16 ])
    , (OpRDefault_M32 , [ OpRDefault , OpM32 ])
    , (OpR64_R32_M32  , [ OpR64 , OpM32 ] )
    ]


leX64 :: OperandType -> OperandType -> Bool
QUICKCHECK((propPartialOrd leX64))
ASSERT_TRUE((leX64 OpEAX OpR32_M16))
ASSERT_TRUE((leX64 OpRAX OpRDefault_M8))
ASSERT_FALSE((leX64 OpEAX OpRDefault_M8))
ASSERT_TRUE((leX64 OpRAX OpRDefault_M32))
leX64 = leBy operandGraphX64 `on` setOperandMode X64


operandGraphX86 :: RelationGraph OperandType
QUICKCHECK((propMeetSemiLattice operandGraphX86))
operandGraphX86 = makeOperandGraph
    [ 
      -- We need to take the possible ambuigity 
      -- arising from the standard conversion sequence in mind.
      -- It is perhaps better to have a equivalence class for these built-in
      -- types? (in which case we would have a preorder)
      (OpRel64,[OpRel32])
    , (OpRel32,[OpRel16])
    , (OpRel16,[OpRel8])
    , (OpIMM8,[OpRel8,Op1,Op0])
    , (OpIMM64,[OpRel64,OpIMM32])
    , (OpIMM32,[OpRel32,OpIMM16])
    , (OpIMM16,[OpRel16,OpIMM8])

    , (OpR8 ,[ OpAL,OpCL])
    , (OpR16     , [ OpAX,OpDX,OpR16_m_AX])
    , (OpR16_m_AX  , [OpDX])
    , (OpR32,[OpEAX,OpR32_m_EAX])
    , (OpR32_m_EAX , [])
      
    , (OpST ,[OpST0,OpST_m_ST0])
    , (OpXMM,[OpXMM0])
    , (OpSReg,[OpDS,OpES,OpSS,OpFS,OpGS,OpCL])
    , (OpRM8,[OpR8,OpM8])
    , (OpRM16,[OpR16,OpM16])
    , (OpRM32,[OpR32,OpM32])
    , (OpRM64,[OpR64,OpM64])
    , (OpXMM_M16 , [ OpXMM , OpM16 ])
    , (OpXMM_M32 , [ OpXMM , OpM32 ])
    , (OpXMM_M64 , [ OpXMM , OpM64 ])
    , (OpXMM_M128, [ OpXMM , OpM128Xmm ])
    , (OpMM_M32  , [ OpMM  , OpM32])
    , (OpMM_M64  , [ OpMM  , OpM64])
    , (OpR32_M8  , [ OpR32 , OpM8])
    , (OpR32_M16 , [ OpR32 , OpM16 ])
    , (OpR64_M8  , [ OpR64 , OpM8 ])
    , (OpR64_M16 , [ OpR64 , OpM16 ])
    , (OpR64_M32 , [ OpR64 , OpM32 ])
    , (OpM8 , [ OpM8_ES_DI_EDI_RDI , OpM8_DS_SI_ESI_RSI ,OpM8_DS_BX_EBX_RBX ])
    , (OpM16, [ OpM16_ES_DI_EDI_RDI , OpM16_DS_SI_ESI_RSI ] )
    , (OpM32, [ OpM32_ES_DI_EDI_RDI , OpM32_DS_SI_ESI_RSI ] )
    , (OpM64, [ OpM64_ES_DI_EDI_RDI , OpM64_DS_SI_ESI_RSI ] )
        -- modal operand types
        -- RDefault means R64 if x64 or R32 if x86 mode
    , (OpRDefault, [ OpR32 ] )
    , (OpRDefault_M8  , [ OpRDefault , OpM8 ])
    , (OpRDefault_M16 , [ OpRDefault , OpM16 ])
    , (OpRDefault_M32 , [ OpRDefault , OpM32 ])
    , (OpR64_R32_M32  , [ OpR32 , OpM32 ] )
    ]


leX86 :: OperandType -> OperandType -> Bool
QUICKCHECK((propPartialOrd leX86))
ASSERT_TRUE((leX86 OpEAX OpR32_M16))
ASSERT_TRUE((leX86 OpEAX OpRDefault_M32))
leX86 = leBy operandGraphX86 `on` setOperandMode X86


le :: Mode -> OperandType -> OperandType -> Bool
le X64 = leX64
le X86 = leX86

operandIntersection :: Mode -> OperandType -> OperandType ->  OperandType
operandIntersection X64 = operandIntersectionX64
operandIntersection X86 = operandIntersectionX86

ASSERT_EQ((operandIntersectionX64 OpR8 OpR8_86),(OpR8_86))
ASSERT_EQ((operandIntersectionX64 OpR8 OpCL),(OpCL))

modrmParamSet :: Set OperandType
modrmParamSet =
  Set.fromList [ OpRM8 , OpRM16 , OpRM32 ,OpRM64
               , OpM
               , OpM8 , OpM16 , OpM32 ,OpM64 ,OpM128
               , OpMFar16 ,OpMFar32 , OpMFar64
               , OpM16_16 ,OpM16_32 , OpM16_64 ,  OpM32_32
               , OpM32FP ,OpM64FP ,OpM80FP
               , OpM16Int ,OpM32Int ,OpM64Int
               , OpMM_M32 ,OpMM_M64
               , OpXMM_M16 , OpXMM_M32 ,OpXMM_M64 ,OpXMM_M128
               , OpRDefault_M8 , OpRDefault_M16 , OpRDefault_M32
               , OpR32_M8 , OpR32_M16
               , OpR64_R32_M32 , OpR64_M16
               , OpM80Dec , OpM80BCD
               , OpM2byte , OpM512byte , OpM94_108byte , OpM14_28byte
               ]

isModrm :: OperandType -> Bool
isModrm = flip Set.member modrmParamSet

isImm :: OperandType -> Bool
isImm x = (OpRel8 <= x && x <= OpIMM64)


data RMType =  RMRegOnly | RMMemOnly | RMBoth
            deriving (Eq,Ord,Enum,Bounded,Show)

instance Arbitrary RMType where
    arbitrary = elements [minBound .. maxBound]


rmTypeMap :: Map OperandType RMType
rmTypeMap = Map.fromList $
            pair RMBoth [ OpRM8,OpRM16,OpRM32,OpRM64 
                        , OpXMM_M16 , OpXMM_M32 ,OpXMM_M64 , OpXMM_M128 
                        , OpMM_M32 ,OpMM_M64
                        , OpRDefault_M8 , OpRDefault_M16 , OpRDefault_M32 
                        , OpR32_M8 , OpR32_M16 
                        , OpR64_R32_M32 , OpR64_M16
                        ] ++ 
            pair RMRegOnly [OpR8,OpR16,OpR32,OpR64
                           , OpXMM ,OpMM ,OpCR,OpDR , OpRUnused
                           ] ++
            pair RMMemOnly [ OpM,OpM8,OpM16,OpM32,OpM64,OpM128,OpM128Xmm
                           , OpMFar16,OpMFar32,OpMFar64
                           , OpM16_16 ,OpM16_32 , OpM16_64 ,  OpM32_32
                           , OpM32FP ,OpM64FP ,OpM80FP
                           , OpM16Int ,OpM32Int ,OpM64Int
                           , OpM80Dec , OpM80BCD
                           , OpM2byte , OpM512byte
                           , OpM94_108byte , OpM14_28byte ] 
  where pair r os = map (\x -> (x,r)) os

rmType :: OperandType -> Maybe RMType
rmType o = Map.lookup o rmTypeMap



guessOperandSizeByOperands :: [OperandType] -> Maybe Int
guessOperandSizeByOperands os =
  case (catMaybes ) (map (\x-> Map.lookup x opSizeMap) os) of
    []   -> Nothing --_ERROR(show(pretty oes <$$> pretty ns))
    [n]  ->  Just n
    ns   ->  Just $ maximum ns

ASSERT_EQ((guessOperandSizeByOperands [OpM32,OpDX]),(Just 4))
ASSERT_EQ((guessOperandSizeByOperands [OpM16,OpDX]),(Just 2))


ASSERT_EQ((map (flip Map.lookup opSizeMap) [OpRel32]),([Just 4]))


opSize :: OperandType -> Maybe Int
opSize o = Map.lookup o opSizeMap

opSizeMap :: Map (OperandType) Int
opSizeMap = Map.fromList [ (OpAL, 1)
                         , (OpAX , 2)
                         , (OpEAX, 4)
                         , (OpRAX  ,8)
                         , (OpR8   ,1)
                         , (OpR8_86   ,1)
                         , (OpR8Low   ,1)
                         , (OpR8High   ,1)
                         , (OpR8_Rex   ,1)
                         , (OpR16  ,2)
                         , (OpR16_86  ,2)
                         , (OpR16_Rex  ,2)
                         , (OpR16_m_AX  ,2)
                         , (OpR16_86_m_AX  ,2)
                         , (OpR32  ,4)
                         , (OpR32_86  ,4)
                         , (OpR32_Rex  ,4)
                         , (OpR32_m_EAX  ,4)
                         , (OpR32_86_m_EAX  ,4)
                         , (OpR64  ,8)
                         , (OpR64_m_RAX  ,8)

                         , (OpRM8   ,1)
                         , (OpR8_Rex_M8   ,1)
                         , (OpRM16  ,2)
                         , (OpRM32  ,4)
                         , (OpRM64  ,8)
                         , (OpIMM8   ,1)
                         , (OpIMM16  ,2)
                         , (OpIMM32  ,4)
                         , (OpIMM64  ,8)

                         , (OpRel8 , 1)
                         , (OpRel16, 2)
                         , (OpRel32, 4)
                         , (OpRel64, 8)

                         , (OpM8  ,1)
                         , (OpM8_86  ,1)
                         , (OpM8_Rex  ,1)
                         , (OpM16  ,2)
                         , (OpM32  ,4)
                         , (OpM64  ,8)
                         , (OpR32_M16 , 4)
                         , (OpR64_M16 , 8)

                         , (OpFar16  , 2)
                         , (OpFar32  , 4)
                         , (OpMFar16 , 2)
                         , (OpMFar32 , 4)
                         , (OpMFar64 , 8)
                         , (OpM8_DS_SI_ESI_RSI ,1)
                         , (OpM8_ES_DI_EDI_RDI ,1)
                         , (OpM16_DS_SI_ESI_RSI ,2)
                         , (OpM16_ES_DI_EDI_RDI ,2)
                         , (OpM32_DS_SI_ESI_RSI ,4)
                         , (OpM32_ES_DI_EDI_RDI ,4)
                         , (OpM64_DS_SI_ESI_RSI ,8)
                         , (OpM64_ES_DI_EDI_RDI ,8)

                         , (OpMOffs16 , 2)
                         , (OpMOffs32 , 4)
                         ]



data OperandEnc = EncModrm_reg
                | EncModrm_rm
                | EncModrm_rm_reg
                | EncModrm_rm_mem
                | Enc
                | EncReg
                | EncIMM
                | EncOffset
                | EncImplicit
                | EncDisplacement
                | EncDS_SI_ESI_RSI
                | EncES_DI_EDI_RDI
                | Enc1
                deriving (Eq,Ord,Enum,Bounded,Show)

instance Arbitrary OperandEnc where
    arbitrary = elements [minBound..maxBound]

isImmEnc :: OperandEnc -> Bool
isImmEnc x = x == EncIMM

operandEncSetMap :: Map OperandType (Set OperandEnc)
operandEncSetMap = 
    Map.fromList $ concat $ map (\(os,es) -> zip os (repeat $ Set.fromList es))
           [ ([ OpRel8 
              , OpRel16
              , OpRel32
              , OpRel64
              , OpFar16
              , OpFar32
              , OpFar64 ]
             , [EncOffset])
           , ([ OpIMM8
              , OpIMM16
              , OpIMM32
              , OpIMM64
              ]
             , [EncIMM])
           , ([ OpR8
              , OpR8_86
              , OpR8Low
              , OpR8High
              , OpR8_Rex 
              , OpR16
              , OpR16_86
              , OpR16_Rex
              , OpR32
              , OpR32_86
              , OpR32_Rex
              , OpR64
              , OpST
              , OpMM
              , OpXMM
              , OpSReg
              , OpRUnused 
              , OpRDefault
              ]
             , [EncModrm_reg,EncModrm_rm,EncModrm_rm_reg,EncReg] )

           , ([ OpCR
              , OpDR ]
             , [ EncModrm_reg ] )

           , ([ OpAL
              , OpAX
              , OpEAX
              , OpRAX
              , OpDX
              , OpCL
              , OpST0
              , OpXMM0
              , OpDS
              , OpES
              , OpSS
              , OpFS
              , OpGS
              , OpCS
              ]
             , [EncImplicit])
           , ([Op1] , [Enc1])
           , ([ OpRM8
              , OpR8_Rex_M8
              , OpRM16
              , OpRM32
              , OpRM64
              , OpXMM_M16
              , OpXMM_M32
              , OpXMM_M64
              , OpXMM_M128
              , OpMM_M32
              , OpMM_M64
              , OpR32_M8
              , OpR32_M16  
              , OpR64_M8
              , OpR64_M16
              , OpR64_M32 
              , OpRDefault_M8 , OpRDefault_M16 , OpRDefault_M32
              , OpR64_R32_M32
              ]
             , [EncModrm_rm])
           , ([ OpM 
              , OpM8
              , OpM16 
              , OpM32 
              , OpM64
              , OpM128
              , OpM128Xmm
              , OpMFar16 , OpMFar32  , OpMFar64
              , OpM16_16 , OpM16_32  , OpM16_64  ,  OpM32_32
              , OpM32FP  , OpM64FP   , OpM80FP
              , OpM16Int , OpM32Int  , OpM64Int
              , OpM80Dec , OpM80BCD
              , OpM2byte , OpM512byte , OpM94_108byte , OpM14_28byte ]
             , [ EncModrm_rm,EncModrm_rm_mem ] )
           , ([ OpM8_ES_DI_EDI_RDI  
              , OpM16_ES_DI_EDI_RDI 
              , OpM32_ES_DI_EDI_RDI 
              , OpM64_ES_DI_EDI_RDI 
              ]
             , [EncES_DI_EDI_RDI])
           , ([ OpM8_DS_SI_ESI_RSI 
              , OpM16_DS_SI_ESI_RSI
              , OpM32_DS_SI_ESI_RSI
              , OpM64_DS_SI_ESI_RSI
              , OpM8_DS_BX_EBX_RBX
              ]
             , [EncDS_SI_ESI_RSI])
           , ([OpMOffs8 , OpMOffs16 , OpMOffs32 ,  OpMOffs64]
             ,[EncDisplacement])
           ]
