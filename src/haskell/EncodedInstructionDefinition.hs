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
{-# LANGUAGE PatternGuards #-}
module EncodedInstructionDefinition
       ( EncodedInstructionDefinition
       , OperandSpec
       , OperandSpecList
       , Prototype(..)
       , OperandType(..)
       , ModrmType(..)
       , RegAdd(..)
       , IMMType(..)
       , Opcode(..)
       , OperandEnc(..)
       , OperandAccess(..)
       , Byte
       , RMType(..)
       , makeDef
       , opcode
       , ambiguousPrototypes
       , unambiguousPrototypes
       , overlappingPrototypes
       , disjointPrototypes
       , moreSpecificPrototype
       , isSubsetPrototype
       , unionPrototype
       , emptyOpcode
       , setDescription
       , setOpcodePrefix
       , setOpcodeBytes
       , setOpcodeModrm
       , setOpcodeRegadd
       , setOpcodeImms
       , setOperands
       , setOpcodeOperandSizeMode
       , setOpcodeAddressSizeMode
       , arity
       , setOperandSizeMode
       , setAddressSizeMode
       , prefix
       , isRex
       , rexw
       , isModrm
       , isImm
       , isImmEnc
       , mnemonic
       , protoType
       , operands
       , operandSpecList
       , operandSpecList2
       , operandEncList
       , operandList
       , operandList2
       , valid64
       , validLeg
       , description
       , regAdd
       , hasRegAdd
       , modrm
       , needModrm
       , modrmDigit
       , opcodeBytes
       , opcodeLength
       , imms
       , immSize
       , insertPrefix
       , operandSpecFromList
       , aliasMnemonic
       , aliasDefinitions
       , rmType
       , assumeOperandReadWrite
       , updateMnemonic
       , updatePrototype
       , updateOperands
       , updateOperandSpecList
       , updateOperandSpecAt
       , updateOperandList
       , updateOperandList2
       , updatePrefix
       , updateOpcodeBytes
       , updateOpcodeModrm
       , updateRmType
       , updateOperandEnc
       , updateImm
       , updateValidLeg
       , updateValid64
       , emptyOperandSpec
       , operandSizeMode
       , addressSizeMode
       , protoTypeOperandSize
       , protoTypeAddressSize
       , isXmmInstruction
       , isMmInstruction
       , prefix_LOCK 
       , prefix_REPNE 
       , prefix_REP 
       , prefix_CS 
       , prefix_SS 
       , prefix_DS 
       , prefix_ES 
       , prefix_FS 
       , prefix_GS 
       , prefix_NOT_BRANCH 
       , prefix_BRANCH 
       , prefix_OPERAND_SIZE 
       , prefix_ADDRESS_SIZE 
       , prefix_REX 
       , prefix_REXW 
       , EncodedInstructionDefinition.runTests
       )
       where
import Test.HUnit
import Test.QuickCheck
import ErasmCommon
import qualified Prefix
import Data.Int
import Data.Set(Set)
import qualified Data.Set as Set

import Data.Map(Map)
import qualified Data.Map as Map

import Operands hiding(rmType)
import qualified Operands
import Equivalence

infixl 8 `setOpcodePrefix` , `setOpcodeBytes` , `setOpcodeModrm`
infixl 8 `setOpcodeRegadd` , `setOpcodeImms` 
infixl 8 `setOpcodeOperandSizeMode` , `setOpcodeAddressSizeMode`

type OperandSpec = (OperandEnc,OperandAccess)
type OperandSpecList = [ Maybe OperandSpec ]

data EncodedInstructionDefinition =
  EncodedInstructionDefinition
  { encdefOpcode       :: Opcode
  , encdefPrototype    :: Prototype
  , encdefOperandSpec  :: OperandSpecList
  , encdefValid64      :: Bool
  , encdefValidLeg     :: Bool
  , encdefDescription  :: String
  }  deriving (Eq,Ord,Show)



setDescription :: EncodedInstructionDefinition -> String
               -> EncodedInstructionDefinition
setDescription e d = e { encdefDescription = d }

setOperandSizeMode :: EncodedInstructionDefinition 
                   -> Maybe OperandSize
                   -> EncodedInstructionDefinition
setOperandSizeMode e x = e { encdefOpcode = o }
    where
      o = opcode e `setOpcodeOperandSizeMode` x

setAddressSizeMode :: EncodedInstructionDefinition 
                   -> Maybe AddressSize
                   -> EncodedInstructionDefinition
setAddressSizeMode e x = e { encdefOpcode = o }
    where
      o = opcode e `setOpcodeAddressSizeMode` x

setOperands :: EncodedInstructionDefinition -> [OperandType] 
            -> EncodedInstructionDefinition
setOperands = flip updateOperands

ambiguousPrototypes :: Mode -> Prototype -> Prototype -> Bool
ambiguousPrototypes mode x y = not $ unambiguousPrototypes mode x y

unambiguousPrototypes :: Mode -> Prototype -> Prototype -> Bool
unambiguousPrototypes mode x y =
    disjointPrototypes mode x y || 
    (x /= y &&  (moreSpecificPrototype mode x y ||
                 moreSpecificPrototype mode y x ))

ASSERT_TRUE((unambiguousPrototypes X64
             (Prototype "ADC" [OpAL,OpIMM8])
             (Prototype "ADC" [OpRM8,OpIMM8])))

ASSERT_TRUE((moreSpecificOperand X64 OpAL OpRM8))
ASSERT_TRUE((moreSpecificOperand X64 OpIMM8 OpIMM8))

overlappingPrototypes :: Mode -> Prototype -> Prototype -> Bool
overlappingPrototypes mode x@(Prototype m xs) y@(Prototype n ys)
    | m /= n || length xs /= length ys
        = ERROR((show $ (x, y)))
    | otherwise
        = not $ or $ zipWith (disjointOperands mode) xs ys

disjointPrototypes :: Mode -> Prototype -> Prototype -> Bool
disjointPrototypes mode x y = not $ overlappingPrototypes mode x y

moreSpecificPrototype :: Mode -> Prototype -> Prototype -> Bool
moreSpecificPrototype mode x@(Prototype m xs) y@(Prototype n ys)
    | m /= n || length xs /= length ys
        = ERROR((show $ (x, y)))
    | otherwise 
        = and $ zipWith (moreSpecificOperand mode) xs ys

isSubsetPrototype :: Mode -> Prototype -> Prototype -> Bool
isSubsetPrototype mode x@(Prototype m xs) y@(Prototype n ys)
    | m /= n || length xs /= length ys
        = ERROR((show $ (x, y)))
    | otherwise 
        = and $ zipWith (Operands.le mode) xs ys

ASSERT_TRUE((isSubsetPrototype X64 (Prototype "test" [OpR64]) (Prototype "test" [OpRM64])))

unionPrototype :: Mode -> Prototype -> Prototype -> Prototype
unionPrototype mode p q = UNIMPLEMENTED

instance Arbitrary EncodedInstructionDefinition where
    arbitrary = return EncodedInstructionDefinition
                `ap` arbitrary `ap` arbitrary `ap` arbitrary `ap` arbitrary `ap` arbitrary `ap` arbitrary
                

instance Named EncodedInstructionDefinition where
  name = mnemonic

makePrefix name code =  EncodedInstructionDefinition 
                        (emptyOpcode `setOpcodeBytes` [code])
                        (Prototype name [])
                        []
                        True True ("prefix " ++ name)


prefix_LOCK = makePrefix "LOCK" 0xf0
prefix_REPNE = makePrefix "REPNE" 0xf2
prefix_REP = makePrefix "REP" 0xf3
prefix_CS = makePrefix "CS" 0x2e
prefix_SS = makePrefix "SS" 0x36
prefix_DS = makePrefix "DS" 0x3E
prefix_ES = makePrefix "ES" 0x26
prefix_FS = makePrefix "FS" 0x64
prefix_GS = makePrefix "GS" 0x65
prefix_NOT_BRANCH = makePrefix "NOT_BRANCH" 0x2e
prefix_BRANCH = makePrefix "BRANCH" 0x3e
prefix_OPERAND_SIZE = makePrefix "OPERAND_SIZE" 0x66
prefix_ADDRESS_SIZE = makePrefix "ADDRESS_SIZE" 0x67


prefix_REX = EncodedInstructionDefinition 
             (emptyOpcode { opcodeCode = [0x40] 
                          , opcodeRegAdd = Just RaddB })
             (Prototype "REX" [])
             []
             True True "rex 0x40 - 0x47"

prefix_REXW = EncodedInstructionDefinition 
              (emptyOpcode { opcodeCode = [0x48] 
                           , opcodeRegAdd = Just RaddB })
              (Prototype "REX.W" [])
              []
              True True "rex 0x48 - 0x4f"


data Opcode = Opcode
              { opcodePrefix :: Prefix.Set
              , opcodeCode   :: [ Byte ]
              , opcodeModrm  :: Maybe ModrmType
              , opcodeRmType :: Maybe RMType
              , opcodeRegAdd :: Maybe RegAdd
              , opcodeImms   :: [IMMType]
              , opcodeOperandSizeMode :: Maybe OperandSize 
              , opcodeAddressSizeMode :: Maybe AddressSize 
              }   deriving (Eq,Ord,Show)

operandSizeMode :: EncodedInstructionDefinition -> Maybe OperandSize
operandSizeMode = opcodeOperandSizeMode . opcode 
addressSizeMode :: EncodedInstructionDefinition -> Maybe AddressSize
addressSizeMode = opcodeAddressSizeMode . opcode 

updateRmType :: Maybe RMType -> EncodedInstructionDefinition
             -> EncodedInstructionDefinition
updateRmType rm e = updateOpcode o e
    where o = (opcode e) {opcodeRmType = rm }

emptyOpcode :: Opcode
emptyOpcode = Opcode Prefix.empty [] Nothing Nothing Nothing [] 
              Nothing Nothing

setOpcodePrefix :: Opcode -> Prefix.Set -> Opcode
setOpcodePrefix o x = o { opcodePrefix  = x }

setOpcodeBytes :: Opcode -> [Byte]  -> Opcode
setOpcodeBytes o x = o { opcodeCode  = x }

setOpcodeModrm :: Opcode -> Maybe ModrmType -> Opcode
setOpcodeModrm o x = o { opcodeModrm  = x }

setOpcodeRegadd :: Opcode -> Maybe RegAdd -> Opcode
setOpcodeRegadd o x = o { opcodeRegAdd  = x }

setOpcodeImms :: Opcode -> [ IMMType ]  -> Opcode
setOpcodeImms o x = o { opcodeImms  = x }

setOpcodeOperandSizeMode :: Opcode -> Maybe OperandSize -> Opcode
setOpcodeOperandSizeMode o x = o { opcodeOperandSizeMode  = x }

setOpcodeAddressSizeMode :: Opcode -> Maybe AddressSize -> Opcode
setOpcodeAddressSizeMode o x = o { opcodeAddressSizeMode  = x }

instance Arbitrary Opcode where
    arbitrary = return Opcode `ap` arbitrary `ap`
                          arbitrary `ap` arbitrary `ap` arbitrary `ap`
                          arbitrary `ap` arbitrary `ap` arbitrary `ap` 
                          arbitrary

data Prototype = Prototype
                 { protoMnemonic :: String
                 , protoOperands :: [ OperandType ]
                 }  deriving (Eq,Ord,Show)

arity :: EncodedInstructionDefinition -> Int
arity e = length $ protoOperands $ protoType e


instance Arbitrary Prototype where
    arbitrary = liftM2 Prototype arbitrary arbitrary


data ModrmType =  ModrmDigit Int | Modrm
               deriving (Eq,Ord,Show)
instance Arbitrary ModrmType where
    arbitrary = oneof ( return Modrm : map (return . ModrmDigit) [0..7] )

data RegAdd    =  RaddB | RaddW | RaddD | RaddO | RaddI
               deriving (Eq,Ord,Enum,Bounded,Show)
instance Arbitrary RegAdd where
    arbitrary = elements [minBound..maxBound]

data IMMType   = IMM_IB | IMM_IW | IMM_ID | IMM_IO
               | IMM_CB | IMM_CW | IMM_CD | IMM_CP | IMM_CO | IMM_CT
               | IMM_IB_Const Int8
               deriving (Eq,Ord,Show)
instance Arbitrary IMMType where
    arbitrary = oneof $ liftM IMM_IB_Const arbitrary : 
                map return [IMM_IB,IMM_IW,IMM_ID,IMM_IO,IMM_CB,IMM_CW,IMM_CD
                           , IMM_CP,IMM_CO,IMM_CT ]

immSizeMap :: Map IMMType Int
immSizeMap = Map.fromList [ (IMM_IB , 1)
                          , (IMM_IW , 2)
                          , (IMM_ID , 4)
                          , (IMM_IO , 8)
                          , (IMM_CB , 1)
                          , (IMM_CW , 2)
                          , (IMM_CD , 4)
                          , (IMM_CP , 6)
                          , (IMM_CO , 8)
                          , (IMM_CT , 10)
                          ]


rmType :: EncodedInstructionDefinition -> Maybe RMType
rmType e =
  case  elemIndex (Just EncModrm_rm) (operandEncList e) of
    Just n  -> Operands.rmType (operands e !! n) 
    Nothing -> Nothing

ASSERT((rmType (EncodedInstructionDefinition
                (emptyOpcode { opcodeCode = [128] 
                             , opcodeModrm = Just $ ModrmDigit 2
                             , opcodeImms  =  [IMM_IB] })
                (Prototype  "ADC"  [OpRM8,OpIMM8])
                (operandSpecFromList [(EncModrm_rm,OperandReadOnly),(EncIMM,OperandReadOnly)])
                True True
                "Add with carry imm8 to r/m8.")
        @?= Just RMBoth))



class Sizeable a where
    size :: a -> Int

instance Sizeable IMMType where
    size (IMM_IB_Const _) = 1
    size x = Map.findWithDefault (ERROR((show x))) x immSizeMap


data OperandAccess = OperandReadOnly | OperandReadWrite
                   deriving (Eq,Ord,Enum,Bounded,Show)
instance Arbitrary OperandAccess where
    arbitrary = elements [minBound..maxBound]



aliasMnemonic :: String -> String -> Bool
aliasMnemonic = equivalentBy aliasMnemonicClasses `on` map toUpper

ASSERT_EQ((aliasMnemonic "cmovc" "cmovb"),(True))
ASSERT_EQ((aliasMnemonic "cmovb" "cmovnae"),(True))

aliasMnemonicClasses :: Equivalence String
aliasMnemonicClasses = Equivalence.fromList $
                   [ ["WAIT","FWAIT" ]
                   , [ "SHL" , "SAL" ] ] ++
                   map (map ("CMOV" ++)) suffixList ++
                   map (map ("J" ++))    suffixList ++
                   map (map ("SET" ++))  suffixList

                     where suffixList =
                             [ ["NAE","C","B"]
                             , ["NC","NB","AE"]
                             , ["Z","E"]
                             , ["NZ","NE"]
                             , ["NA","BE"]
                             , ["A","NBE"]
                             , ["PE","P"]
                             , ["PO","NP"]
                             , ["NGE","L"]
                             , ["NL","GE"]
                             , ["NG","LE"]
                             , ["NLE","G"]
                             ]

aliasPrototype :: Prototype -> Prototype -> Bool
aliasPrototype = equivalentBy aliasPrototypeClasses <||> xchgAlias

xchgAlias :: Prototype -> Prototype -> Bool
xchgAlias prt@(Prototype "XCHG" xs@[x,y]) (Prototype "XCHG" ys@[z,w]) =
  xs == ys || xs == shift ys
  where
    shift [] = ERROR((show prt))
    shift (x:xs) = xs ++ [x]
xchgAlias _ _ = False

aliasPrototypeClasses :: Equivalence Prototype
aliasPrototypeClasses = Equivalence.fromList $
                        [ [ Prototype "INSB" [] , Prototype "INS" [OpM8_ES_DI_EDI_RDI,OpDX] ]
                        , [ Prototype "INSW" [] , Prototype "INS" [OpM16_ES_DI_EDI_RDI,OpDX] ]
                        , [ Prototype "INSD" [] , Prototype "INS" [OpM32_ES_DI_EDI_RDI,OpDX] ]

                        , [ Prototype "OUTSB" [] , Prototype "OUTS" [OpDX,OpM8_DS_SI_ESI_RSI] ]
                        , [ Prototype "OUTSW" [] , Prototype "OUTS" [OpDX,OpM16_DS_SI_ESI_RSI] ]
                        , [ Prototype "OUTSD" [] , Prototype "OUTS" [OpDX,OpM32_DS_SI_ESI_RSI] ]

                        , [ Prototype "MOVSB" [] , Prototype "MOVS" [OpM8_ES_DI_EDI_RDI,OpM8_DS_SI_ESI_RSI] ]
                        , [ Prototype "MOVSW" [] , Prototype "MOVS" [OpM16_ES_DI_EDI_RDI,OpM16_DS_SI_ESI_RSI] ]
                        , [ Prototype "MOVSD" [] , Prototype "MOVS" [OpM32_ES_DI_EDI_RDI,OpM32_DS_SI_ESI_RSI] ]
                        , [ Prototype "MOVSQ" [] , Prototype "MOVS" [OpM64_ES_DI_EDI_RDI,OpM64_DS_SI_ESI_RSI] ]

                        , [ Prototype "CMPSB" []
                          , Prototype "CMPS" [OpM8_ES_DI_EDI_RDI,OpM8_DS_SI_ESI_RSI]
                          , Prototype "CMPS" [OpM8_DS_SI_ESI_RSI,OpM8_ES_DI_EDI_RDI] ]
                        , [ Prototype "CMPSW" []
                          , Prototype "CMPS" [OpM16_ES_DI_EDI_RDI,OpM16_DS_SI_ESI_RSI]
                          , Prototype "CMPS" [OpM16_DS_SI_ESI_RSI,OpM16_ES_DI_EDI_RDI] ]
                        , [ Prototype "CMPSD" []
                          , Prototype "CMPS" [OpM32_ES_DI_EDI_RDI,OpM32_DS_SI_ESI_RSI]
                          , Prototype "CMPS" [OpM32_DS_SI_ESI_RSI,OpM32_ES_DI_EDI_RDI] ]
                        , [ Prototype "CMPSQ" []
                          , Prototype "CMPS" [OpM64_ES_DI_EDI_RDI,OpM64_DS_SI_ESI_RSI]
                          , Prototype "CMPS" [OpM64_DS_SI_ESI_RSI,OpM64_ES_DI_EDI_RDI] ]

                        , [ Prototype "STOSB" [] , Prototype "STOS" [OpM8_ES_DI_EDI_RDI]  , Prototype "STOS" [OpM8_ES_DI_EDI_RDI,OpAL] ]
                        , [ Prototype "STOSW" [] , Prototype "STOS" [OpM16_ES_DI_EDI_RDI] , Prototype "STOS" [OpM16_ES_DI_EDI_RDI,OpAX] ]
                        , [ Prototype "STOSD" [] , Prototype "STOS" [OpM32_ES_DI_EDI_RDI] , Prototype "STOS" [OpM32_ES_DI_EDI_RDI,OpEAX] ]
                        , [ Prototype "STOSQ" [] , Prototype "STOS" [OpM64_ES_DI_EDI_RDI] , Prototype "STOS" [OpM64_ES_DI_EDI_RDI,OpRAX] ]

                        , [ Prototype "LODSB" [] , Prototype "LODS" [OpM8_DS_SI_ESI_RSI]  , Prototype "LODS" [OpAL,OpM8_DS_SI_ESI_RSI] ]
                        , [ Prototype "LODSW" [] , Prototype "LODS" [OpM16_DS_SI_ESI_RSI] , Prototype "LODS" [OpAX,OpM16_DS_SI_ESI_RSI] ]
                        , [ Prototype "LODSD" [] , Prototype "LODS" [OpM32_DS_SI_ESI_RSI] , Prototype "LODS" [OpEAX,OpM32_DS_SI_ESI_RSI] ]
                        , [ Prototype "LODSQ" [] , Prototype "LODS" [OpM64_DS_SI_ESI_RSI] , Prototype "LODS" [OpRAX,OpM64_DS_SI_ESI_RSI] ]

                        , [ Prototype "SCASB" [] , Prototype "SCAS" [OpM8_ES_DI_EDI_RDI]  , Prototype "SCAS" [OpAL,OpM8_ES_DI_EDI_RDI] ]
                        , [ Prototype "SCASW" [] , Prototype "SCAS" [OpM16_ES_DI_EDI_RDI] , Prototype "SCAS" [OpAX,OpM16_ES_DI_EDI_RDI] ]
                        , [ Prototype "SCASD" [] , Prototype "SCAS" [OpM32_ES_DI_EDI_RDI] , Prototype "SCAS" [OpEAX,OpM32_ES_DI_EDI_RDI] ]
                        , [ Prototype "SCASQ" [] , Prototype "SCAS" [OpM64_ES_DI_EDI_RDI] , Prototype "SCAS" [OpRAX,OpM64_ES_DI_EDI_RDI] ]

                        , [ Prototype "XLATB" [] , Prototype "XLAT" [OpM8_DS_BX_EBX_RBX] ]
                        ]

makeDef :: Opcode -> Prototype -> OperandSpecList -> Bool -> Bool -> String -> EncodedInstructionDefinition
makeDef = EncodedInstructionDefinition

opcode :: EncodedInstructionDefinition -> Opcode
opcode = encdefOpcode

prefix :: EncodedInstructionDefinition -> Prefix.Set
prefix = opcodePrefix . encdefOpcode

isRex :: EncodedInstructionDefinition -> Bool
isRex e = Prefix.hasRex $ prefix e

rexw :: EncodedInstructionDefinition -> Bool
rexw e =  Prefix.member Prefix.RexW $ prefix e

mnemonic :: EncodedInstructionDefinition -> String
mnemonic = protoMnemonic . encdefPrototype

valid64 :: EncodedInstructionDefinition -> Bool
valid64 = encdefValid64

validLeg :: EncodedInstructionDefinition -> Bool
validLeg = encdefValidLeg

description :: EncodedInstructionDefinition -> String
description = encdefDescription

operands :: EncodedInstructionDefinition -> [ OperandType ]
operands = protoOperands . encdefPrototype

protoType :: EncodedInstructionDefinition -> Prototype
protoType =  encdefPrototype

operandSpecList :: EncodedInstructionDefinition -> OperandSpecList
operandSpecList =  encdefOperandSpec


operandEncList :: EncodedInstructionDefinition -> [ Maybe OperandEnc ]
operandEncList =  (map $ maybe Nothing $ Just . fst ) .  encdefOperandSpec

operandList :: EncodedInstructionDefinition 
            -> [ (OperandType,Maybe OperandEnc,Maybe OperandAccess) ]
operandList e = let ots = operands e
                    oss = map (maybe (Nothing,Nothing) (\(x,y) -> (Just x,Just y) )) $ operandSpecList e
                    zip' = zipWith (\x (y,z) -> (x,y,z))
                in zip' ots oss

operandList2 :: EncodedInstructionDefinition -> [ (OperandType,OperandEnc) ]
operandList2 e = map (\(x,y,z) -> (x,fromj y)) $ filter (isJust . snd' ) $ operandList e
  where snd' (x,y,z) = y
        fromj (Just x) = x
        fromj Nothing  = ERROR((show  e))


operandSpecList2 :: EncodedInstructionDefinition
                 -> [(Maybe OperandType,Maybe OperandEnc,Maybe OperandAccess)]
operandSpecList2 e = let mops  = map Just (operands e) ++ repeat Nothing
                         specs = operandSpecList e
                     in zipWith f mops specs
    where
      f (Just x) (Just (y,z)) = (Just x,Just y,Just z)
      f (Just x) Nothing      = (Just x,Nothing,Nothing)
      f Nothing  (Just (y,z)) = (Nothing,Just y,Just z)
      f Nothing  Nothing      = (Nothing,Nothing,Nothing)

                     

hasRegAdd :: EncodedInstructionDefinition -> Bool
hasRegAdd e
  | (Just _) <- regAdd e  = True
  | otherwise = False

regAdd :: EncodedInstructionDefinition -> Maybe RegAdd
regAdd = opcodeRegAdd . opcode

modrm :: EncodedInstructionDefinition -> Maybe ModrmType
modrm = opcodeModrm . opcode

needModrm :: EncodedInstructionDefinition -> Bool
needModrm = isJust . modrm

modrmDigit :: EncodedInstructionDefinition -> Maybe Int
modrmDigit e =
  case opcodeModrm $ opcode e of
    Just (ModrmDigit i) -> Just i
    _                   -> Nothing

opcodeBytes :: EncodedInstructionDefinition -> [ Byte ]
opcodeBytes = opcodeCode . opcode

opcodeLength :: EncodedInstructionDefinition -> Int
opcodeLength = length . opcodeBytes

imms :: EncodedInstructionDefinition -> [ IMMType ]
imms = opcodeImms . opcode

immSize :: EncodedInstructionDefinition -> Int
immSize e = sum $ map size $ imms e

-- End accessors

xmmOpSet = Set.fromList [ OpXMM , OpXMM_M128 , OpXMM0 , OpXMM_M64 , OpXMM_M32 , OpXMM_M16 ]
mmOpSet = Set.fromList [ OpMM , OpMM_M64 , OpMM_M32 ]

isXmmInstruction :: EncodedInstructionDefinition -> Bool
isXmmInstruction e = not $ Set.null $ Set.intersection xmmOpSet (Set.fromList $  operands e)

isMmInstruction :: EncodedInstructionDefinition -> Bool
isMmInstruction e = not $ Set.null $ Set.intersection mmOpSet (Set.fromList $ operands e)


operandSpecFromList :: [OperandSpec] -> OperandSpecList
operandSpecFromList = map Just


insertPrefix :: Prefix.Prefix -> EncodedInstructionDefinition  -> EncodedInstructionDefinition
insertPrefix p (EncodedInstructionDefinition op proto enc v64 vl desc)
  = EncodedInstructionDefinition (insertPrefixToOpcode p op ) proto enc v64 vl desc

insertPrefixToOpcode :: Prefix.Prefix -> Opcode -> Opcode
insertPrefixToOpcode p o = o { opcodePrefix = p' }
    where
      p' = (Prefix.insert p pre)
      pre = opcodePrefix o

emptyOperandSpec :: OperandSpecList
emptyOperandSpec =  replicate 4 Nothing

exPush1 = EncodedInstructionDefinition
          (emptyOpcode { opcodeCode = [0x0F,0xA0] })
          (Prototype "PUSH" [OpFS])
          emptyOperandSpec
          True  True "Push FS and decrement stack pointer by 16 bits."

exPush2 = EncodedInstructionDefinition
          (emptyOpcode { opcodeCode = [0x0F,0xA0] })
          (Prototype "PUSH"  [OpFS])
          emptyOperandSpec
          True  False "Push FS. Default operand size 64-bits. (66H override causes 16-bit operation)."

ASSERT_EQ((aliasDefinitions exPush1 exPush2),(False))


aliasDefinitions :: EncodedInstructionDefinition
                 -> EncodedInstructionDefinition -> Bool
aliasDefinitions e e' =  (aliasMnemonic `on` mnemonic) e e'
                         && ((==) `on` prefix_op_proto_spec)  e e'
                        ||
                        (aliasPrototype `on` protoType)  e e'
                         && ((==) `on` prefix_op_spec)  e e'
    where
      prefix_op_proto_spec e = (opcode e 
                               , operands e
                               , operandSpecList e
                               , valid64 e
                               , validLeg e)
      prefix_op_spec e = (opcode e 
                         , valid64 e
                         , validLeg e)




prop_replaceOperandType  e = replaceOperandType (const Nothing) e == e
prop_replaceOperandType2 e = replaceOperandType (Just . id) e == e

replaceOperandType :: (OperandType -> Maybe OperandType)
                      -> EncodedInstructionDefinition
                      -> EncodedInstructionDefinition
replaceOperandType f e = updateOperands ops e
  where ops = mapMaybe (\x -> f x `mplus` Just x ) $ operands e


assumeOperandReadWrite :: [ OperandEnc ] ->  OperandSpecList
assumeOperandReadWrite = map $ Just . ( flip (,) OperandReadWrite )

updateMnemonic :: String -> EncodedInstructionDefinition -> EncodedInstructionDefinition
updateMnemonic m e = e { encdefPrototype = p' }
  where p' = (encdefPrototype e) { protoMnemonic = m }

updateOpcode :: Opcode ->  EncodedInstructionDefinition -> EncodedInstructionDefinition
updateOpcode o e = e { encdefOpcode = o }

updatePrefix :: Prefix.Set -> EncodedInstructionDefinition -> EncodedInstructionDefinition
updatePrefix p e = updateOpcode o e
  where o = (opcode e) { opcodePrefix = p }

updateOpcodeBytes :: [Byte] -> EncodedInstructionDefinition -> EncodedInstructionDefinition
updateOpcodeBytes bs e = updateOpcode opcode' e
  where opcode' = (encdefOpcode e) { opcodeCode = bs }

updateOpcodeModrm :: Maybe ModrmType -> EncodedInstructionDefinition -> EncodedInstructionDefinition
updateOpcodeModrm m e = updateOpcode opcode' e
  where opcode' = (encdefOpcode e) { opcodeModrm = m }


updateImm :: [IMMType] -> EncodedInstructionDefinition -> EncodedInstructionDefinition
updateImm is e = updateOpcode o e
  where o = (opcode e) { opcodeImms = is }

updatePrototype :: Prototype -> EncodedInstructionDefinition -> EncodedInstructionDefinition
updatePrototype p e = e { encdefPrototype = p }

prop_updateOperands os e  = operands (updateOperands os e) == os

updateOperands :: [OperandType] -> EncodedInstructionDefinition
               -> EncodedInstructionDefinition
updateOperands ps e  = updatePrototype (Prototype m ps) e
  where m = mnemonic e

updateOperandList ::  [(OperandType , Maybe (OperandEnc , OperandAccess))] -> EncodedInstructionDefinition -> EncodedInstructionDefinition
updateOperandList os e = updateOperands (map fst os) $ updateOperandSpecList (map snd os) e

updateOperandList2 :: [(OperandType,OperandEnc)] -> EncodedInstructionDefinition -> EncodedInstructionDefinition
updateOperandList2 os e = updateOperandList (map (\(x,y) -> (x,Just (y,OperandReadWrite))) os) e


updateOperandSpecList :: OperandSpecList -> EncodedInstructionDefinition -> EncodedInstructionDefinition
updateOperandSpecList xs e = e { encdefOperandSpec = xs }

updateOperandSpecAt :: Int -> OperandSpec -> EncodedInstructionDefinition -> EncodedInstructionDefinition
updateOperandSpecAt n x e = updateOperandSpecList xs e
  where xs = updateListAt n (Just x) (operandSpecList e)
        updateListAt n x xs = take n xs ++ [x] ++ drop (n+1) xs

updateOperandEnc :: [ OperandEnc ] -> EncodedInstructionDefinition -> EncodedInstructionDefinition
updateOperandEnc es e =
  let s  = encdefOperandSpec e
      accs = map (maybe OperandReadWrite snd) s
      s' =  zipWith (\x y -> Just (x,y)) es (accs ++ repeat OperandReadWrite)
  in updateOperandSpecList s' e


updateValidLeg :: Bool -> EncodedInstructionDefinition -> EncodedInstructionDefinition
updateValidLeg b e = e { encdefValidLeg = b }
updateValid64 :: Bool -> EncodedInstructionDefinition -> EncodedInstructionDefinition
updateValid64 b e = e { encdefValid64 = b }




floatingPointInstructionSet :: Set String
floatingPointInstructionSet =
  Set.fromList [ "FSTSW"
               , "FNSTSW"
               ]

isFloatingPointInstruction :: String -> Bool
isFloatingPointInstruction ('F':_) = True
isFloatingPointInstruction _       = False

isFixedAddressInstruction :: String -> Bool
isFixedAddressInstruction n = elem n $
                              [ "ENTER" ,"RET", "LLDT" ,"LMSW" ,"LTR" ,"SLDT","STR" ,"VERR","VERW"]

protoTypeOperandSize :: EncodedInstructionDefinition -> Maybe OperandSize
protoTypeOperandSize e
  | isFloatingPointInstruction (name e)     = Nothing
  | isFixedAddressInstruction  (name e)     = Nothing
  | Prototype "ARPL"  [OpRM16,OpR16] <- proto  = Nothing

  | (Prototype "LEAVE" [],True,True) <- (proto,b64,bl) = Just Op16
  | (Prototype "LEAVE" [],False,True) <- (proto,b64,bl) = Just Op32
  | (Prototype "LEAVE" [],True,False) <- (proto,b64,bl) = Just Op32

  | (Prototype "POP" [OpFS],True,True) <- (proto,b64,bl) = Just Op16
  | (Prototype "POP" [OpFS],False,True) <- (proto,b64,bl) = Just Op32
  | (Prototype "POP" [OpFS],True,False) <- (proto,b64,bl) = Nothing

  | (Prototype "POP" [OpGS],True,True) <- (proto,b64,bl) = Just Op16
  | (Prototype "POP" [OpGS],False,True) <- (proto,b64,bl) = Just Op32
  | (Prototype "POP" [OpGS],True,False) <- (proto,b64,bl) = Nothing


  | (Prototype "PUSH" [OpFS],True,True) <- (proto,b64,bl) = Just Op16
  | (Prototype "PUSH" [OpFS],False,True) <- (proto,b64,bl) = Just Op32
  | (Prototype "PUSH" [OpFS],True,False) <- (proto,b64,bl) = Nothing

  | (Prototype "PUSH" [OpGS],True,True) <- (proto,b64,bl) = Just Op16
  | (Prototype "PUSH" [OpGS],False,True) <- (proto,b64,bl) = Just Op32
  | (Prototype "PUSH" [OpGS],True,False) <- (proto,b64,bl) = Nothing

  | Just n <- guessOperandSizeByPrototype e =
    case n of
      2 -> Just Op16
      4 -> Just Op32
      _ -> Nothing

  | rexw e = Nothing
  | isXmmInstruction e = Nothing
  | isMmInstruction  e = Nothing
  | elem (mnemonic e) ["CRC32"] 
      = case  minimum $ catMaybes $ map opSize $ operands e of
          2 -> Just Op16
          4 -> Just Op32
          _ -> Nothing
  | otherwise 
      = case maximum $ 0 : (catMaybes $ map opSize $ operands e) of
          2 -> Just Op16
          4 -> Just Op32
          _ -> Nothing

  where
    proto = protoType e
    b64   = valid64 e
    bl    = validLeg e


guessOperandSizeByPrototype :: EncodedInstructionDefinition -> Maybe Int
guessOperandSizeByPrototype e = Map.lookup (protoType e) opSizeProtoMap

opSizeProtoMap :: Map Prototype Int
opSizeProtoMap = Map.fromList [ (Prototype "INSB" [],1)
                              , (Prototype "INSW" [],2)
                              , (Prototype "INSD" [],4)
                              , (Prototype "OUTSB" [],1)
                              , (Prototype "OUTSW" [],2)
                              , (Prototype "OUTSD" [],4)

                              , (Prototype "MOVSB" [],1)
                              , (Prototype "MOVSW" [],2)
                              , (Prototype "MOVSD" [],4)
                              , (Prototype "MOVSQ" [],8)

                              , (Prototype "CMPSB" [],1)
                              , (Prototype "CMPSW" [],2)
                              , (Prototype "CMPSD" [],4)
                              , (Prototype "CMPSQ" [],8)

                              , (Prototype "STOSB" [],1)
                              , (Prototype "STOSW" [],2)
                              , (Prototype "STOSD" [],4)
                              , (Prototype "STOSQ" [],8)

                              , (Prototype "LODSB" [],1)
                              , (Prototype "LODSW" [],2)
                              , (Prototype "LODSD" [],4)
                              , (Prototype "LODSQ" [],8)

                              , (Prototype "SCASB" [],1)
                              , (Prototype "SCASW" [],2)
                              , (Prototype "SCASD" [],4)
                              , (Prototype "SCASQ" [],8)

                              , (Prototype "POPA" [],2)
                              , (Prototype "POPAD" [],4)
                              , (Prototype "PUSHA" [],2)
                              , (Prototype "PUSHAD" [],4)
                              , (Prototype "POPF" [],2)
                              , (Prototype "POPFD" [],4)

                              -- POPFQ requires Rex.W ,but PUSHFQ doesn't ?
                              --  | Prototype "POPFQ" [] <- proto = Just Op32

                              , (Prototype "PUSHF" [],2)
                              , (Prototype "PUSHFD" [],4)
                              -- not a bug,see the reference
                              , (Prototype "PUSHFQ" [],4) 

                              , (Prototype "CBW" [],2)
                              , (Prototype "CWDE" [],4)
                              , (Prototype "CDQE" [],8)

                              , (Prototype "CWD" [],2)
                              , (Prototype "CDQ" [],4)
                              , (Prototype "CQO" [],8)

                              , (Prototype "IRET" [],2)
                              , (Prototype "IRETD" [],4)
                              , (Prototype "IRETQ" [],8)

                              , (Prototype "MOV"   [OpRM16, OpSReg ] , 0)
                              , (Prototype "MOV"   [OpSReg, OpRM16 ] , 0)

                              ]



protoTypeAddressSize :: EncodedInstructionDefinition -> Maybe AddressSize
protoTypeAddressSize e = protoAddressSize (protoType e)

protoAddressSize :: Prototype -> Maybe AddressSize
protoAddressSize (Prototype "JRCXZ" _) = Just Addr64
protoAddressSize (Prototype "JECXZ" _) = Just Addr32
protoAddressSize (Prototype "JCXZ" _) = Just Addr16
protoAddressSize _ = Nothing
