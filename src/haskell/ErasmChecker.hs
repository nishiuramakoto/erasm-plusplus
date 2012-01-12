--  Copyright (C) 2011,2012 Makoto Nishiura.

--  This file is part of ERASM++.

--  ERASM++ is free software; you can redistribute it and/or modify it under
--  the terms of the GNU General Public License as published by the Free
--  Software Foundation; either version 3, or (at your option) any later
--  version.

--  ERASM++ is distributed in the hope that it will be useful, but WITHOUT ANY
--  WARRANTY; without even the implied warranty of MERCHANTABILITY or
--  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
--  for more details.

--  You should have received a copy of the GNU General Public License
--  along with ERASM++; see the file COPYING3.  If not see
--  <http://www.gnu.org/licenses/>.  

{-# OPTIONS_GHC -F -pgmF preprocess  #-}
--m4_include(my_ghc_testframework.m4)

{-# LANGUAGE PatternGuards #-}

module Main where
import ErasmCommon
import ErasmParseCommon
import Test.HUnit
import Test.QuickCheck hiding ((.&.))

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language


import Data.Map(Map)
import qualified Data.Map as Map
import Data.Int
import Data.Word
import Data.Bits
import Numeric
import Polynomial hiding (var)
import qualified Polynomial as Poly
import EncodedInstructionDefinition(aliasMnemonic)
import qualified EncodedInstructionDefinition as Encoded
import qualified Prefix

import System.Environment
import System.IO
import System.Exit


type IntType = Int8

type RegPoly = GrevlexPoly IntType String
type RegMon  = Grevlex String
data SegmentedPoly = SPoly Segment RegPoly
                   deriving (Ord,Show)


data Operand = RegOp Register
             | IntOp IntType
             | AddrOp Integer
             | PtrOp OperandType SegmentedPoly
             | FarPtrOp Integer Integer
             deriving (Show,Ord,Eq)

var :: String -> RegPoly
var x = Poly.var x

monomial1 :: String -> RegMon
monomial1 = lm . var

rsi = var "rsi"
rdi = var "rdi"
rbx = var "rbx"
esi = var "esi"
edi = var "edi"
ebx = var "ebx"
si = var "si"
di = var "di"
bx = var "bx"

ds_rsi = SPoly DS rsi
es_rdi = SPoly ES rdi
ds_rbx = SPoly DS rbx

ds_esi = SPoly DS esi
es_edi = SPoly ES edi
ds_ebx = SPoly DS ebx

ds_si = SPoly DS si
es_di = SPoly ES di
ds_bx = SPoly DS bx

ds_rsi_esi X64 = ds_rsi
ds_rsi_esi X86 = ds_esi

es_rdi_edi X64 = es_rdi
es_rdi_edi X86 = es_edi

ds_rbx_ebx X64 = ds_rbx
ds_rbx_ebx X86 = ds_ebx

instance Eq SegmentedPoly where
  (SPoly s p) == (SPoly t q) = s' == t' && p == q
    where s' = seg s p
          t' = seg t q
          seg DefaultSegment p = defaultSegment p
          seg t p = t



newtype Mnemonic = Mnemonic String
        deriving (Ord,Show)

instance Eq Mnemonic where
  (Mnemonic x) == (Mnemonic y) =  x == y || aliasMnemonic x y

instance Named Mnemonic where
  name (Mnemonic s) = s

data Assembly= Assembly Mnemonic [Operand] Prefix.Set
               deriving (Show)

-- instance Eq Assembly where
--   (==) = eqAsm' `on` (canonicalizeAsm . stripAsmPrefix)

asmEq :: Mode -> Assembly -> Assembly -> Bool
asmEq mode = eqAsm' `on` (canonicalizeAsm mode . stripAsmPrefix)


instance Named Assembly where
  name (Assembly m _ _) = name m

updateAsmMnemonic :: String -> Assembly -> Assembly
updateAsmMnemonic m (Assembly (Mnemonic _) ops p) = Assembly (Mnemonic m) ops p

rexw = Prefix.member Prefix.RexW

fixOperandSize :: OperandType -> Assembly -> Assembly
fixOperandSize t (Assembly m ops p) = Assembly m (map (fixOperandSize' t) ops) p

fixOperandSize' :: OperandType -> Operand -> Operand
fixOperandSize' t (RegOp x)
  | isWordReg x || isDwordReg x || isQwordReg x  = RegOp $ conv t x
    where conv Word  = toWordReg
          conv Dword = toDwordReg
          conv Qword = toQwordReg
          conv _ = UNIMPLEMENTED

fixOperandSize' t (PtrOp t' p) = PtrOp t p
fixOperandSize' t x = x

fixDsmBug :: Mode -> Assembly -> Assembly
fixDsmBug mode f@(Assembly m  [RegOp RegST0,RegOp RegST0] p) = f

fixDsmBug mode (Assembly m@(Mnemonic "aad") ops@[IntOp 0xa] p) = Assembly m [] p
fixDsmBug mode (Assembly m@(Mnemonic "aam") ops@[IntOp 0xa] p) = Assembly m [] p

fixDsmBug X86 (Assembly m@(Mnemonic "cmpless") ops  p) = Assembly (Mnemonic "cmpneqss") ops p
fixDsmBug X86 (Assembly m@(Mnemonic "cmpnless") ops  p) = Assembly (Mnemonic "cmpordss") ops p

fixDsmBug X86 (Assembly m@(Mnemonic "callw") ops  p) = Assembly (Mnemonic "call") ops p
fixDsmBug X86 (Assembly m@(Mnemonic "jmpw")  ops  p) = Assembly (Mnemonic "jmp") ops p

fixDsmBug mode (Assembly m@(Mnemonic "call") ops@[PtrOp Fword x] p) | rexw p  = Assembly m [PtrOp Tbyte x] p

fixDsmBug mode (Assembly (Mnemonic "fdivr")  ops@[x,RegOp RegST0] p) = Assembly (Mnemonic "fdiv")   ops p
fixDsmBug mode (Assembly (Mnemonic "fdiv" )  ops@[x,RegOp RegST0] p) = Assembly (Mnemonic "fdivr")   ops p
fixDsmBug mode (Assembly (Mnemonic "fdivrp") ops@[x,RegOp RegST0] p) = Assembly (Mnemonic "fdivp")  ops p
fixDsmBug mode (Assembly (Mnemonic "fdivp")  ops@[x,RegOp RegST0] p) = Assembly (Mnemonic "fdivrp")  ops p
fixDsmBug mode (Assembly (Mnemonic "fsub" )  ops@[x,RegOp RegST0] p) = Assembly (Mnemonic "fsubr")   ops p
fixDsmBug mode (Assembly (Mnemonic "fsubp" )  ops@[x,RegOp RegST0] p) = Assembly (Mnemonic "fsubrp")   ops p
fixDsmBug mode (Assembly (Mnemonic "fsubr" )  ops@[x,RegOp RegST0] p) = Assembly (Mnemonic "fsub")   ops p
fixDsmBug mode (Assembly (Mnemonic "fsubrp" ) ops@[x,RegOp RegST0] p) = Assembly (Mnemonic "fsubp")   ops p
fixDsmBug mode (Assembly (Mnemonic "iretw") [] p) = Assembly (Mnemonic "iret") [] Prefix.empty
fixDsmBug mode (Assembly (Mnemonic "iret")  [] p)
  | Prefix.null p  =  Assembly (Mnemonic "iretd") [] Prefix.empty
  | otherwise      =  Assembly (Mnemonic "iretq") [] Prefix.empty

fixDsmBug mode (Assembly m@(Mnemonic "jmp") [PtrOp Fword x] p)
  | Prefix.member Prefix.RexW p  = Assembly m [PtrOp Tbyte x] Prefix.empty
fixDsmBug mode (Assembly m@(Mnemonic mne) [RegOp x,PtrOp Fword y] p )
  | elem mne [ "lfs" , "lgs","lss" ]
  , isQwordReg x  = Assembly m [RegOp x,PtrOp Tbyte y] p

--fixDsmBug mode (Assembly m@(Mnemonic "lods") [x,y] p) = Assembly m [y] p

fixDsmBug mode f@(Assembly m@(Mnemonic "mov") [RegOp x,RegOp y] p)
  | isDwordReg x && isSegReg y  = fixOperandSize Word f
  | isDwordReg y && isSegReg x  = fixOperandSize Word f
  | isWordReg x && isSegReg y && rexw p = fixOperandSize Qword f
  | isWordReg y && isSegReg x && rexw p = fixOperandSize Qword f

fixDsmBug mode f@(Assembly m@(Mnemonic "mov") [PtrOp Word x,RegOp y] p)
  | isSegReg y && rexw p = fixOperandSize Qword f

fixDsmBug mode f@(Assembly m@(Mnemonic "mov") [RegOp x, PtrOp Word y] p)
  | isSegReg x && rexw p = fixOperandSize Qword f

fixDsmBug X64 f@(Assembly m@(Mnemonic "movmskpd") [RegOp x,RegOp y] p) = fixOperandSize Qword f
fixDsmBug X64 f@(Assembly m@(Mnemonic "movmskps") [RegOp x,RegOp y] p) = fixOperandSize Qword f

fixDsmBug mode (Assembly m@(Mnemonic mne) [RegOp x,PtrOp Void (SPoly seg y)] p)
  | elem mne ["mov","movabs"]
  , Just t <- lookup x [(RegAL,Byte),(RegAX,Word),(RegEAX,Dword),(RegRAX,Qword) ]
   =  Assembly (Mnemonic "mov") [RegOp x , PtrOp t (SPoly seg' y)] Prefix.empty
      where seg' | seg == DS = DefaultSegment
                 | otherwise = seg

fixDsmBug mode (Assembly m@(Mnemonic mne) [PtrOp Void (SPoly seg y),RegOp x] p)
  | elem mne ["mov","movabs"]
  , Just t <- lookup x [(RegAL,Byte),(RegAX,Word),(RegEAX,Dword),(RegRAX,Qword) ]
   =  Assembly (Mnemonic "mov") [PtrOp t (SPoly seg' y),RegOp x] Prefix.empty
      where seg' | seg == DS = DefaultSegment
                 | otherwise = seg

fixDsmBug mode f | name f == "movabs"  = updateAsmMnemonic "mov" f

fixDsmBug X64 f@(Assembly (Mnemonic "pextrb") [RegOp x,y,z] p) = fixOperandSize Qword f

fixDsmBug X64 f@(Assembly (Mnemonic "pmovmskb") [RegOp x,y] p) | rexw p = fixOperandSize Qword f
fixDsmBug X64 f@(Assembly (Mnemonic "pmovmskb") [RegOp x,RegOp y] p) | isXmmReg y = fixOperandSize Qword f


fixDsmBug X86 f@(Assembly (Mnemonic "lgdtd") ps p)  = updateAsmMnemonic "lgdt" f
fixDsmBug X86 f@(Assembly (Mnemonic "lidtd") ps p)  = updateAsmMnemonic "lidt" f
fixDsmBug X86 f@(Assembly (Mnemonic "sgdtd") ps p)  = updateAsmMnemonic "sgdt" f
fixDsmBug X86 f@(Assembly (Mnemonic "sidtd") ps p)  = updateAsmMnemonic "sidt" f

fixDsmBug _ f@(Assembly (Mnemonic "popw") ps p)   = updateAsmMnemonic "pop" f

fixDsmBug _ f@(Assembly (Mnemonic "popfw") ps p)   = updateAsmMnemonic "popf" f
fixDsmBug _ f@(Assembly (Mnemonic "popaw") ps p)   = updateAsmMnemonic "popa" f

fixDsmBug X86 f@(Assembly (Mnemonic "popf") ps p)   = updateAsmMnemonic "popfd" f
fixDsmBug X86 f@(Assembly (Mnemonic "popa") ps p)   = updateAsmMnemonic "popad" f

fixDsmBug _ f@(Assembly (Mnemonic "pushfw") ps p)   = updateAsmMnemonic "pushf" f
fixDsmBug _ f@(Assembly (Mnemonic "pushaw") ps p)   = updateAsmMnemonic "pusha" f

fixDsmBug X86 f@(Assembly (Mnemonic "pushf") ps p)   = updateAsmMnemonic "pushfd" f
fixDsmBug X86 f@(Assembly (Mnemonic "pusha") ps p)   = updateAsmMnemonic "pushad" f

fixDsmBug X64 f@(Assembly (Mnemonic "pushf") ps p)   = updateAsmMnemonic "pushfq" f
fixDsmBug X64 f@(Assembly (Mnemonic "popf") ps p) | rexw p  = updateAsmMnemonic "popfq" f


fixDsmBug mode f | name f == "pclmulhqhqdq" = updateAsmMnemonic "pclmulhqhdq" f
fixDsmBug mode f | name f == "pclmullqhqdq" = updateAsmMnemonic "pclmullqhdq" f

fixDsmBug mode f = f


stripAsmPrefix (Assembly m@(Mnemonic mne) ops p)
  | Just mne' <- stripPrefix "rep_" mne =
    Assembly (Mnemonic mne') ops (Prefix.insert Prefix.REP p)
  | Just mne' <- stripPrefix "repe_" mne =
    Assembly (Mnemonic mne') ops (Prefix.insert Prefix.REP p)
  | Just mne' <- stripPrefix "repz_" mne =
    Assembly (Mnemonic mne') ops (Prefix.insert Prefix.REP p)
  | Just mne' <- stripPrefix "repne_" mne =
    Assembly (Mnemonic mne') ops (Prefix.insert Prefix.REPNE p)
  | Just mne' <- stripPrefix "repnz_" mne =
    Assembly (Mnemonic mne') ops (Prefix.insert Prefix.REPNE p)

stripAsmPrefix a = a

canonicalizeAsm :: Mode -> Assembly -> Assembly

canonicalizeAsm mode (Assembly m@(Mnemonic "aad") ops@[IntOp 0xa] p) = Assembly m [] p
canonicalizeAsm mode (Assembly m@(Mnemonic "aam") ops@[IntOp 0xa] p) = Assembly m [] p

canonicalizeAsm mode (Assembly (Mnemonic mne) [x,y] p)
  | ("cmp",inf,suf) <- decompose mne
  , elem suf ["pd","ps","sd","ss"]
  , (Just n) <- findIndex (== inf) [ "eq","lt","le","unord","neq","nlt","nle","ord" ] =
    Assembly (Mnemonic ("cmp" ++ suf)) [x,y,IntOp (fromInteger $ toInteger n)] p

      where decompose m | (p,p0)  <- splitAt (length m -2) m
                        , (p2,p1) <- splitAt 3 p = (p2,p1,p0)


canonicalizeAsm mode (Assembly m@(Mnemonic "sldt") [RegOp x] p) = Assembly m [RegOp $ toWordReg x] p
canonicalizeAsm mode (Assembly m@(Mnemonic "str")  [RegOp x] p) = Assembly m [RegOp $ toWordReg x] p
canonicalizeAsm mode (Assembly m@(Mnemonic "lar")  [RegOp x,RegOp y] p) = Assembly m [RegOp x,RegOp $ toDwordReg y] p
canonicalizeAsm mode (Assembly m@(Mnemonic "lsl")  [RegOp x,RegOp y] p) = Assembly m [RegOp x,RegOp $ toDwordReg y] p

canonicalizeAsm mode (Assembly m@(Mnemonic mne)  [PtrOp _ e] p)
  | elem mne [ "lgdt","lidt" ,"invlpg" ] = Assembly m [PtrOp Void e] p

canonicalizeAsm mode (Assembly m@(Mnemonic "movnti")   [PtrOp Qword x,RegOp y] p) = Assembly m [PtrOp Dword x,RegOp y] p
canonicalizeAsm mode (Assembly m@(Mnemonic mne) [RegOp x,y,z] p)
  | elem mne ["pextrw","extractps"] =  Assembly m [RegOp $ toQwordReg x,y,z] p

canonicalizeAsm mode (Assembly m@(Mnemonic mne) op@[RegOp RegST0,RegOp RegST0] p)
  | Just mne' <- lookup mne [ ("fsubrp","fsubp") , ("fdivp","fdivrp") ]
                    = Assembly (Mnemonic mne') op p


canonicalizeAsm mode (Assembly m@(Mnemonic mne) [] p)
  | mne == "faddp"  = Assembly m [st1,st0] p
  | mne == "fcom"   = Assembly m [st1] p
  | mne == "fcomp"  = Assembly m [st1] p
  | mne == "fdivp"  = Assembly m [st1,st0] p
  | mne == "fdivrp" = Assembly m [st1,st0] p
  | mne == "fmulp"  = Assembly m [st1,st0] p
  | mne == "fsubp"  = Assembly m [st1,st0] p
  | mne == "fsubrp"  = Assembly m [st1,st0] p
  | mne == "fucom"  = Assembly m [st1] p
  | mne == "fucomp"  = Assembly m [st1] p
  | mne == "fxch"    = Assembly m [st1] p
    where (st0,st1) = (RegOp RegST0,RegOp RegST1)


canonicalizeAsm mode (Assembly m@(Mnemonic "lods") [PtrOp Byte f] p) = Assembly (Mnemonic "lods") [RegOp RegAL,PtrOp Byte f] p
canonicalizeAsm mode (Assembly m@(Mnemonic "lods") [PtrOp Word f] p) = Assembly (Mnemonic "lods") [RegOp RegAX,PtrOp Word f] p
canonicalizeAsm mode (Assembly m@(Mnemonic "lods") [PtrOp Dword f] p) = Assembly (Mnemonic "lods") [RegOp RegEAX,PtrOp Dword f] p
canonicalizeAsm mode (Assembly m@(Mnemonic "lods") [PtrOp Qword f] p) = Assembly (Mnemonic "lods") [RegOp RegRAX,PtrOp Qword f] p

canonicalizeAsm mode (Assembly m@(Mnemonic "scas") [PtrOp Byte f] p) = Assembly (Mnemonic "scas") [RegOp RegAL,PtrOp Byte f] p
canonicalizeAsm mode (Assembly m@(Mnemonic "scas") [PtrOp Word f] p) = Assembly (Mnemonic "scas") [RegOp RegAX,PtrOp Word f] p
canonicalizeAsm mode (Assembly m@(Mnemonic "scas") [PtrOp Dword f] p) = Assembly (Mnemonic "scas") [RegOp RegEAX,PtrOp Dword f] p
canonicalizeAsm mode (Assembly m@(Mnemonic "scas") [PtrOp Qword f] p) = Assembly (Mnemonic "scas") [RegOp RegRAX,PtrOp Qword f] p

canonicalizeAsm mode (Assembly m@(Mnemonic "stos") [PtrOp Byte f] p) = Assembly (Mnemonic "stos") [PtrOp Byte f,RegOp RegAL] p
canonicalizeAsm mode (Assembly m@(Mnemonic "stos") [PtrOp Word f] p) = Assembly (Mnemonic "stos") [PtrOp Word f,RegOp RegAX] p
canonicalizeAsm mode (Assembly m@(Mnemonic "stos") [PtrOp Dword f] p) = Assembly (Mnemonic "stos") [PtrOp Dword f,RegOp RegEAX] p
canonicalizeAsm mode (Assembly m@(Mnemonic "stos") [PtrOp Qword f] p) = Assembly (Mnemonic "stos") [PtrOp Qword f,RegOp RegRAX] p


canonicalizeAsm mode (Assembly (Mnemonic "cmpsb") [] p) = Assembly (Mnemonic "cmps") [PtrOp Byte  (ds_rsi_esi mode) ,PtrOp Byte  (es_rdi_edi mode) ] p
canonicalizeAsm mode (Assembly (Mnemonic "cmpsw") [] p) = Assembly (Mnemonic "cmps") [PtrOp Word  (ds_rsi_esi mode) ,PtrOp Word  (es_rdi_edi mode) ] p
canonicalizeAsm mode (Assembly (Mnemonic "cmpsd") [] p) = Assembly (Mnemonic "cmps") [PtrOp Dword (ds_rsi_esi mode) ,PtrOp Dword (es_rdi_edi mode) ] p
canonicalizeAsm mode (Assembly (Mnemonic "cmpsq") [] p) = Assembly (Mnemonic "cmps") [PtrOp Qword (ds_rsi_esi mode) ,PtrOp Qword (es_rdi_edi mode) ] p


-- canonicalizeAsm mode (Assembly (Mnemonic "cmpsd") [x,y,IntOp 0] p) = Assembly (Mnemonic "cmpeqsd") [x,y] p
-- canonicalizeAsm mode (Assembly (Mnemonic "cmpsd") [x,y,IntOp 1] p) = Assembly (Mnemonic "cmpltsd") [x,y] p
-- canonicalizeAsm mode (Assembly (Mnemonic "cmpsd") [x,y,IntOp 2] p) = Assembly (Mnemonic "cmplesd") [x,y] p
-- canonicalizeAsm mode (Assembly (Mnemonic "cmpsd") [x,y,IntOp 3] p) = Assembly (Mnemonic "cmpunordsd") [x,y] p
-- canonicalizeAsm mode (Assembly (Mnemonic "cmpsd") [x,y,IntOp 4] p) = Assembly (Mnemonic "cmpneqsd") [x,y] p
-- canonicalizeAsm mode (Assembly (Mnemonic "cmpsd") [x,y,IntOp 5] p) = Assembly (Mnemonic "cmpnltsd") [x,y] p
-- canonicalizeAsm mode (Assembly (Mnemonic "cmpsd") [x,y,IntOp 6] p) = Assembly (Mnemonic "cmpnlesd") [x,y] p
-- canonicalizeAsm mode (Assembly (Mnemonic "cmpsd") [x,y,IntOp 7] p) = Assembly (Mnemonic "cmpordsd") [x,y] p

canonicalizeAsm mode (Assembly m@(Mnemonic "pushw") ops p) = Assembly (Mnemonic "push") ops p

canonicalizeAsm mode (Assembly m@(Mnemonic "insb") [] p) = Assembly (Mnemonic "ins") [PtrOp Byte (es_rdi_edi mode),RegOp RegDX] p
canonicalizeAsm mode (Assembly m@(Mnemonic "insw") [] p) = Assembly (Mnemonic "ins") [PtrOp Word (es_rdi_edi mode),RegOp RegDX] p
canonicalizeAsm mode (Assembly m@(Mnemonic "insd") [] p) = Assembly (Mnemonic "ins") [PtrOp Dword (es_rdi_edi mode),RegOp RegDX] p

canonicalizeAsm mode (Assembly m@(Mnemonic "outsb") [] p) = Assembly (Mnemonic "outs") [RegOp RegDX , PtrOp Byte (ds_rsi_esi mode)] p
canonicalizeAsm mode (Assembly m@(Mnemonic "outsw") [] p) = Assembly (Mnemonic "outs") [RegOp RegDX , PtrOp Word (ds_rsi_esi mode)] p
canonicalizeAsm mode (Assembly m@(Mnemonic "outsd") [] p) = Assembly (Mnemonic "outs") [RegOp RegDX , PtrOp Dword (ds_rsi_esi mode)] p

canonicalizeAsm mode (Assembly m@(Mnemonic "lodsb") [] p) = Assembly (Mnemonic "lods") [RegOp RegAL,PtrOp Byte (ds_rsi_esi mode)] p
canonicalizeAsm mode (Assembly m@(Mnemonic "lodsw") [] p) = Assembly (Mnemonic "lods") [RegOp RegAX,PtrOp Word (ds_rsi_esi mode)] p
canonicalizeAsm mode (Assembly m@(Mnemonic "lodsd") [] p) = Assembly (Mnemonic "lods") [RegOp RegEAX,PtrOp Dword (ds_rsi_esi mode)] p
canonicalizeAsm mode (Assembly m@(Mnemonic "lodsq") [] p) = Assembly (Mnemonic "lods") [RegOp RegRAX,PtrOp Qword (ds_rsi_esi mode)] p


canonicalizeAsm mode (Assembly m@(Mnemonic "movsb") [] p) = Assembly (Mnemonic "movs") [PtrOp Byte (es_rdi_edi mode) ,PtrOp Byte (ds_rsi_esi mode)] p
canonicalizeAsm mode (Assembly m@(Mnemonic "movsw") [] p) = Assembly (Mnemonic "movs") [PtrOp Word (es_rdi_edi mode) ,PtrOp Word (ds_rsi_esi mode)] p
canonicalizeAsm mode (Assembly m@(Mnemonic "movsd") [] p) = Assembly (Mnemonic "movs") [PtrOp Dword (es_rdi_edi mode) ,PtrOp Dword (ds_rsi_esi mode)] p
canonicalizeAsm mode (Assembly m@(Mnemonic "movsq") [] p) = Assembly (Mnemonic "movs") [PtrOp Qword (es_rdi_edi mode) ,PtrOp Qword (ds_rsi_esi mode)] p

canonicalizeAsm mode (Assembly m@(Mnemonic "scasb") [] p) = Assembly (Mnemonic "scas") [RegOp RegAL,PtrOp Byte (es_rdi_edi mode)] p
canonicalizeAsm mode (Assembly m@(Mnemonic "scasw") [] p) = Assembly (Mnemonic "scas") [RegOp RegAX,PtrOp Word (es_rdi_edi mode)] p
canonicalizeAsm mode (Assembly m@(Mnemonic "scasd") [] p) = Assembly (Mnemonic "scas") [RegOp RegEAX,PtrOp Dword (es_rdi_edi mode)] p
canonicalizeAsm mode (Assembly m@(Mnemonic "scasq") [] p) = Assembly (Mnemonic "scas") [RegOp RegRAX,PtrOp Qword (es_rdi_edi mode)] p

canonicalizeAsm mode (Assembly m@(Mnemonic "stosb") [] p) = Assembly (Mnemonic "stos") [PtrOp Byte (es_rdi_edi mode),RegOp RegAL] p
canonicalizeAsm mode (Assembly m@(Mnemonic "stosw") [] p) = Assembly (Mnemonic "stos") [PtrOp Word (es_rdi_edi mode),RegOp RegAX] p
canonicalizeAsm mode (Assembly m@(Mnemonic "stosd") [] p) = Assembly (Mnemonic "stos") [PtrOp Dword (es_rdi_edi mode),RegOp RegEAX] p
canonicalizeAsm mode (Assembly m@(Mnemonic "stosq") [] p) = Assembly (Mnemonic "stos") [PtrOp Qword (es_rdi_edi mode),RegOp RegRAX] p


--canonicalizeAsm mode (Assembly m@(Mnemonic "scas") [RegOp x,y] p) = Assembly m [y] p
--canonicalizeAsm mode (Assembly m@(Mnemonic "stos") [x,RegOp y] p) = Assembly m [x] p
--canonicalizeAsm mode (Assembly m@(Mnemonic "lods") [RegOp x,PtrOp t y] p) = Assembly m [PtrOp t y] p

canonicalizeAsm mode (Assembly m@(Mnemonic "pclmulqdq") [op1,op2,IntOp x] p) =
  case x0 of
    0x00 -> Assembly (Mnemonic "pclmullqlqdq") [op1,op2] p
    0x01 -> Assembly (Mnemonic "pclmulhqlqdq") [op1,op2] p
    0x10 -> Assembly (Mnemonic "pclmullqhdq" ) [op1,op2] p
    0x11 -> Assembly (Mnemonic "pclmulhqhdq" ) [op1,op2] p
    _ -> UNIMPLEMENTED
  where x0 = x .&. 0x11

canonicalizeAsm mode (Assembly m@(Mnemonic "xchg") [RegOp RegRAX,RegOp RegRAX] p)
  = Assembly (Mnemonic "nop") [] p
canonicalizeAsm mode (Assembly m@(Mnemonic "xchg") [RegOp RegEAX,RegOp RegEAX] p)
  = Assembly (Mnemonic "nop") [] p
canonicalizeAsm mode (Assembly m@(Mnemonic "xchg") [RegOp RegAX ,RegOp RegAX ] p)
  = Assembly (Mnemonic "nop") [] p
canonicalizeAsm mode (Assembly m@(Mnemonic "xchg") ops p) = Assembly m (sort ops) p

canonicalizeAsm mode (Assembly m@(Mnemonic "xlatb") [] p) = Assembly (Mnemonic "xlat") [PtrOp Byte (ds_rbx_ebx mode) ] p

canonicalizeAsm mode f = f

eqAsm' :: Assembly -> Assembly -> Bool
eqAsm' (Assembly (Mnemonic "bswap") [RegOp r] _)  (Assembly (Mnemonic "bswap") [RegOp s] _)
  | otherwise = elem (rn - sn) [0,8,-8]
    where [rn,sn] = map fromEnum [r,s]


eqAsm' (Assembly m ps p)  (Assembly n qs q) = m == n && ps == qs && mask p == mask q
  where mask p = Prefix.intersection p p0
        p0 = Prefix.fromList [Prefix.REPNE,Prefix.REP,Prefix.LOCK]





defaultSegment :: RegPoly -> Segment
defaultSegment p | ss_based = SS
                 | cs_based = CS
                 | otherwise = DS
                   where
                     ss_based = elem 1 $ map (\x -> coeff (monomial1 x) p ) [ "rsp","rbp","esp","ebp" ]
                     cs_based = elem 1 $ map (\x -> coeff (monomial1 x) p ) [ "rip" ]



-- instance Eq Operand where
--   (RegOp x) == (RegOp y) = x == y
--   (PtrOp x y z) == (PtrOp u v w) = u == x && y == v && z == w
--   (IntOp x) == (IntOp y) = or $ map (uncurry (==)) [(x,y),(signExtend x,y),(x , signExtend y) ]


ASSERT_EQ((signExtend 0x7f),(0x7f))
ASSERT_EQ((signExtend 0x80),(0xffffffffffffff80))

signExtend :: Integer -> Integer
signExtend x | - 0xff   <= x && x <= 0xff            = toInteger (fromInteger (toInteger (fromInteger x :: Int8) )  :: Word64)
             | - 0xffff <= x && x <= 0xffff          = toInteger (fromInteger (toInteger (fromInteger x :: Int16) ) :: Word64)
             | - 0xffffffff <= x && x <= 0xffffffff  = toInteger (fromInteger (toInteger (fromInteger x :: Int32) ) :: Word64)
             | otherwise = x

data AddressExpr = Map Register Int
                 deriving (Eq,Ord,Show)

data OperandType = Byte | Word | Dword | Qword | MmWord | XmmWord | Oword | Fword | Tbyte | Void
             deriving (Eq,Enum,Ord,Show)

data Segment = DefaultSegment | ES | CS | SS | DS | FS | GS
             deriving (Eq,Enum,Ord,Show)

data Register = RegAL
              | RegCL
              | RegDL
              | RegBL
              | RegAH
              | RegCH
              | RegDH
              | RegBH

              | RegSPL
              | RegBPL
              | RegSIL
              | RegDIL

              | RegR8L
              | RegR9L
              | RegR10L
              | RegR11L
              | RegR12L
              | RegR13L
              | RegR14L
              | RegR15L

              | RegAX
              | RegCX
              | RegDX
              | RegBX
              | RegSP
              | RegBP
              | RegSI
              | RegDI

              | RegR8W
              | RegR9W
              | RegR10W
              | RegR11W
              | RegR12W
              | RegR13W
              | RegR14W
              | RegR15W

              | RegEAX
              | RegECX
              | RegEDX
              | RegEBX
              | RegESP
              | RegEBP
              | RegESI
              | RegEDI


              | RegR8D
              | RegR9D
              | RegR10D
              | RegR11D
              | RegR12D
              | RegR13D
              | RegR14D
              | RegR15D

              | RegRIP
              | RegRAX
              | RegRCX
              | RegRDX
              | RegRBX
              | RegRSP
              | RegRBP
              | RegRSI
              | RegRDI

              | RegR8
              | RegR9
              | RegR10
              | RegR11
              | RegR12
              | RegR13
              | RegR14
              | RegR15

              | RegMM0
              | RegMM1
              | RegMM2
              | RegMM3
              | RegMM4
              | RegMM5
              | RegMM6
              | RegMM7
              | RegMM8
              | RegMM9
              | RegMM10
              | RegMM11
              | RegMM12
              | RegMM13
              | RegMM14
              | RegMM15

              | RegXMM0
              | RegXMM1
              | RegXMM2
              | RegXMM3
              | RegXMM4
              | RegXMM5
              | RegXMM6
              | RegXMM7

              | RegXMM8
              | RegXMM9
              | RegXMM10
              | RegXMM11
              | RegXMM12
              | RegXMM13
              | RegXMM14
              | RegXMM15

              | RegST0
              | RegST1
              | RegST2
              | RegST3
              | RegST4
              | RegST5
              | RegST6
              | RegST7

              | RegCR0
              | RegCR1
              | RegCR2
              | RegCR3
              | RegCR4
              | RegCR5
              | RegCR6
              | RegCR7
              | RegCR8

              | RegDR0
              | RegDR1
              | RegDR2
              | RegDR3
              | RegDR4
              | RegDR5
              | RegDR6
              | RegDR7

              | RegES
              | RegCS
              | RegSS
              | RegDS
              | RegFS
              | RegGS
              deriving (Show,Enum,Ord,Eq)

isWordReg :: Register -> Bool
isWordReg x = RegAX <= x && x <= RegR15W

isDwordReg :: Register -> Bool
isDwordReg x = RegEAX <= x && x <= RegR15D

isQwordReg :: Register -> Bool
isQwordReg x = RegRAX <= x && x<= RegR15

isSegReg :: Register -> Bool
isSegReg x = RegES <= x && x <= RegGS

isXmmReg :: Register -> Bool
isXmmReg x = RegXMM0 <= x && x <= RegXMM15

toWordReg :: Register -> Register
toWordReg x | isWordReg  x = x
            | isDwordReg x || isQwordReg x = toEnum $ (fromEnum x - fromEnum RegEAX) + fromEnum RegAX
            | otherwise = ERROR((show x))

toDwordReg :: Register -> Register
toDwordReg x | isWordReg x  = toEnum $ (fromEnum x - fromEnum RegAX)  + fromEnum RegEAX
             | isDwordReg x = x
             | otherwise = ERROR((show x))

toQwordReg :: Register -> Register
toQwordReg x | isDwordReg x = toEnum $ (fromEnum x - fromEnum RegEAX)  + fromEnum RegRAX
             | isQwordReg x = x
             | otherwise = ERROR((show x))


regDef :: Map String Register
regDef = Map.fromList $ map (\(x,y) -> (y,x)) $
         [ (RegAL,"al")
         , (RegCL,"cl")
         , (RegDL,"dl")
         , (RegBL,"bl")
         , (RegAH,"ah")
         , (RegCH,"ch")
         , (RegDH,"dh")
         , (RegBH,"bh")
         , (RegSPL,"spl")
         , (RegBPL,"bpl")
         , (RegSIL,"sil")
         , (RegDIL,"dil")
         , (RegR8L,"r8l")
         , (RegR9L,"r9l")
         , (RegR10L,"r10l")
         , (RegR11L,"r11l")
         , (RegR12L,"r12l")
         , (RegR13L,"r13l")
         , (RegR14L,"r14l")
         , (RegR15L,"r15l")
         , (RegR8L,"r8b")
         , (RegR9L,"r9b")
         , (RegR10L,"r10b")
         , (RegR11L,"r11b")
         , (RegR12L,"r12b")
         , (RegR13L,"r13b")
         , (RegR14L,"r14b")
         , (RegR15L,"r15b")
         , (RegAX,"ax")
         , (RegCX,"cx")
         , (RegDX,"dx")
         , (RegBX,"bx")
         , (RegSP,"sp")
         , (RegBP,"bp")
         , (RegSI,"si")
         , (RegDI,"di")
         , (RegR8W,"r8w")
         , (RegR9W,"r9w")
         , (RegR10W,"r10w")
         , (RegR11W,"r11w")
         , (RegR12W,"r12w")
         , (RegR13W,"r13w")
         , (RegR14W,"r14w")
         , (RegR15W,"r15w")
         , (RegEAX,"eax")
         , (RegECX,"ecx")
         , (RegEDX,"edx")
         , (RegEBX,"ebx")
         , (RegESP,"esp")
         , (RegEBP,"ebp")
         , (RegESI,"esi")
         , (RegEDI,"edi")
         , (RegR8D,"r8d")
         , (RegR9D,"r9d")
         , (RegR10D,"r10d")
         , (RegR11D,"r11d")
         , (RegR12D,"r12d")
         , (RegR13D,"r13d")
         , (RegR14D,"r14d")
         , (RegR15D,"r15d")

         , (RegRAX,"rax")
         , (RegRCX,"rcx")
         , (RegRDX,"rdx")
         , (RegRBX,"rbx")
         , (RegRSP,"rsp")
         , (RegRBP,"rbp")
         , (RegRSI,"rsi")
         , (RegRDI,"rdi")

         , (RegR8,"r8")
         , (RegR9,"r9")
         , (RegR10,"r10")
         , (RegR11,"r11")
         , (RegR12,"r12")
         , (RegR13,"r13")
         , (RegR14,"r14")
         , (RegR15,"r15")

         , (RegMM0,"mm0")
         , (RegMM1,"mm1")
         , (RegMM2,"mm2")
         , (RegMM3,"mm3")
         , (RegMM4,"mm4")
         , (RegMM5,"mm5")
         , (RegMM6,"mm6")
         , (RegMM7,"mm7")
         , (RegMM8,"mm8")
         , (RegMM9,"mm9")
         , (RegMM10,"mm10")
         , (RegMM11,"mm11")
         , (RegMM12,"mm12")
         , (RegMM13,"mm13")
         , (RegMM14,"mm14")
         , (RegMM15,"mm15")

         , (RegXMM0,"xmm0")
         , (RegXMM1,"xmm1")
         , (RegXMM2,"xmm2")
         , (RegXMM3,"xmm3")
         , (RegXMM4,"xmm4")
         , (RegXMM5,"xmm5")
         , (RegXMM6,"xmm6")
         , (RegXMM7,"xmm7")

         , (RegXMM8,"xmm8")
         , (RegXMM9,"xmm9")
         , (RegXMM10,"xmm10")
         , (RegXMM11,"xmm11")
         , (RegXMM12,"xmm12")
         , (RegXMM13,"xmm13")
         , (RegXMM14,"xmm14")
         , (RegXMM15,"xmm15")

         , (RegCR0,"cr0")
         , (RegCR1,"cr1")
         , (RegCR2,"cr2")
         , (RegCR3,"cr3")
         , (RegCR4,"cr4")
         , (RegCR5,"cr5")
         , (RegCR6,"cr6")
         , (RegCR7,"cr7")
         , (RegCR8,"cr8")

         , (RegDR0,"dr0")
         , (RegDR1,"dr1")
         , (RegDR2,"dr2")
         , (RegDR3,"dr3")
         , (RegDR4,"dr4")
         , (RegDR5,"dr5")
         , (RegDR6,"dr6")
         , (RegDR7,"dr7")

         , (RegDR0,"db0")
         , (RegDR1,"db1")
         , (RegDR2,"db2")
         , (RegDR3,"db3")
         , (RegDR4,"db4")
         , (RegDR5,"db5")
         , (RegDR6,"db6")
         , (RegDR7,"db7")

         , (RegES,"es")
         , (RegCS,"cs")
         , (RegSS,"ss")
         , (RegDS,"ds")
         , (RegFS,"fs")
         , (RegGS,"gs")

         , (RegRIP , "rip")
         ]



ASSERT_EQ((stringParser codeP "4403e9              08179510"),((0x4403e9, 0x08179510)))
ASSERT_EQ((stringParser codeP "6664810425e74887fa80af08179505"),((0x6664810425e74887fa80af, 0x08179505)))

codeP :: Parser (Integer,Integer)
codeP = define "(code,address)" $ liftM (mapPair $ read . ("0x" ++ )  ) $
        do x <- hexCode
           if length x > 21
             then return $  splitAt (length x - 8) x
             else hexCode >>= \y -> return (x, y)
        where mapPair f (x,y) = (f x,f y)


asmP :: Integer -> Integer  -> Parser Assembly
asmP addr len =
  define "asm" $
       do pre <- many prefixP
          mne <- mnemonicP
          ops <- sepBy opP comma
          return $ Assembly mne (map f ops) (foldr Prefix.union Prefix.empty pre)
         where
           f (AddrOp x) = IntOp $ fromInteger (x - (addr + len) )
           f x = x

prefixP :: Parser Prefix.Set
prefixP = lexeme $ do reserved "rex"
                      dot
                      xs <- many1 (oneOf "RBWX")
                      return $ Prefix.fromList $ Prefix.Rex : map pre xs
          <|>  (choice (map reserved ["repnz","repne"])  >> return Prefix.repne)
          <|>  (choice (map reserved ["rep" ,"repe","repz"])  >> return Prefix.rep)
          <|>  (reserved "addr16"   >> return Prefix.addr)
          <|>  (reserved "addr32"   >> return Prefix.addr)
          <|>  (reserved "data16"   >> return Prefix.op )
          <|>  (reserved "data32"   >> return Prefix.op )
  where
    pre 'R' = Prefix.RexR
    pre 'B' = Prefix.RexB
    pre 'W' = Prefix.RexW
    pre 'X' = Prefix.RexX
    pre _   = UNIMPLEMENTED

mnemonicP :: Parser Mnemonic
mnemonicP = define "mnemonic" $
            try (do symbol "ret"
                    whiteSpace
                    reserved "far"
                    whiteSpace
                    return $ Mnemonic "ret far" )
            <|>
            do x <- identifier
               return $  Mnemonic $ filter ( /= '_' ) x

opP :: Parser Operand
opP = define "operand" $ farPtrP <|> numOpP <|> ptrP   <|>  regP <|> labelP

farPtrP :: Parser Operand
farPtrP = define "segment:offset" $
          try ( do x <- natural
                   colon
                   y <- natural
                   return $ FarPtrOp x y )

labelP :: Parser Operand
labelP = define "<address>" $ liftM (AddrOp . read . ("0x" ++) ) $ angles hexCode

numOpP :: Parser Operand
numOpP = define "number" $ natural >>= return . IntOp .fromInteger

regP :: Parser Operand
regP = define "register" $
       stRegP
       <|> try (do x <- identifier
                   case Map.lookup x regDef of
                     Just d  -> return $ RegOp d
                     Nothing -> pzero)


stRegP :: Parser Operand
stRegP = define "floating point register" $ lexeme $
         do try $ symbol "st"
            n <- option 0 (parens decimal <|> decimal)
            return $ RegOp $ toEnum (fromEnum RegST0 + fromInteger n)

ptrP :: Parser Operand
ptrP = define "ptr" $
       foldr1 (<|>) [ bytePtrP
                    , wordPtrP
                    , dwordPtrP
                    , qwordPtrP
                    , xmmwordPtrP
                    , mmwordPtrP
                    , owordPtrP
                    , fwordPtrP
                    , tbytePtrP
                    , voidPtrP
                    ]

bytePtrP :: Parser Operand
bytePtrP = ptrParser "BYTE" Byte
wordPtrP = ptrParser "WORD" Word
dwordPtrP = ptrParser "DWORD" Dword
qwordPtrP = ptrParser "QWORD" Qword
fwordPtrP = ptrParser "FWORD" Fword
tbytePtrP = ptrParser "TBYTE" Tbyte
owordPtrP = ptrParser "OWORD" Oword
xmmwordPtrP = ptrParser "XMMWORD" XmmWord
mmwordPtrP  = ptrParser "MMWORD" MmWord

ptrParser :: String -> OperandType -> Parser Operand
ptrParser x t  = do reserved x
                    reserved "PTR"
                    expr <- segAddrP
                    return $ PtrOp t expr

voidPtrP :: Parser Operand
voidPtrP = do expr <- segAddrP
              return $ PtrOp Void expr

segAddrP :: Parser SegmentedPoly
segAddrP =  do addr <- addrP
               return $ SPoly DefaultSegment addr
            <|>
            try (do seg  <- segmentP
                    addr <- addrP
                    return $ SPoly seg addr)



segmentP :: Parser Segment
segmentP = do seg <- segRegP
              colon
              return seg

segRegP :: Parser Segment
segRegP = foldr1 (<|>) $ map (\(x,y) -> reserved x >> return y) [ ("es",ES) ,("cs",CS),("ss",SS),("ds",DS),("fs",FS),("gs",GS) ]

addrP :: Parser RegPoly
addrP = constP  <|> (squares $ polyP termP)

constP :: Parser RegPoly
constP = liftM fromInteger natural

termP :: Parser RegPoly
termP = (identifier >>= return . var) <|>  constP
        <?> "reg or int"


casmP :: Parser Assembly
casmP = define "a c asm" $
        do (pre,mne) <- cmnemonicP
           ops <- parens $ sepBy1 copP comma
           return $ Assembly mne (tail ops) pre

cmnemonicP :: Parser (Prefix.Set,Mnemonic)
cmnemonicP = define "c mnemonic" $
             do x <- identifier
                return $ splitPrefix x

splitPrefix :: String -> (Prefix.Set,Mnemonic)
splitPrefix x =
  case span (/= '_') x of
    (s,[])          -> (Prefix.empty , Mnemonic s)
    (s,"_")   -> (Prefix.empty , Mnemonic s )
    ("rep",'_':t)   -> (Prefix.rep , Mnemonic t)
    ("repe",'_':t)  -> (Prefix.repe , Mnemonic t)
    ("repne",'_':t) -> (Prefix.repne , Mnemonic t)
    ("rexw",'_':t)  -> (Prefix.rexw , Mnemonic t)
    ("ret",'_':"far")  -> (Prefix.empty , Mnemonic "ret far")
    _ -> ERROR((show x))

copP :: Parser Operand
copP = define "a c operand" $ cnumOpP  <|> cptrP <|> regP <|> (reserved "p" >> return (IntOp 0)) <|> cfarPtrP

cfarPtrP :: Parser Operand
cfarPtrP = define "far{16,32}(seg,offset)" $
           do reserved "far16" <|> reserved "far32"
              [x,y] <- parens $ sepBy  cnumP comma
              return $ FarPtrOp x y

cptrP :: Parser Operand
cptrP = define "c ptr" $
        foldr1 (<|>) [ cbytePtrP
                     , cwordPtrP
                     , cdwordPtrP
                     , cqwordPtrP
                     , cfwordPtrP
                     , cowordPtrP
                     , ctbytePtrP
                     , cxmmwordPtrP
                     , cmmwordPtrP
                     , far16PtrP
                     , far32PtrP
                     , far64PtrP
                     , cvoidPtrP
                     , cbyteOffsetP
                     , cwordOffsetP
                     , cdwordOffsetP
                     , cqwordOffsetP
                     ]

cbytePtrP  = cptrParser "byte" Byte
cwordPtrP  = cptrParser "word" Word
cdwordPtrP = cptrParser "dword" Dword
cqwordPtrP = cptrParser "qword" Qword
cfwordPtrP = cptrParser "fword" Fword
cowordPtrP = cptrParser "oword" Oword
ctbytePtrP = cptrParser "tbyte" Tbyte
cxmmwordPtrP = cptrParser "xmmword" XmmWord
cmmwordPtrP = cptrParser "mmword" MmWord
far16PtrP = cptrParser "far16" Dword
far32PtrP = cptrParser "far32" Fword
far64PtrP = cptrParser "far64" Tbyte
cvoidPtrP = cptrParser "void" Void

cptrParser :: String -> OperandType -> Parser Operand
cptrParser x t = do reserved $ x ++ "_ptr"
                    expr <- csegExprP
                    return $ PtrOp t expr

cbyteOffsetP  = coffsetParser "byte" Byte
cwordOffsetP  = coffsetParser "word" Word
cdwordOffsetP = coffsetParser "dword" Dword
cqwordOffsetP = coffsetParser "qword" Qword

coffsetParser :: String -> OperandType -> Parser Operand
coffsetParser x t = do reserved $ x ++ "_offset"
                       expr <- csegExprP
                       return $ PtrOp t expr


ASSERT_EQ((stringParser csegExprP ".cs[UINT64_C(0x1)]"),(SPoly CS (fromInteger 0x1)))
ASSERT_EQ((stringParser cbyteOffsetP "byte_offset.cs[UINT64_C(0x1)]"),(PtrOp Byte (SPoly CS  (fromInteger 0x1))))


csegExprP :: Parser SegmentedPoly
csegExprP = do expr <- squares caddrP
               return $ SPoly DefaultSegment expr
            <|>
            do seg <-  csegmentP
               expr <- squares caddrP
               return $ SPoly seg expr

csegmentP :: Parser Segment
csegmentP = dot >> segRegP


ASSERT_EQ((stringParser caddrP "UINT64_C(0x1)"),(fromInteger 0x1))
ASSERT_EQ((stringParser caddrP "(int32_t) INT32_C (0x2)"),(fromInteger 0x2))
ASSERT_EQ((stringParser caddrP "rax"),(var "rax"))

caddrP :: Parser RegPoly
caddrP = polyP $ ( (cscaleP <|> cnumP ) >>= return . fromInteger )  <|>  (identifier >>= return . var)

cscaleP :: Parser Integer
cscaleP = define "scale" $ try $ lexeme  $ char '_' >>  decimal

cnumOpP :: Parser Operand
cnumOpP = cnumP >>= return . IntOp . fromInteger

cnumP :: Parser Integer
cnumP = define "number" $
        cinteger <|> (castP >> cinteger)

castP = parens identifier

cintMacroP :: Parser ()
cintMacroP = foldr1 (<|>) $ map reserved
             [ "UINT8_C"
             , "UINT16_C"
             , "UINT32_C"
             , "UINT64_C"
             , "INT8_C"
             , "INT16_C"
             , "INT32_C"
             , "INT64_C"
             ]
ASSERT_EQ((stringParser cintMacroP "UINT32_C"),())
ASSERT_EQ((stringParser cnumP   "UINT64_C(0x12)"),(0x12))
ASSERT_EQ((stringParser cinteger "123"),(123))
ASSERT_EQ((stringParser cinteger "- 123"),(-123))

cinteger :: Parser Integer
cinteger = define "integer" $ (cintMacroP >> parens cintegerLiteral) <|> cintegerLiteral

cintegerLiteral :: Parser Integer
cintegerLiteral = define "integer literal" $
          do reservedOp "-"
             n <- natural
             intSuffix
             return $ negate n
          <|>
          do n <- natural
             intSuffix
             return n

intSuffix :: Parser String
intSuffix = many (oneOf "ulUL")

hexCode :: Parser String
hexCode = define "hexCode" $  lexeme $ many1 hexDigit

ASSERT_EQ((stringParser (many1 (lexeme decimal)) "12 34"),([12,34]))
ASSERT_EQ((stringParser (many1 (natural)) "12 34"),([12,34]))

comment :: Parser String
comment = define "comment" $
          do reservedOp "#" >> many (noneOf ";")
          <|>
          do string "/*"
             manyTill anyChar (try (string "*/"))


lineP :: Parser (Assembly,Assembly)
lineP = do (code,addr) <- codeP
           len <- lexeme decimal <?> "code len"
           s  <- asmP addr len
           semi <|> (comment >> semi)
           s' <- casmP
           semi
           return (s,s')


lexer :: P.TokenParser ()
lexer = P.makeTokenParser
        (haskellDef
         { P.reservedNames   = [ "byte_ptr","word_ptr","dword_ptr","qword_ptr" ]
         , P.reservedOpNames = [ "*","/","+","-" ]
         })

whiteSpace= P.whiteSpace lexer
lexeme    = P.lexeme lexer
decimal   = P.decimal lexer
hexadecimal   = P.hexadecimal lexer
symbol    = P.symbol lexer
natural   = P.natural lexer
parens    = P.parens lexer
angles    = P.angles lexer
squares   = P.squares lexer
semi      = P.semi lexer
dot       = P.dot lexer
comma     = P.comma lexer
colon     = P.colon lexer
identifier= P.identifier lexer
reserved  = P.reserved lexer
reservedOp= P.reservedOp lexer

define :: String -> Parser a -> Parser a
define = flip (<?>)



checkLine :: Mode -> String -> IO Bool
checkLine mode s =
  case parse lineP "stdin" s of
    Left err    ->  fail  $ "FAIL:" ++ show err ++ ":" ++ s
    Right (x,y) -> if not $ asmEq mode x y || asmEq mode x' y
                   then fail $ "FAIL:" ++ line [s
                                               , "dsm :" ++ show x
                                               , "dsm':" ++ show (canonicalizeAsm mode x')
                                               , "asm :" ++  show y
                                               , "asm':" ++  show (canonicalizeAsm mode y)
                                               ]
                   else pass $ "PASS:" ++ s
                        where x' = fixDsmBug mode x
    where line xs = intercalate  "\n" xs
          fail xs = echo xs >> return False
          pass xs = echo xs >> return True

echo = hPutStrLn stdout
echoError = hPutStrLn stderr


getMode :: IO Mode
getMode = do as <- getArgs
             echoError $ show as
             return $ argsToMode as

argsToMode ["-x86"] = X86
argsToMode ["-x64"] = X64
argsToMode _ = X64

main =
  do
    Main.runTests
    mode <- getMode
    echoError $  "ErasmChecker: mode =  " ++ show mode
    contents <- hGetContents stdin
    bs <- mapM (checkLine mode) $ lines contents
    if elem False bs
      then exitFailure
      else exitSuccess

