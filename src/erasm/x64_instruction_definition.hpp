#ifndef ERASM_X64_INSTRUCTION_DEFINITION_HPP
#define ERASM_X64_INSTRUCTION_DEFINITION_HPP
/* DO NOT EDIT! 
Copyright (C) 2011,2012 Makoto Nishiura.

This file is part of ERASM++.

ERASM++ is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

ERASM++ is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with ERASM++; see the file COPYING3.  If not see
 <http://www.gnu.org/licenses/>. */

#include "erasm/x64_instruction_definition_common.hpp"
namespace erasm {
namespace x64 {
struct DecodeError : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Adc : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Add : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Addpd : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Addps : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Addsd : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Addss : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Addsubpd : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Addsubps : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Aesdec : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Aesdeclast : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Aesenc : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Aesenclast : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Aesimc : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Aeskeygenassist : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct And : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Andnpd : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Andnps : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Andpd : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Andps : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Blendpd : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Blendps : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Blendvpd : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Blendvps : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Bsf : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Bsr : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Bswap : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Bt : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Btc : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Btr : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Bts : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Call : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Cbw : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Cdq : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Cdqe : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Clc : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Cld : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Clflush : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Cli : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Clts : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Cmc : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Cmova : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Cmovae : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Cmovb : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Cmovbe : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Cmovc : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Cmove : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Cmovg : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Cmovge : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Cmovl : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Cmovle : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Cmovna : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Cmovnae : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Cmovnb : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Cmovnbe : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Cmovnc : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Cmovne : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Cmovng : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Cmovnge : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Cmovnl : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Cmovnle : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Cmovno : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Cmovnp : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Cmovns : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Cmovnz : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Cmovo : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Cmovp : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Cmovpe : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Cmovpo : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Cmovs : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Cmovz : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Cmp : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Cmpeqpd : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Cmpeqps : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Cmpeqsd : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Cmpeqss : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Cmplepd : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Cmpleps : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Cmplesd : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Cmpless : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Cmpltpd : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Cmpltps : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Cmpltsd : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Cmpltss : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Cmpneqpd : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Cmpneqps : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Cmpneqsd : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Cmpneqss : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Cmpnlepd : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Cmpnleps : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Cmpnlesd : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Cmpnless : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Cmpnltpd : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Cmpnltps : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Cmpnltsd : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Cmpnltss : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Cmpordpd : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Cmpordps : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Cmpordsd : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Cmpordss : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Cmppd : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Cmpps : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Cmps : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Cmpsb : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Cmpsd : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Cmpsq : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Cmpss : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Cmpsw : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Cmpunordpd : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Cmpunordps : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Cmpunordsd : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Cmpunordss : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Cmpxchg : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Cmpxchg16b : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Cmpxchg8b : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Comisd : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Comiss : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Cpuid : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Cqo : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Crc32 : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Cvtdq2pd : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Cvtdq2ps : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Cvtpd2dq : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Cvtpd2pi : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Cvtpd2ps : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Cvtpi2pd : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Cvtpi2ps : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Cvtps2dq : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Cvtps2pd : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Cvtps2pi : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Cvtsd2si : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Cvtsd2ss : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Cvtsi2sd : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Cvtsi2ss : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Cvtss2sd : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Cvtss2si : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Cvttpd2dq : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Cvttpd2pi : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Cvttps2dq : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Cvttps2pi : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Cvttsd2si : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Cvttss2si : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Cwd : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Cwde : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Dec : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Div : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Divpd : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Divps : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Divsd : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Divss : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Dppd : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Dpps : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Emms : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Enter : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Extractps : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct F2xm1 : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Fabs : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Fadd : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Faddp : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Fbld : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Fbstp : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Fchs : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Fclex : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Fcmovb : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Fcmovbe : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Fcmove : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Fcmovnb : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Fcmovnbe : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Fcmovne : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Fcmovnu : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Fcmovu : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Fcom : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Fcomi : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Fcomip : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Fcomp : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Fcompp : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Fcos : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Fdecstp : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Fdiv : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Fdivp : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Fdivr : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Fdivrp : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Ffree : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Fiadd : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Ficom : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Ficomp : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Fidiv : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Fidivr : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Fild : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Fimul : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Fincstp : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Finit : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Fist : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Fistp : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Fisttp : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Fisub : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Fisubr : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Fld : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Fld1 : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Fldcw : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Fldenv : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Fldl2e : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Fldl2t : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Fldlg2 : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Fldln2 : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Fldpi : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Fldz : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Fmul : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Fmulp : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Fnclex : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Fninit : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Fnop : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Fnsave : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Fnstcw : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Fnstenv : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Fnstsw : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Fpatan : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Fprem : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Fprem1 : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Fptan : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Frndint : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Frstor : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Fsave : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Fscale : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Fsin : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Fsincos : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Fsqrt : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Fst : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Fstcw : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Fstenv : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Fstp : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Fstsw : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Fsub : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Fsubp : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Fsubr : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Fsubrp : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Ftst : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Fucom : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Fucomi : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Fucomip : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Fucomp : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Fucompp : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Fwait : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Fxam : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Fxch : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Fxrstor : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Fxrstor64 : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Fxsave : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Fxsave64 : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Fxtract : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Fyl2x : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Fyl2xp1 : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Getsec : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Haddpd : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Haddps : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Hlt : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Hsubpd : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Hsubps : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Idiv : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Imul : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct In : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Inc : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Ins : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Insb : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Insd : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Insertps : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Insw : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Int : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Int3 : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Invd : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Invept : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Invlpg : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Invvpid : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Iret : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Iretd : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Iretq : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Ja : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Jae : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Jb : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Jbe : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Jc : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Je : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Jecxz : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Jg : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Jge : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Jl : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Jle : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Jmp : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Jna : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Jnae : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Jnb : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Jnbe : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Jnc : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Jne : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Jng : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Jnge : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Jnl : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Jnle : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Jno : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Jnp : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Jns : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Jnz : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Jo : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Jp : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Jpe : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Jpo : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Jrcxz : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Js : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Jz : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Lar : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Lddqu : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Ldmxcsr : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Lea : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Leave : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Leavew : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Lfence : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Lfs : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Lgdt : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Lgs : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Lidt : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Lldt : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Lmsw : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Lods : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Lodsb : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Lodsd : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Lodsq : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Lodsw : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Loop : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Loope : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Loopne : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Lsl : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Lss : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Ltr : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Maskmovdqu : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Maskmovq : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Maxpd : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Maxps : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Maxsd : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Maxss : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Mfence : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Minpd : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Minps : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Minsd : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Minss : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Monitor : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Mov : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Movapd : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Movaps : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Movbe : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Movd : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Movddup : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Movdq2q : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Movdqa : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Movdqu : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Movhlps : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Movhpd : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Movhps : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Movlhps : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Movlpd : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Movlps : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Movmskpd : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Movmskps : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Movntdq : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Movntdqa : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Movnti : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Movntpd : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Movntps : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Movntq : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Movq : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Movq2dq : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Movs : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Movsb : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Movsd : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Movshdup : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Movsldup : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Movsq : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Movss : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Movsw : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Movsx : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Movsxd : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Movupd : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Movups : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Movzx : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Mpsadbw : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Mul : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Mulpd : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Mulps : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Mulsd : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Mulss : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Mwait : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Neg : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Nop : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Not : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Or : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Orpd : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Orps : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Out : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Outs : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Outsb : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Outsd : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Outsw : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Pabsb : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Pabsd : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Pabsw : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Packssdw : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Packsswb : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Packusdw : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Packuswb : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Paddb : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Paddd : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Paddq : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Paddsb : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Paddsw : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Paddusb : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Paddusw : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Paddw : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Palignr : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Pand : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Pandn : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Pause : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Pavgb : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Pavgw : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Pblendvb : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Pblendw : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Pclmulhqhdq : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Pclmulhqlqdq : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Pclmullqhdq : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Pclmullqlqdq : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Pclmulqdq : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Pcmpeqb : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Pcmpeqd : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Pcmpeqq : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Pcmpeqw : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Pcmpestri : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Pcmpestrm : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Pcmpgtb : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Pcmpgtd : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Pcmpgtq : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Pcmpgtw : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Pcmpistri : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Pcmpistrm : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Pextrb : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Pextrd : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Pextrq : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Pextrw : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Phaddd : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Phaddsw : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Phaddw : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Phminposuw : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Phsubd : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Phsubsw : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Phsubw : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Pinsrb : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Pinsrd : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Pinsrw : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Pmaddubsw : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Pmaddwd : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Pmaxsb : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Pmaxsd : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Pmaxsw : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Pmaxub : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Pmaxud : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Pmaxuw : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Pminsb : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Pminsd : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Pminsw : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Pminub : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Pminud : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Pminuw : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Pmovmskb : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Pmovsxbd : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Pmovsxbq : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Pmovsxbw : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Pmovsxdq : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Pmovsxwd : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Pmovsxwq : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Pmovzxbd : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Pmovzxbq : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Pmovzxbw : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Pmovzxdq : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Pmovzxwd : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Pmovzxwq : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Pmuldq : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Pmulhrsw : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Pmulhuw : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Pmulhw : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Pmulld : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Pmullw : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Pmuludq : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Pop : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Popcnt : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Popf : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Popfq : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Por : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Prefetchnta : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Prefetcht0 : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Prefetcht1 : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Prefetcht2 : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Psadbw : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Pshufb : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Pshufd : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Pshufhw : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Pshuflw : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Pshufw : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Psignb : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Psignd : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Psignw : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Pslld : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Pslldq : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Psllq : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Psllw : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Psrad : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Psraw : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Psrld : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Psrldq : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Psrlq : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Psrlw : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Psubb : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Psubd : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Psubq : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Psubsb : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Psubsw : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Psubusb : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Psubusw : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Psubw : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Ptest : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Punpckhbw : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Punpckhdq : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Punpckhqdq : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Punpckhwd : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Punpcklbw : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Punpckldq : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Punpcklqdq : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Punpcklwd : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Push : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Pushf : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Pushfq : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Pxor : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Rcl : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Rcpps : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Rcpss : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Rcr : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Rdmsr : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Rdpmc : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Rdtsc : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Rdtscp : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Repe_cmps : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Repe_scas : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Repne_cmps : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Repne_scas : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Rep_ins : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Rep_lods : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Rep_movs : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Rep_outs : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Rep_stos : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Ret : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Retf : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Rol : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Ror : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Roundpd : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Roundps : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Roundsd : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Roundss : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Rsqrtps : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Rsqrtss : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Sal : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Sar : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Sbb : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Scas : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Scasb : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Scasd : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Scasq : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Scasw : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Seta : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Setae : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Setb : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Setbe : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Setc : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Sete : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Setg : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Setge : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Setl : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Setle : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Setna : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Setnae : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Setnb : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Setnbe : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Setnc : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Setne : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Setng : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Setnge : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Setnl : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Setnle : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Setno : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Setnp : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Setns : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Setnz : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Seto : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Setp : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Setpe : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Setpo : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Sets : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Setz : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Sfence : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Sgdt : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Shl : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Shld : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Shr : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Shrd : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Shufpd : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Shufps : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Sidt : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Sldt : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Smsw : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Sqrtpd : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Sqrtps : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Sqrtsd : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Sqrtss : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Stc : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Std : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Sti : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Stmxcsr : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Stos : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Stosb : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Stosd : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Stosq : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Stosw : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Str : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Sub : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Subpd : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Subps : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Subsd : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Subss : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Swapgs : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Syscall : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Sysenter : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Sysexit : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Sysret : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Test : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Ucomisd : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Ucomiss : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Ud2 : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Unpckhpd : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Unpckhps : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Unpcklpd : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Unpcklps : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Verr : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Verw : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Vmcall : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Vmclear : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Vmlaunch : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Vmptrld : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Vmptrst : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Vmread : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Vmresume : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Vmwrite : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Vmxoff : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Vmxon : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Wait : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Wbinvd : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Wrmsr : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Xadd : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Xchg : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Xgetbv : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Xlat : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Xlatb : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Xor : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Xorpd : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Xorps : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Xrstor : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Xrstor64 : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Xsave : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Xsave64 : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
struct Xsetbv : InstructionData
{
   static
    const char (* const  mnemonic);
} ;
}
}
#else

#endif