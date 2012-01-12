/* 
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
   along with ERASM++; see the file COPYING.  If not see
   <http://www.gnu.org/licenses/>.  */
#define ERASM_NO_META_ASSERT 1

#include "erasm/x64.hpp"
#include <limits>
#include <iostream>
#include "common_macros.hpp"
#include <stdlib.h>

namespace erasm { namespace x64 {

const char * const byte_reg_names [] = 
{ "al","cl","dl","bl","ah","ch","dh","bh"};

const char * const byte_reg_rex_names [] = 
{ "al"	,"cl"	,"dl"	,"bl"	,"spl"	,"bpl"	,"sil"	,"dil"	,
  "r8l"	,"r9l"	,"r10l"	,"r11l"	,"r12l"	,"r13l"	,"r14l"	,"r15l" };

const char * const word_reg_names [] = 
{ "ax"	,"cx"	,"dx"	,"bx"	,"sp"	,"bp"	,"si"	,"di"	,
  "r8w"	,"r9w"	,"r10w"	,"r11w"	,"r12w"	,"r13w"	,"r14w"	,"r15w" };

const char * const dword_reg_names [] = 
{ "eax"	,"ecx"	,"edx"	,"ebx"	,"esp"	,"ebp"	,"esi"	,"edi"	,
  "r8d"	,"r9d"	,"r10d"	,"r11d"	,"r12d"	,"r13d"	,"r14d"	,"r15d" };

const char * const qword_reg_names [] = 
{ "rax"	,"rcx"	,"rdx"	,"rbx"	,"rsp"	,"rbp"	,"rsi"	,"rdi"	,
  "r8"	,"r9"	,"r10"	,"r11"	,"r12"	,"r13"	,"r14"	,"r15" };

const char * const mm_reg_names [] = 
{ "mm0"	,"mm1"	,"mm2"	,"mm3"	,"mm4"	,"mm5"	,"mm6"	,"mm7"	,
  "mm8"	,"mm9"	,"mm10"	,"mm11"	,"mm12"	,"mm13"	,"mm14"	,"mm15" };

const char * const xmm_reg_names [] = 
{ "xmm0","xmm1"	,"xmm2"	,"xmm3"	,"xmm4"	,"xmm5"	,"xmm6"	,"xmm7"	,
  "xmm8","xmm9"	,"xmm10","xmm11","xmm12","xmm13","xmm14","xmm15" };
 
const char * const st_reg_names [] = 
{ "st0","st1"	,"st2"	,"st3"	,"st4"	,"st5"	,"st6"	,"st7"	};

const char * const seg_reg_names [] = 
{ "es","cs","ss","ds","fs","gs"};

const char * const cr_reg_names [] = 
{ "cr0","cr1"	,"cr2"	,"cr3"	,"cr4"	,"cr5"	,"cr6"	,"cr7"	,
  "cr8","cr9"	,"cr10","cr11","cr12","cr13","cr14","cr15" };

const char * const dr_reg_names [] = 
{ "dr0","dr1"	,"dr2"	,"dr3"	,"dr4"	,"dr5"	,"dr6"	,"dr7"	,
  "dr8","dr9"	,"dr10","dr11","dr12","dr13","dr14","dr15" };


One	_1;
Two	_2;
Three	_3;
Four	_4;
Five	_5;
Six	_6;
Seven	_7;
Eight	_8;
Nine	_9;

RegAL al;
RegCL cl;
RegDL dl;
RegBL bl;
RegAH ah;
RegCH ch;
RegDH dh;
RegBH bh;

RegSPL spl;
RegBPL bpl;
RegSIL sil;
RegDIL dil;

RegR8L r8l;
RegR9L r9l;
RegR10L r10l;
RegR11L r11l;
RegR12L r12l;
RegR13L r13l;
RegR14L r14l;
RegR15L r15l;

RegR8L& r8b = r8l;
RegR9L& r9b = r9l;
RegR10L& r10b = r10l;
RegR11L& r11b = r11l;
RegR12L& r12b = r12l;
RegR13L& r13b = r13l;
RegR14L& r14b = r14l;
RegR15L& r15b = r15l;

RegAX ax;
RegCX cx;
RegDX dx;
RegBX bx;
RegSP sp;
RegBP bp;
RegSI si;
RegDI di;

RegR8W r8w;
RegR9W r9w;
RegR10W r10w;
RegR11W r11w;
RegR12W r12w;
RegR13W r13w;
RegR14W r14w;
RegR15W r15w;

RegEAX eax;
RegECX ecx;
RegEDX edx;
RegEBX ebx;
RegESP esp;
RegEBP ebp;
RegESI esi;
RegEDI edi;

RegR8D r8d;
RegR9D r9d;
RegR10D r10d;
RegR11D r11d;
RegR12D r12d;
RegR13D r13d;
RegR14D r14d;
RegR15D r15d;

RegRIP rip;

RegRAX rax;
RegRCX rcx;
RegRDX rdx;
RegRBX rbx;
RegRSP rsp;
RegRBP rbp;
RegRSI rsi;
RegRDI rdi;

RegR8 r8;
RegR9 r9;
RegR10 r10;
RegR11 r11;
RegR12 r12;
RegR13 r13;
RegR14 r14;
RegR15 r15;

RegMM0 mm0 ;
RegMM1 mm1 ;
RegMM2 mm2 ;
RegMM3 mm3 ;
RegMM4 mm4 ;
RegMM5 mm5 ;
RegMM6 mm6 ;
RegMM7 mm7 ;

RegXMM0 xmm0 ;
RegXMM1 xmm1 ;
RegXMM2 xmm2 ;
RegXMM3 xmm3 ;
RegXMM4 xmm4 ;
RegXMM5 xmm5 ;
RegXMM6 xmm6 ;
RegXMM7 xmm7 ;

RegXMM8 xmm8 ;
RegXMM9 xmm9 ;
RegXMM10 xmm10 ;
RegXMM11 xmm11 ;
RegXMM12 xmm12 ;
RegXMM13 xmm13 ;
RegXMM14 xmm14 ;
RegXMM15 xmm15 ;

RegST0 st0 ;
RegST1 st1 ;
RegST2 st2 ;
RegST3 st3 ;
RegST4 st4 ;
RegST5 st5 ;
RegST6 st6 ;
RegST7 st7 ;

RegCR0 cr0 ;
RegCR1 cr1 ;
RegCR2 cr2 ;
RegCR3 cr3 ;
RegCR4 cr4 ;
RegCR5 cr5 ;
RegCR6 cr6 ;
RegCR7 cr7 ;
RegCR8 cr8 ;

RegDR0 dr0 ;
RegDR1 dr1 ;
RegDR2 dr2 ;
RegDR3 dr3 ;
RegDR4 dr4 ;
RegDR5 dr5 ;
RegDR6 dr6 ;
RegDR7 dr7 ;

RegDR0& db0=dr0;
RegDR1& db1=dr1;
RegDR2& db2=dr2;
RegDR3& db3=dr3;
RegDR4& db4=dr4;
RegDR5& db5=dr5;
RegDR6& db6=dr6;
RegDR7& db7=dr7;

RegES es;
RegCS cs;
RegSS ss;
RegDS ds;
RegFS fs;
RegGS gs;



FarPtr16 far16(uint16_t selector,uint16_t offset)
{
   FarPtr16 p;
   p.selector = selector;
   p.offset   = offset;
   return p;
}

FarPtr32 far32(uint16_t selector,uint32_t offset)
{
   FarPtr32 p;
   p.selector = selector;
   p.offset   = offset;
   return p;
}

FarPtr64 far64(uint16_t selector,uint64_t offset)
{
   FarPtr64 p;
   p.selector = selector;
   p.offset   = offset;
   return p;
}

PtrProxyContainer<void> void_ptr;		
PtrProxyContainer<uint8_t> byte_ptr;		
PtrProxyContainer<uint16_t> word_ptr;		
PtrProxyContainer<uint32_t> dword_ptr;	
PtrProxyContainer<uint64_t> qword_ptr;
PtrProxyContainer<MmWord> mmword_ptr;
PtrProxyContainer<XmmWord> xmmword_ptr;
PtrProxyContainer<Fword>   fword_ptr;
PtrProxyContainer<Oword>   oword_ptr;
PtrProxyContainer<Tbyte>   tbyte_ptr;
PtrProxyContainer<Real4>   real4_ptr;
PtrProxyContainer<Real8>   real8_ptr;
PtrProxyContainer<Real10>  real10_ptr;
PtrProxyContainer<FarPtr16> far16_ptr;
PtrProxyContainer<FarPtr32> far32_ptr;
PtrProxyContainer<FarPtr64> far64_ptr;


OffsetProxyContainer<uint8_t> byte_offset;		
OffsetProxyContainer<uint16_t> word_offset;		
OffsetProxyContainer<uint32_t> dword_offset;	
OffsetProxyContainer<uint64_t> qword_offset;

}} // namespace erasm
