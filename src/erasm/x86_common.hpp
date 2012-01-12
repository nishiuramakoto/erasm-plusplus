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
#ifndef MY_ERASM_X86_COMMON_HPP
#define MY_ERASM_X86_COMMON_HPP

#include <iostream>
#include <boost/cstdint.hpp>
#include <boost/static_assert.hpp>
#include "erasm/x64_common.hpp"
#include "meta_prelude.hpp"

namespace erasm {  namespace x86 {
using erasm::prelude::True;
using erasm::prelude::False;

using erasm::prelude::One;	
using erasm::prelude::Two;	
using erasm::prelude::Three;	
using erasm::prelude::Four;	
using erasm::prelude::Five;	
using erasm::prelude::Six;	
using erasm::prelude::Seven;	
using erasm::prelude::Eight;	
using erasm::prelude::Nine;	

using erasm::x64::ModrmConstHandle;
using erasm::x64::ModrmBytes;
using erasm::x64::SibBytes;


using x64::int8_t ;
using x64::int16_t ;
using x64::int32_t ;
using x64::int64_t ;

using x64::uint8_t ;
using x64::uint16_t ;
using x64::uint32_t ;
using x64::uint64_t ;

using x64::disp8_type;
using x64::disp16_type;
using x64::disp32_type;

using x64::offset8_t;
using x64::offset16_t;
using x64::offset32_t;
using x64::offset64_t;

using x64::byte_t ;
using x64::code_ptr ;
using x64::code_pointer_type ;
using x64::const_code_ptr;
using x64::const_code_pointer_type;
using x64::imm8_t;
using x64::imm16_t;
using x64::imm32_t;
using x64::imm64_t;
using x64::rel8_t;
using x64::rel16_t;
using x64::rel32_t;
using x64::rel64_t;

using x64:: MmWord;
using x64:: XmmWord;
using x64:: Fword; 
using x64:: Qword;
using x64:: Tbyte;
using x64:: Oword; 

using x64:: Real4;
using x64:: Real8;
using x64:: Real10;

using x64::Segment;
using x64::DefaultSegment;
using x64::Segment_CS;
using x64::Segment_SS;
using x64::Segment_DS;
using x64::Segment_ES;
using x64::Segment_FS;
using x64::Segment_GS;

using x64::PrefixByte;
using x64::Prefix_Nothing       ;
using x64::Prefix_LOCK		;
using x64::Prefix_REPNE		;
using x64::Prefix_REPNZ		;
using x64::Prefix_F2		;
using x64::Prefix_REP		;
using x64::Prefix_REPE		;
using x64::Prefix_REPZ		;
using x64::Prefix_F3            ;
using x64::Prefix_CS		;
using x64::Prefix_SS		;
using x64::Prefix_DS		;
using x64::Prefix_ES		;
using x64::Prefix_FS		;
using x64::Prefix_GS		;
using x64::Prefix_NOT_BRANCH	;
using x64::Prefix_BRANCH	;
using x64::Prefix_OPERAND_SIZE	;
using x64::Prefix_66     	;
using x64::Prefix_ADDRESS_SIZE	;

using x64:: Register8Low;
using x64:: Register8High;
using x64:: Register16;
using x64:: Register32;
using x64:: RegisterMM;
using x64:: RegisterXMM;
using x64:: RegisterST;
using x64:: RegisterSeg;
using x64:: RegisterCR;
using x64:: RegisterDR;
using x64:: RegNone;
using x64:: Register;



typedef erasm::x64::ByteReg86 ByteReg; 
typedef erasm::x64::WordReg86 WordReg; 
typedef erasm::x64::WordReg86_m_AX WordReg_m_AX; 
typedef erasm::x64::DwordReg86 DwordReg; 
typedef erasm::x64::DwordReg86_m_EAX DwordReg_m_EAX; 
typedef erasm::x64::MmReg86 MmReg; 
typedef erasm::x64::XmmReg86 XmmReg; 
typedef erasm::x64::CrReg86 CrReg; 
typedef erasm::x64::DrReg86 DrReg; 

typedef erasm::x64::StReg StReg; 
typedef erasm::x64::StReg_m_ST0 StReg_m_ST0; 
typedef erasm::x64::SegReg SegReg; 

using x64:: RegAL;
using x64:: RegCL;
using x64:: RegDL;
using x64:: RegBL;
using x64:: RegAH;
using x64:: RegCH;
using x64:: RegDH;
using x64:: RegBH;

using x64:: RegAX;
using x64:: RegCX;
using x64:: RegDX;
using x64:: RegBX;
using x64:: RegSP;
using x64:: RegBP;
using x64:: RegSI;
using x64:: RegDI;

using x64:: RegEAX;
using x64:: RegECX;
using x64:: RegEDX;
using x64:: RegEBX;
using x64:: RegESP;
using x64:: RegEBP;
using x64:: RegESI;
using x64:: RegEDI;

using x64:: RegMM0;
using x64:: RegMM1;
using x64:: RegMM2;
using x64:: RegMM3;
using x64:: RegMM4;
using x64:: RegMM5;
using x64:: RegMM6;
using x64:: RegMM7;

using x64:: RegXMM0;
using x64:: RegXMM1;
using x64:: RegXMM2;
using x64:: RegXMM3;
using x64:: RegXMM4;
using x64:: RegXMM5;
using x64:: RegXMM6;
using x64:: RegXMM7;

using x64:: RegST0;
using x64:: RegST1;
using x64:: RegST2;
using x64:: RegST3;
using x64:: RegST4;
using x64:: RegST5;
using x64:: RegST6;
using x64:: RegST7;

using x64:: RegCR0;
using x64:: RegCR1;
using x64:: RegCR2;
using x64:: RegCR3;
using x64:: RegCR4;
using x64:: RegCR5;
using x64:: RegCR6;
using x64:: RegCR7;

using x64:: RegDR0;
using x64:: RegDR1;
using x64:: RegDR2;
using x64:: RegDR3;
using x64:: RegDR4;
using x64:: RegDR5;
using x64:: RegDR6;
using x64:: RegDR7;

using x64:: RegES;
using x64:: RegCS;
using x64:: RegSS;
using x64:: RegDS;
using x64:: RegFS;
using x64:: RegGS;

/////
using x64::al;
using x64::cl;
using x64::dl;
using x64::bl;
using x64::ah;
using x64::ch;
using x64::dh;
using x64::bh;

using x64::ax;
using x64::cx;
using x64::dx;
using x64::bx;
using x64::sp;
using x64::bp;
using x64::si;
using x64::di;

using x64::eax;
using x64::ecx;
using x64::edx;
using x64::ebx;
using x64::esp;
using x64::ebp;
using x64::esi;
using x64::edi;

using x64::rax;

using x64::mm0;
using x64::mm1;
using x64::mm2;
using x64::mm3;
using x64::mm4;
using x64::mm5;
using x64::mm6;
using x64::mm7;

using x64::xmm0;
using x64::xmm1;
using x64::xmm2;
using x64::xmm3;
using x64::xmm4;
using x64::xmm5;
using x64::xmm6;
using x64::xmm7;

using x64::st0;
using x64::st1;
using x64::st2;
using x64::st3;
using x64::st4;
using x64::st5;
using x64::st6;
using x64::st7;

using x64::cr0;
using x64::cr1;
using x64::cr2;
using x64::cr3;
using x64::cr4;
using x64::cr5;
using x64::cr6;
using x64::cr7;


using x64::dr0;
using x64::dr1;
using x64::dr2;
using x64::dr3;
using x64::dr4;
using x64::dr5;
using x64::dr6;
using x64::dr7;

using x64::db0;
using x64::db1;
using x64::db2;
using x64::db3;
using x64::db4;
using x64::db5;
using x64::db6;
using x64::db7;

using x64::es;
using x64::cs;
using x64::ss;
using x64::ds;
using x64::fs;
using x64::gs;

using x64::_1;
using x64::_2;
using x64::_3;
using x64::_4;
using x64::_5;
using x64::_6;
using x64::_7;
using x64::_8;
using x64::_9;

using x64::FarPtr16;
using x64::FarPtr32;
using x64::far16;
using x64::far32;

}}


#endif // MY_ERASM_X86_COMMON_HPP
