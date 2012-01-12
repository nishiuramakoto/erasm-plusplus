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
#ifndef MY_X64_COMMON
#define MY_X64_COMMON


#include "meta_prelude.hpp"
#include "erasm/intel_common.hpp"
#include "common_macros.hpp"
#include <boost/static_assert.hpp>

namespace erasm {  namespace x64 {

struct Error_Invalid_displacement;
struct Error_Invalid_register_code;
struct Error_Invalid_address_expression;
struct Error_ESP_and_RSP_cannot_be_used_as_the_index_register;
struct Error_Invalid_Scale;
struct Error_AH_CH_DH_and_BH_cannot_be_used_with_rex_registers;

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



const int RIP_CODE = -2;

enum Segment {
   DefaultSegment = 0,
   Segment_CS = 0x2e,
   Segment_SS = 0x36,
   Segment_DS = 0x3e,
   Segment_ES = 0x26,
   Segment_FS = 0x64,
   Segment_GS = 0x65
};

enum PrefixByte {
   Prefix_Nothing       = 0,
   Prefix_LOCK		= 0xF0,
   Prefix_REPNE		= 0xF2,
   Prefix_REPNZ		= 0xF2,
   Prefix_F2		= 0xF2,
   Prefix_REP		= 0xF3,
   Prefix_REPE		= 0xF3,
   Prefix_REPZ		= 0xF3,
   Prefix_F3            = 0xF3,

   Prefix_CS		= 0x2E,
   Prefix_SS		= 0x36,
   Prefix_DS		= 0x3E,
   Prefix_ES		= 0x26,
   Prefix_FS		= 0x64,
   Prefix_GS		= 0x65,

   Prefix_NOT_BRANCH	= 0x2E,
   Prefix_BRANCH	= 0x3E,
   Prefix_OPERAND_SIZE	= 0x66,
   Prefix_66     	= 0x66,
   Prefix_ADDRESS_SIZE	= 0x67
};

BOOST_STATIC_ASSERT(((byte_t)Prefix_CS == (byte_t)Segment_CS));
BOOST_STATIC_ASSERT(((byte_t)Prefix_SS == (byte_t)Segment_SS));
BOOST_STATIC_ASSERT(((byte_t)Prefix_DS == (byte_t)Segment_DS));
BOOST_STATIC_ASSERT(((byte_t)Prefix_ES == (byte_t)Segment_ES));
BOOST_STATIC_ASSERT(((byte_t)Prefix_FS == (byte_t)Segment_FS));
BOOST_STATIC_ASSERT(((byte_t)Prefix_GS == (byte_t)Segment_GS));

const byte_t  NoRex = 0;
const byte_t  Rex  = 0x40;
const byte_t  RexW = 0x48;
const byte_t  RexR = 0x44;
const byte_t  RexX = 0x42;
const byte_t  RexB = 0x41;

const byte_t  RexWBit = 0x8;
const byte_t  RexRBit = 0x4;
const byte_t  RexXBit = 0x2;
const byte_t  RexBBit = 0x1;

// 64 bit
struct MmWord;
// 128 bit
struct XmmWord;
// 6 byte
struct Fword;
// 8 byte
struct Qword;
// 10 byte
struct Tbyte;
// 16 byte
struct Oword;

// float
struct Real4;
// double
struct Real8;
// long double
struct Real10;

// Using these symbols makes font-locking easier
typedef uint16_t word_t;
typedef uint32_t dword_t;
typedef uint64_t qword_t;
typedef MmWord   mmword_t;
typedef XmmWord  xmmword_t;
typedef Fword    fword_t;
typedef Tbyte    tbyte_t;
typedef Oword    oword_t;
typedef Real4    real4_t;
typedef Real8    real8_t;
typedef Real10   real10_t;
typedef FarPtr16 farptr16_t;
typedef FarPtr32 farptr32_t;
typedef FarPtr64 farptr64_t;



template <int i> struct Register8Low {};
template <int i> struct Register8High {};
template <int i> struct Register8Rex {};
template <int i> struct Register16 {};
template <int i> struct Register32 {};
template <int i> struct Register64 {};
template <int i> struct RegisterMM {};
template <int i> struct RegisterXMM {};
template <int i> struct RegisterST {};
template <int i> struct RegisterSeg {};
template <int i> struct RegisterCR {};
template <int i> struct RegisterDR {};
template <int i> struct RegNoneT {};

struct Register
{
   explicit Register(int c,bool rex=false)
      : code(c + (rex ? 0x8 : 0))
      {}

   int  get_code() const
      { return code & 0x7 ; }
   bool is_ext_reg() const
      { return code >= 8 ; }

   int  get_extended_code() const
      { return code ; }


   bool operator == (const Register& x) const
      { return code == x.code ; }
   int  code;
};


template<class X,int max_bound,const char* const name[]>
struct TypedRegisterBase : Register
{
   typedef X value_type;
   typedef TypedRegisterBase base;

   explicit TypedRegisterBase   (int c,bool rex=false) 
      : Register(c,rex) 
      {	
	 assert(is_valid() 
		|| DUMP3(get_extended_code(), name[0],name[max_bound])); 
      }

   const char* get_name() const
      {
	 assert(is_valid() 
		|| DUMP3(get_extended_code(), name[0],name[max_bound]));
	 return name[get_extended_code()];
      }
   bool is_valid() const
      { return impl::in_range(get_extended_code(),0,max_bound);}
};


struct ByteReg    : Register
{
   typedef Register base;
   template<int i> 
   ByteReg(Register8Low<i>)  : base(i)   {}
   template<int i> 
   ByteReg(Register8High<i>) : base(i)   {}
   template<int i>
   ByteReg(Register8Rex<i>) : base(i) {}

};


struct ByteReg86 :  public TypedRegisterBase<byte_t ,7,byte_reg_names>
{
   explicit ByteReg86(int c,bool rex=false)
      : base(c,rex)
      {}
   template<int i> 
   ByteReg86(Register8Low<i>)
   : base(i)
      { assert(impl::in_range(i,0,3)); }
   template<int i> 
   ByteReg86(Register8High<i>)
   : base(i)
      { assert(impl::in_range(i,4,7)); }
};

struct ByteRegLow :  public ByteReg86
{
   explicit ByteRegLow(int c,bool rex=false)
      : ByteReg86(c,rex)
      { assert(impl::in_range(c,0,3));}
   template<int i> 
   ByteRegLow(Register8Low<i>)
   : ByteReg86(i)
      { assert(impl::in_range(i,0,3)); }
};

struct ByteRegHigh :  public ByteReg86
{
   explicit ByteRegHigh(int c,bool rex=false)
      : ByteReg86(c,rex)
      { assert(impl::in_range(c,0,3));}
   template<int i> 
   ByteRegHigh(Register8High<i>)
   : ByteReg86(i)
      { assert(impl::in_range(i,4,7)); }
};

struct ByteRegRex : public TypedRegisterBase<byte_t ,15,byte_reg_rex_names>
{
   explicit ByteRegRex(int c,bool rex=false) : base(c,rex) {}
   template<int i>
   ByteRegRex(Register8Rex<i>) : base(i) {}
};
struct WordReg : public TypedRegisterBase<word_t,15,word_reg_names> 
{
   explicit WordReg(int c,bool rex=false) : base(c,rex) {}
   template<int i>
   WordReg(Register16<i>) : base(i) {}
};

struct WordReg_m_AX : public WordReg
{
   typedef WordReg base;
   explicit WordReg_m_AX(int c,bool rex=false) : base(c,rex) {}

   WordReg_m_AX(Register16<1>) : base(1) {}
   WordReg_m_AX(Register16<2>) : base(2) {}
   WordReg_m_AX(Register16<3>) : base(3) {}
   WordReg_m_AX(Register16<4>) : base(4) {}
   WordReg_m_AX(Register16<5>) : base(5) {}
   WordReg_m_AX(Register16<6>) : base(6) {}
   WordReg_m_AX(Register16<7>) : base(7) {}
   WordReg_m_AX(Register16<8>) : base(8) {}
   WordReg_m_AX(Register16<9>) : base(9) {}
   WordReg_m_AX(Register16<10>) : base(10) {}
   WordReg_m_AX(Register16<11>) : base(11) {}
   WordReg_m_AX(Register16<12>) : base(12) {}
   WordReg_m_AX(Register16<13>) : base(13) {}
   WordReg_m_AX(Register16<14>) : base(14) {}
   WordReg_m_AX(Register16<15>) : base(15) {}
};



struct WordReg86 : public  WordReg
{
   explicit WordReg86(int c,bool rex=false) : WordReg(c,rex) {}
   WordReg86(Register16<0>) : WordReg(0) {}
   WordReg86(Register16<1>) : WordReg(1) {}
   WordReg86(Register16<2>) : WordReg(2) {}
   WordReg86(Register16<3>) : WordReg(3) {}
   WordReg86(Register16<4>) : WordReg(4) {}
   WordReg86(Register16<5>) : WordReg(5) {}
   WordReg86(Register16<6>) : WordReg(6) {}
   WordReg86(Register16<7>) : WordReg(7) {}
};

struct WordReg86_m_AX : public  WordReg86
{
   typedef WordReg86 base;
   explicit WordReg86_m_AX(int c,bool rex=false) : base(c,rex) {}
   WordReg86_m_AX(Register16<1>) : base(1) {}
   WordReg86_m_AX(Register16<2>) : base(2) {}
   WordReg86_m_AX(Register16<3>) : base(3) {}
   WordReg86_m_AX(Register16<4>) : base(4) {}
   WordReg86_m_AX(Register16<5>) : base(5) {}
   WordReg86_m_AX(Register16<6>) : base(6) {}
   WordReg86_m_AX(Register16<7>) : base(7) {}
};

struct WordRegRex : public  WordReg
{
   explicit WordRegRex(int c,bool rex=false) : WordReg(c,rex) {}
   WordRegRex(Register16<8>)  : WordReg(8) {}
   WordRegRex(Register16<9>)  : WordReg(9) {}
   WordRegRex(Register16<10>) : WordReg(10) {}
   WordRegRex(Register16<11>) : WordReg(11) {}
   WordRegRex(Register16<12>) : WordReg(12) {}
   WordRegRex(Register16<13>) : WordReg(13) {}
   WordRegRex(Register16<14>) : WordReg(14) {}
   WordRegRex(Register16<15>) : WordReg(15) {}
};


struct DwordReg : public  TypedRegisterBase<dword_t,15,dword_reg_names>
{
   explicit DwordReg(int c,bool rex=false) : base(c,rex) {}
   template<int i>
   DwordReg(Register32<i>) : base(i) {}
};

struct DwordReg_m_EAX : public DwordReg
{
   typedef DwordReg base;
   explicit DwordReg_m_EAX(int c,bool rex=false) : base(c,rex) {}

   DwordReg_m_EAX(Register32<1>) : base(1) {}
   DwordReg_m_EAX(Register32<2>) : base(2) {}
   DwordReg_m_EAX(Register32<3>) : base(3) {}
   DwordReg_m_EAX(Register32<4>) : base(4) {}
   DwordReg_m_EAX(Register32<5>) : base(5) {}
   DwordReg_m_EAX(Register32<6>) : base(6) {}
   DwordReg_m_EAX(Register32<7>) : base(7) {}
   DwordReg_m_EAX(Register32<8>) : base(8) {}
   DwordReg_m_EAX(Register32<9>) : base(9) {}
   DwordReg_m_EAX(Register32<10>) : base(10) {}
   DwordReg_m_EAX(Register32<11>) : base(11) {}
   DwordReg_m_EAX(Register32<12>) : base(12) {}
   DwordReg_m_EAX(Register32<13>) : base(13) {}
   DwordReg_m_EAX(Register32<14>) : base(14) {}
   DwordReg_m_EAX(Register32<15>) : base(15) {}
};


struct DwordReg86 : public  DwordReg
{
   explicit DwordReg86(int c,bool rex=false) : DwordReg(c,rex) {}
   DwordReg86(Register32<0>) : DwordReg(0) {}
   DwordReg86(Register32<1>) : DwordReg(1) {}
   DwordReg86(Register32<2>) : DwordReg(2) {}
   DwordReg86(Register32<3>) : DwordReg(3) {}
   DwordReg86(Register32<4>) : DwordReg(4) {}
   DwordReg86(Register32<5>) : DwordReg(5) {}
   DwordReg86(Register32<6>) : DwordReg(6) {}
   DwordReg86(Register32<7>) : DwordReg(7) {}
};

struct DwordReg86_m_EAX : public  DwordReg86
{
   typedef DwordReg86 base;
   explicit DwordReg86_m_EAX(int c,bool rex=false) : base(c,rex) {}
   DwordReg86_m_EAX(Register32<1>) : base(1) {}
   DwordReg86_m_EAX(Register32<2>) : base(2) {}
   DwordReg86_m_EAX(Register32<3>) : base(3) {}
   DwordReg86_m_EAX(Register32<4>) : base(4) {}
   DwordReg86_m_EAX(Register32<5>) : base(5) {}
   DwordReg86_m_EAX(Register32<6>) : base(6) {}
   DwordReg86_m_EAX(Register32<7>) : base(7) {}
};


struct DwordRegRex : public  DwordReg
{
   explicit DwordRegRex(int c,bool rex=false) : DwordReg(c,rex) {}
   DwordRegRex(Register32<8>)  : DwordReg(8) {}
   DwordRegRex(Register32<9>)  : DwordReg(9) {}
   DwordRegRex(Register32<10>) : DwordReg(10) {}
   DwordRegRex(Register32<11>) : DwordReg(11) {}
   DwordRegRex(Register32<12>) : DwordReg(12) {}
   DwordRegRex(Register32<13>) : DwordReg(13) {}
   DwordRegRex(Register32<14>) : DwordReg(14) {}
   DwordRegRex(Register32<15>) : DwordReg(15) {}
};

struct QwordReg : public TypedRegisterBase<qword_t,15,qword_reg_names> 
{
   explicit QwordReg(int c,bool rex=false) : base(c,rex) {}
   template<int i>
   QwordReg(Register64<i>) : base(i) {}
};

struct QwordReg_m_RAX : public QwordReg
{
   typedef QwordReg base;
   explicit QwordReg_m_RAX(int c,bool rex=false) : base(c,rex) {}

   QwordReg_m_RAX(Register64<1>) : base(1) {}
   QwordReg_m_RAX(Register64<2>) : base(2) {}
   QwordReg_m_RAX(Register64<3>) : base(3) {}
   QwordReg_m_RAX(Register64<4>) : base(4) {}
   QwordReg_m_RAX(Register64<5>) : base(5) {}
   QwordReg_m_RAX(Register64<6>) : base(6) {}
   QwordReg_m_RAX(Register64<7>) : base(7) {}
   QwordReg_m_RAX(Register64<8>) : base(8) {}
   QwordReg_m_RAX(Register64<9>) : base(9) {}
   QwordReg_m_RAX(Register64<10>) : base(10) {}
   QwordReg_m_RAX(Register64<11>) : base(11) {}
   QwordReg_m_RAX(Register64<12>) : base(12) {}
   QwordReg_m_RAX(Register64<13>) : base(13) {}
   QwordReg_m_RAX(Register64<14>) : base(14) {}
   QwordReg_m_RAX(Register64<15>) : base(15) {}
};

struct MmReg : public TypedRegisterBase<mmword_t,15,mm_reg_names>
{
   explicit MmReg(int c,bool rex=false) : base(c,rex) {}
   template<int i>
   MmReg(RegisterMM<i>) : base(i) {}
};

struct MmReg86 : public MmReg
{
   typedef MmReg base;
   explicit MmReg86(int c,bool rex=false) : base(c,rex) {}

   MmReg86(RegisterMM<0>) : base(0) {}
   MmReg86(RegisterMM<1>) : base(1) {}
   MmReg86(RegisterMM<2>) : base(2) {}
   MmReg86(RegisterMM<3>) : base(3) {}
   MmReg86(RegisterMM<4>) : base(4) {}
   MmReg86(RegisterMM<5>) : base(5) {}
   MmReg86(RegisterMM<6>) : base(6) {}
   MmReg86(RegisterMM<7>) : base(7) {}
};



struct XmmReg : public  TypedRegisterBase<xmmword_t,15,xmm_reg_names>
{
   explicit XmmReg(int c,bool rex=false) : base(c,rex) {}
   template<int i>
   XmmReg(RegisterXMM<i>) : base(i) {}
};

struct XmmReg86 : public XmmReg
{
   typedef XmmReg base;
   explicit XmmReg86(int c,bool rex=false) : base(c,rex) {}

   XmmReg86(RegisterXMM<0>) : base(0) {}
   XmmReg86(RegisterXMM<1>) : base(1) {}
   XmmReg86(RegisterXMM<2>) : base(2) {}
   XmmReg86(RegisterXMM<3>) : base(3) {}
   XmmReg86(RegisterXMM<4>) : base(4) {}
   XmmReg86(RegisterXMM<5>) : base(5) {}
   XmmReg86(RegisterXMM<6>) : base(6) {}
   XmmReg86(RegisterXMM<7>) : base(7) {}
};


struct StReg : public TypedRegisterBase<float,7,st_reg_names>
{
   explicit StReg(int c,bool rex=false) : base(c,rex) {}
   template<int i>
   StReg(RegisterST<i>) : base(i) {}
};

struct StReg_m_ST0 : public StReg
{
   typedef StReg base;
   explicit StReg_m_ST0(int c,bool rex=false) : base(c,rex) {}

   StReg_m_ST0(RegisterST<1>) : base(1) {}
   StReg_m_ST0(RegisterST<2>) : base(2) {}
   StReg_m_ST0(RegisterST<3>) : base(3) {}
   StReg_m_ST0(RegisterST<4>) : base(4) {}
   StReg_m_ST0(RegisterST<5>) : base(5) {}
   StReg_m_ST0(RegisterST<6>) : base(6) {}
   StReg_m_ST0(RegisterST<7>) : base(7) {}
};

struct CrReg : public TypedRegisterBase<void,15,cr_reg_names>
{
   explicit CrReg(int c,bool rex=false) : base(c,rex) {}
   template<int i>
   CrReg(RegisterCR<i>) : base(i) {}
};

struct CrReg86 : public CrReg
{
   typedef CrReg base;
   explicit CrReg86(int c,bool rex=false) : base(c,rex) {}

   CrReg86(RegisterCR<0>) : base(0) {}
   CrReg86(RegisterCR<1>) : base(1) {}
   CrReg86(RegisterCR<2>) : base(2) {}
   CrReg86(RegisterCR<3>) : base(3) {}
   CrReg86(RegisterCR<4>) : base(4) {}
   CrReg86(RegisterCR<5>) : base(5) {}
   CrReg86(RegisterCR<6>) : base(6) {}
   CrReg86(RegisterCR<7>) : base(7) {}
};

struct DrReg : public  TypedRegisterBase<void,15,dr_reg_names>
{
   explicit DrReg(int c,bool rex=false) : base(c,rex) {}
   template<int i>
   DrReg(RegisterDR<i>) : base(i) {}
};

struct DrReg86 : public DrReg
{
   typedef DrReg base;
   explicit DrReg86(int c,bool rex=false) : base(c,rex) {}

   DrReg86(RegisterDR<0>) : base(0) {}
   DrReg86(RegisterDR<1>) : base(1) {}
   DrReg86(RegisterDR<2>) : base(2) {}
   DrReg86(RegisterDR<3>) : base(3) {}
   DrReg86(RegisterDR<4>) : base(4) {}
   DrReg86(RegisterDR<5>) : base(5) {}
   DrReg86(RegisterDR<6>) : base(6) {}
   DrReg86(RegisterDR<7>) : base(7) {}
};

struct SegReg : public TypedRegisterBase<void,7,seg_reg_names>
{
   explicit SegReg(int c,bool rex=false) : base(c,rex) {}
   template<int i>
   SegReg(RegisterSeg<i>) : base(i) {}
};


template<class X>
struct registerCode;

template<class T>
class Label
{
public:
   void set_instruction_address(code_ptr p);
   void set_target_address(code_ptr p);
   void set_encoded_address(code_ptr p);
   bool is_encodable() const;
   void encode() const;
private:
   code_ptr encoded_address;
   code_ptr instruction_address;
   code_ptr target_address;
};

typedef Label<int8_t>  Label8;
typedef Label<int16_t> Label16;
typedef Label<int32_t> Label32;


typedef RegNoneT<-1> RegNone;

typedef Register8Low<0> RegAL;
typedef Register8Low<1> RegCL;
typedef Register8Low<2> RegDL;
typedef Register8Low<3> RegBL;
typedef Register8High<4> RegAH;
typedef Register8High<5> RegCH;
typedef Register8High<6> RegDH;
typedef Register8High<7> RegBH;

typedef Register8Rex<4> RegSPL;
typedef Register8Rex<5> RegBPL;
typedef Register8Rex<6> RegSIL;
typedef Register8Rex<7> RegDIL;
typedef Register8Rex<8> RegR8L;
typedef Register8Rex<9> RegR9L;
typedef Register8Rex<10> RegR10L;
typedef Register8Rex<11> RegR11L;
typedef Register8Rex<12> RegR12L;
typedef Register8Rex<13> RegR13L;
typedef Register8Rex<14> RegR14L;
typedef Register8Rex<15> RegR15L;

// Synonyms: r8b in ATT (and AMD) syntax, r8l in Intel syntax
typedef Register8Rex<8> RegR8B;
typedef Register8Rex<9> RegR9B;
typedef Register8Rex<10> RegR10B;
typedef Register8Rex<11> RegR11B;
typedef Register8Rex<12> RegR12B;
typedef Register8Rex<13> RegR13B;
typedef Register8Rex<14> RegR14B;
typedef Register8Rex<15> RegR15B;


typedef Register16<0> RegAX;
typedef Register16<1> RegCX;
typedef Register16<2> RegDX;
typedef Register16<3> RegBX;
typedef Register16<4> RegSP;
typedef Register16<5> RegBP;
typedef Register16<6> RegSI;
typedef Register16<7> RegDI;

typedef Register16<8> RegR8W;
typedef Register16<9> RegR9W;
typedef Register16<10> RegR10W;
typedef Register16<11> RegR11W;
typedef Register16<12> RegR12W;
typedef Register16<13> RegR13W;
typedef Register16<14> RegR14W;
typedef Register16<15> RegR15W;

typedef Register32<0> RegEAX;
typedef Register32<1> RegECX;
typedef Register32<2> RegEDX;
typedef Register32<3> RegEBX;
typedef Register32<4> RegESP;
typedef Register32<5> RegEBP;
typedef Register32<6> RegESI;
typedef Register32<7> RegEDI;

typedef Register32<8> RegR8D;
typedef Register32<9> RegR9D;
typedef Register32<10> RegR10D;
typedef Register32<11> RegR11D;
typedef Register32<12> RegR12D;
typedef Register32<13> RegR13D;
typedef Register32<14> RegR14D;
typedef Register32<15> RegR15D;

typedef Register64<RIP_CODE> RegRIP;
typedef Register64<0> RegRAX;
typedef Register64<1> RegRCX;
typedef Register64<2> RegRDX;
typedef Register64<3> RegRBX;
typedef Register64<4> RegRSP;
typedef Register64<5> RegRBP;
typedef Register64<6> RegRSI;
typedef Register64<7> RegRDI;

typedef Register64<8> RegR8;
typedef Register64<9> RegR9;
typedef Register64<10> RegR10;
typedef Register64<11> RegR11;
typedef Register64<12> RegR12;
typedef Register64<13> RegR13;
typedef Register64<14> RegR14;
typedef Register64<15> RegR15;

typedef RegisterMM<0> RegMM0;
typedef RegisterMM<1> RegMM1;
typedef RegisterMM<2> RegMM2;
typedef RegisterMM<3> RegMM3;
typedef RegisterMM<4> RegMM4;
typedef RegisterMM<5> RegMM5;
typedef RegisterMM<6> RegMM6;
typedef RegisterMM<7> RegMM7;

typedef RegisterXMM<0> RegXMM0;
typedef RegisterXMM<1> RegXMM1;
typedef RegisterXMM<2> RegXMM2;
typedef RegisterXMM<3> RegXMM3;
typedef RegisterXMM<4> RegXMM4;
typedef RegisterXMM<5> RegXMM5;
typedef RegisterXMM<6> RegXMM6;
typedef RegisterXMM<7> RegXMM7;

typedef RegisterXMM<8> RegXMM8;
typedef RegisterXMM<9> RegXMM9;
typedef RegisterXMM<10> RegXMM10;
typedef RegisterXMM<11> RegXMM11;
typedef RegisterXMM<12> RegXMM12;
typedef RegisterXMM<13> RegXMM13;
typedef RegisterXMM<14> RegXMM14;
typedef RegisterXMM<15> RegXMM15;

typedef RegisterST<0> RegST0;
typedef RegisterST<1> RegST1;
typedef RegisterST<2> RegST2;
typedef RegisterST<3> RegST3;
typedef RegisterST<4> RegST4;
typedef RegisterST<5> RegST5;
typedef RegisterST<6> RegST6;
typedef RegisterST<7> RegST7;

typedef RegisterCR<0> RegCR0;
typedef RegisterCR<1> RegCR1;
typedef RegisterCR<2> RegCR2;
typedef RegisterCR<3> RegCR3;
typedef RegisterCR<4> RegCR4;
typedef RegisterCR<5> RegCR5;
typedef RegisterCR<6> RegCR6;
typedef RegisterCR<7> RegCR7;

typedef RegisterCR<8> RegCR8;
typedef RegisterCR<9> RegCR9;
typedef RegisterCR<10> RegCR10;
typedef RegisterCR<11> RegCR11;
typedef RegisterCR<12> RegCR12;
typedef RegisterCR<13> RegCR13;
typedef RegisterCR<14> RegCR14;
typedef RegisterCR<15> RegCR15;

typedef RegisterDR<0> RegDR0;
typedef RegisterDR<1> RegDR1;
typedef RegisterDR<2> RegDR2;
typedef RegisterDR<3> RegDR3;
typedef RegisterDR<4> RegDR4;
typedef RegisterDR<5> RegDR5;
typedef RegisterDR<6> RegDR6;
typedef RegisterDR<7> RegDR7;

typedef RegisterDR<8> RegDR8;
typedef RegisterDR<9> RegDR9;
typedef RegisterDR<10> RegDR10;
typedef RegisterDR<11> RegDR11;
typedef RegisterDR<12> RegDR12;
typedef RegisterDR<13> RegDR13;
typedef RegisterDR<14> RegDR14;
typedef RegisterDR<15> RegDR15;

typedef RegisterSeg<0> RegES;
typedef RegisterSeg<1> RegCS;
typedef RegisterSeg<2> RegSS;
typedef RegisterSeg<3> RegDS;
typedef RegisterSeg<4> RegFS;
typedef RegisterSeg<5> RegGS;

extern  RegAL al;
extern  RegCL cl;
extern  RegDL dl;
extern  RegBL bl;
extern  RegAH ah;
extern  RegCH ch;
extern  RegDH dh;
extern  RegBH bh;

extern  RegSPL spl;
extern  RegBPL bpl;
extern  RegSIL sil;
extern  RegDIL dil;
extern  RegR8L r8l;
extern  RegR9L r9l;
extern  RegR10L r10l;
extern  RegR11L r11l;
extern  RegR12L r12l;
extern  RegR13L r13l;
extern  RegR14L r14l;
extern  RegR15L r15l;

extern  RegR8L& r8b;
extern  RegR9L& r9b;
extern  RegR10L& r10b;
extern  RegR11L& r11b;
extern  RegR12L& r12b;
extern  RegR13L& r13b;
extern  RegR14L& r14b;
extern  RegR15L& r15b;

extern  RegAX ax;
extern  RegCX cx;
extern  RegDX dx;
extern  RegBX bx;
extern  RegSP sp;
extern  RegBP bp;
extern  RegSI si;
extern  RegDI di;

extern  RegR8W r8w;
extern  RegR9W r9w;
extern  RegR10W r10w;
extern  RegR11W r11w;
extern  RegR12W r12w;
extern  RegR13W r13w;
extern  RegR14W r14w;
extern  RegR15W r15w;

extern  RegEAX eax;
extern  RegECX ecx;
extern  RegEDX edx;
extern  RegEBX ebx;
extern  RegESP esp;
extern  RegEBP ebp;
extern  RegESI esi;
extern  RegEDI edi;

extern  RegR8D r8d;
extern  RegR9D r9d;
extern  RegR10D r10d;
extern  RegR11D r11d;
extern  RegR12D r12d;
extern  RegR13D r13d;
extern  RegR14D r14d;
extern  RegR15D r15d;

extern  RegRIP rip;

extern  RegRAX rax;
extern  RegRCX rcx;
extern  RegRDX rdx;
extern  RegRBX rbx;
extern  RegRSP rsp;
extern  RegRBP rbp;
extern  RegRSI rsi;
extern  RegRDI rdi;

extern  RegR8 r8;
extern  RegR9 r9;
extern  RegR10 r10;
extern  RegR11 r11;
extern  RegR12 r12;
extern  RegR13 r13;
extern  RegR14 r14;
extern  RegR15 r15;

extern  RegMM0 mm0;
extern  RegMM1 mm1;
extern  RegMM2 mm2;
extern  RegMM3 mm3;
extern  RegMM4 mm4;
extern  RegMM5 mm5;
extern  RegMM6 mm6;
extern  RegMM7 mm7;

extern  RegXMM0 xmm0;
extern  RegXMM1 xmm1;
extern  RegXMM2 xmm2;
extern  RegXMM3 xmm3;
extern  RegXMM4 xmm4;
extern  RegXMM5 xmm5;
extern  RegXMM6 xmm6;
extern  RegXMM7 xmm7;

extern  RegXMM8 xmm8;
extern  RegXMM9 xmm9;
extern  RegXMM10 xmm10;
extern  RegXMM11 xmm11;
extern  RegXMM12 xmm12;
extern  RegXMM13 xmm13;
extern  RegXMM14 xmm14;
extern  RegXMM15 xmm15;

extern  RegST0 st0;
extern  RegST1 st1;
extern  RegST2 st2;
extern  RegST3 st3;
extern  RegST4 st4;
extern  RegST5 st5;
extern  RegST6 st6;
extern  RegST7 st7;

extern  RegCR0 cr0;
extern  RegCR1 cr1;
extern  RegCR2 cr2;
extern  RegCR3 cr3;
extern  RegCR4 cr4;
extern  RegCR5 cr5;
extern  RegCR6 cr6;
extern  RegCR7 cr7;
extern  RegCR8 cr8;

extern  RegDR0 dr0;
extern  RegDR1 dr1;
extern  RegDR2 dr2;
extern  RegDR3 dr3;
extern  RegDR4 dr4;
extern  RegDR5 dr5;
extern  RegDR6 dr6;
extern  RegDR7 dr7;

extern  RegDR0& db0;
extern  RegDR1& db1;
extern  RegDR2& db2;
extern  RegDR3& db3;
extern  RegDR4& db4;
extern  RegDR5& db5;
extern  RegDR6& db6;
extern  RegDR7& db7;

extern  RegES es;
extern  RegCS cs;
extern  RegSS ss;
extern  RegDS ds;
extern  RegFS fs;
extern  RegGS gs;



extern  prelude::One	_1;
extern  prelude::Two	_2;
extern  prelude::Three	_3;
extern  prelude::Four	_4;
extern  prelude::Five	_5;
extern  prelude::Six	_6;
extern  prelude::Seven	_7;
extern  prelude::Eight	_8;
extern  prelude::Nine	_9;

FarPtr16 far16(uint16_t selector,uint16_t offset);
FarPtr32 far32(uint16_t selector,uint32_t offset);
FarPtr64 far64(uint16_t selector,uint64_t offset);

}}




#endif // MY_X64_COMMON
