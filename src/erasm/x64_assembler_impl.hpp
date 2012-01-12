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
#ifndef ERASM_X64_ASSEMBLER_IMPL
#define ERASM_X64_ASSEMBLER_IMPL
#include <cstring>
#include "erasm/x64_ptr.hpp"
#include "erasm/x64_common.hpp"


namespace erasm { namespace x64 {
namespace impl {

struct UntypedRegister : public Register
{
   UntypedRegister(const Register& r)
      : Register(r)
      {}
   UntypedRegister(RegAL)
      : Register(0)
      {}
   UntypedRegister(RegAX)
      : Register(0)
      {}
   UntypedRegister(RegEAX)
      : Register(0)
      {}
   UntypedRegister(RegRAX)
      : Register(0)
      {}
   UntypedRegister(RegST0)
      : Register(0)
      {}
};





inline Segment get_segment(Segment s)
{ return s; }

inline Segment get_segment(const UntypedRegister& r)
{ return DefaultSegment; }


inline Segment get_segment(const PtrBase& p)
{ return p.get_segment(); }

template<class X,class Y>
inline Segment get_segment(const MOffset<X,Y>& p)
{ return p.get_segment(); }


enum OperandMode {OpNothing  ,Op16, Op32} ;
enum AddressMode {AddrNothing,Addr16,Addr32,Addr64} ;

struct PrefixBase
{
   explicit PrefixBase() :  size(0)   
      {} 

   int encode(code_ptr p) const
      {
	 impl::memcpy(p,bytes,size);
	 return size;
      }

   // invariant
   bool valid() const
      { return size <= max_prefix_size ; }

protected:
   void init_seg(Segment s)
      {
	 if (s != DefaultSegment) {
	    bytes[size] = (byte_t)s;
	    size += 1;
	 }
      }

   void insert_addr(bool addr) 
      { if (addr && noaddr()) 
	    bytes[size++] = (byte_t) Prefix_ADDRESS_SIZE ; } 

   bool noaddr() const
      {
	 // TODO:Make sure this is unrolled 
	 for (unsigned int i=0;i<size;i++) {
	    if (bytes[i] == Prefix_ADDRESS_SIZE) 
	       return false;
	 }
	 return true;
      }

   void push(PrefixByte x)
      {
	 if (x != Prefix_Nothing) {
	    assert(size < max_prefix_size);
	    bytes[size++] = x;

	 }
      }

   enum { max_prefix_size = 6 };
   unsigned int size;
   byte_t  bytes[max_prefix_size];
   
};


class RegAdd
{
public:
   explicit RegAdd(const UntypedRegister& r)
      : reg(r) {}
   int get_code() const
      { return reg.get_code() ; }

   bool is_ext_reg() const
      { return reg.is_ext_reg() ; }

private:
   const UntypedRegister & reg;
};


class Code 
{
public:
   explicit Code(byte_t c0) 
      {init(c0) ; }
   explicit Code(byte_t c0,const RegAdd& r) 
      { init(c0 + r.get_code()); }

   explicit Code(byte_t c0,byte_t c1) 
      { init(c0,c1); }

   explicit Code(byte_t c0,byte_t c1,const RegAdd& r) 
      { init(c0,c1 + r.get_code()); }

   explicit Code(byte_t c0,byte_t c1,byte_t c2) 
      { init(c0,c1,c2) ;}

   int encode(code_ptr p) const
      {
	 impl::memcpy(p,bytes,size);
	 return size;
      }

private:
   void init(byte_t c0)
      {
	 size=1;
	 bytes[0] = c0;
      }

   void init(byte_t c0,byte_t c1)
      {
	 size=2;
	 bytes[0] = c0;
	 bytes[1] = c1;
      }

   void init(byte_t c0,byte_t c1,byte_t c2)
      {
	 size=3;
	 bytes[0] = c0;
	 bytes[1] = c1;
	 bytes[2] = c2;
      }

   int    size;
   byte_t bytes[3];
};


struct Modrm 
{
   size_t size;
   ModrmBytes bytes;

   explicit Modrm(int mod,int op_reg,int rm)
      : size(1)
      {
	 bytes.mod = mod;
	 bytes.op_reg = op_reg;
	 bytes.rm = rm;
      }
   explicit Modrm(const PtrData& r,int op_reg)
      {
	 size = r.set_modrm(bytes);
	 bytes.op_reg = op_reg;
      }

   int encode(code_ptr p) const
      {
	 impl::memcpy(p,bytes.raw_bytes,size);
	 return size;
      }
};

inline 
Modrm
make_modrm(const UntypedRegister&  r,int i)
{
   Modrm modrm(3,i,r.get_code());
   return modrm;
}

inline
Modrm
make_modrm(const UntypedRegister&  r,const UntypedRegister& r2)
{ return make_modrm(r,r2.get_code());}

inline
Modrm
make_modrm (const PtrData&  r,int i)
{
   Modrm modrm(r,i);
   return modrm;
}
inline
Modrm
make_modrm (const PtrData&  r,const UntypedRegister& r2)
{ return make_modrm(r,r2.get_code()) ; }


inline byte_t rex_regadd(const RegAdd& r)
{ return r.is_ext_reg() ? RexB : 0 ; }
inline byte_t rex_modrm_rm(const PtrBase& p)
{ return p.get_rex_byte() ; }
inline byte_t rex_modrm_rm(const UntypedRegister& r)
{ return r.is_ext_reg() ? RexB : 0 ; }
inline byte_t rex_modrm_r(const UntypedRegister& r)
{ return r.is_ext_reg() ? RexR : 0 ; }

struct RexByteEncoder
{
   byte_t rex_;

   explicit RexByteEncoder()
      : rex_(0) {}
   explicit RexByteEncoder(byte_t rex) 
      : rex_(rex) 
      {
	 assert(rex == 0 || (0x40 <= rex && rex <= 0x4f) );
      }

   int encode(code_ptr p) const 
      {
	 if (rex_) {
	    *p = rex_;
	    return 1;
	 }
	 return 0;
      }
};

inline RexByteEncoder rexByte()
{ return RexByteEncoder() ; }
inline RexByteEncoder rexByte(byte_t x)
{ return RexByteEncoder(x) ; }
inline RexByteEncoder rexByte(byte_t x,byte_t y)
{ return RexByteEncoder(x|y) ; }
inline RexByteEncoder rexByte(byte_t x,byte_t y,byte_t z)
{ return RexByteEncoder(x|y|z) ; }

template<typename X>
struct Imm
{
public:
   explicit Imm(X x) : imm(x) {}

   int encode(code_ptr p) const
      {
	 * (X*)p = imm;
	 return sizeof(X);
      }

   X imm;
};

template<class X>
inline Imm<X> make_imm(X x)
{ return Imm<X>(x); }

inline const FarPtr16&  make_imm(const FarPtr16& x)
{ return x; }
inline const FarPtr32&  make_imm(const FarPtr32& x)
{ return x; }
inline const FarPtr64&  make_imm(const FarPtr64& x)
{ return x; }

template<class X>
inline int encode(code_ptr p,const X& x)
{ return x.encode(p) ; } 

inline int encode(code_ptr p,const FarPtr16& x)
{
   int n= sizeof(x);
   impl::memcpy(p,x.raw_bytes,n);
   return n;
}

inline int encode(code_ptr p,const FarPtr32& x)
{
   int n= sizeof(x);
   impl::memcpy(p,x.raw_bytes,n);
   return n;
}

inline int encode(code_ptr p,const FarPtr64& x)
{
   int n= sizeof(x);
   impl::memcpy(p,x.raw_bytes,n);
   return n;
}


template<class X0>
inline
int encode_instruction(code_ptr p,const X0& x)
{ return encode(p,x) ; }

template<class X0,class X1>
inline
int encode_instruction(code_ptr p,const X0& x0,const X1& x1)
{ 
   code_ptr p0 = p;
   p += encode(p,x0);
   p += encode(p,x1) ;
   return p - p0;
}

template<class X0,class X1,class X2>
inline
int encode_instruction(code_ptr p,const X0& x0,const X1& x1,const X2& x2)
{ 
   code_ptr p0 = p;
   p += encode(p,x0);
   p += encode(p,x1);
   p += encode(p,x2);
   return p - p0;
}

template<class X0,class X1,class X2,class X3>
inline
int encode_instruction(code_ptr p,
		       const X0& x0,
		       const X1& x1,
		       const X2& x2,
		       const X3& x3)
{ 
   code_ptr p0 = p;
   p += encode(p,x0);
   p += encode(p,x1);
   p += encode(p,x2);
   p += encode(p,x3);
   return p - p0;
}

template<class X0,class X1,class X2,class X3,class X4>
inline
int encode_instruction(code_ptr p,
		       const X0& x0,
		       const X1& x1,
		       const X2& x2,
		       const X3& x3,
		       const X4& x4)
{ 
   code_ptr p0 = p;
   p += encode(p,x0);
   p += encode(p,x1);
   p += encode(p,x2);
   p += encode(p,x3);
   p += encode(p,x4);
   return p - p0;
}

template<class X0,class X1,class X2,class X3,class X4,class X5>
inline
int encode_instruction(code_ptr p,
		       const X0& x0,
		       const X1& x1,
		       const X2& x2,
		       const X3& x3,
		       const X4& x4,
		       const X4& x5)

{ 
   code_ptr p0 = p;
   p += encode(p,x0);
   p += encode(p,x1);
   p += encode(p,x2);
   p += encode(p,x3);
   p += encode(p,x4);
   p += encode(p,x5);
   return p - p0;
}


}}}

namespace erasm { namespace x64 { namespace addr64 { namespace data32 {
namespace impl {
using namespace erasm::x64::impl;
class Prefix : public PrefixBase
{
public:
   typedef PrefixBase base;

   void init(OperandMode op,AddressMode addr,
	     PrefixByte c0,PrefixByte c1,PrefixByte c2)
      {
	 if (op == Op16) {
	    push(Prefix_OPERAND_SIZE);
	 }
	 if (addr == Addr32) {
	    push(Prefix_ADDRESS_SIZE);
	 }
	 push(c0);
	 push(c1);
	 push(c2);
      }

   explicit Prefix(OperandMode op,AddressMode addr,
		   PrefixByte c0=Prefix_Nothing,
		   PrefixByte c1=Prefix_Nothing,
		   PrefixByte c2=Prefix_Nothing)
      { init(op,addr,c0,c1,c2) ; }

   template<class X>
   explicit Prefix(OperandMode op,AddressMode addr,const X& x,
		   PrefixByte c0=Prefix_Nothing,
		   PrefixByte c1=Prefix_Nothing,
		   PrefixByte c2=Prefix_Nothing)
      { 
	 init(op,addr,c0,c1,c2);
	 init_seg(get_segment(x)) ; 
	 insert_addr(need_address_prefix(x));
      } 

private:

   bool need_address_prefix(const PtrBase& p)
      { return p.is_addr32(); }

   template<class X,class Y>
   bool need_address_prefix(const MOffset<X,Y>& p)
      { return p.is_addr32(); }

   bool need_address_prefix(Segment s)
      { return false; }

   bool need_address_prefix(const UntypedRegister& r)
      { return false; }

};
}
}}}}



#endif
