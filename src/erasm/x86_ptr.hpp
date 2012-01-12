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
#ifndef MY_ERASM_X86_PTR_HPP
#define MY_ERASM_X86_PTR_HPP

#include "erasm/x86_common.hpp"
#include "erasm/x86_addr16.hpp"
#include "erasm/x86_addr32.hpp"
#include "erasm/x64_ptr.hpp"

namespace erasm {  namespace x86 {
using namespace erasm::prelude::x86;
using erasm::prelude::x64::operator+;
using erasm::prelude::x64::operator*;

template<class OffsetType,class ValType,bool no_segment=false>
class MOffset
{
public:
   typedef uint32_t    default_offset_type;
   typedef OffsetType  offset_type;
   typedef ValType     value_type;

   template<int i>
   MOffset(offset_type o,const RegisterSeg<i>& s)
      : segment(get_segment_code(s)) , offset(o)
      {}

   MOffset(offset_type o,Segment s)
      : segment(s) , offset(o)
      {}

   Segment get_segment() const
      { return segment ; }
   PrefixByte get_segment_prefix() const
      { return (PrefixByte) segment ; }
   bool is_addr16() const
      { return sizeof(offset_type) != sizeof(default_offset_type) ; }

   offset_type get_offset() const
      { return offset ; }
   int get_offset_size() const
      { return sizeof(offset) ; }

   int encode(code_ptr p) const
      {
	 * (offset_type*) p = offset ;
	 return sizeof(offset);
      }

private:
   Segment segment;
   offset_type offset;
};

typedef MOffset<uint32_t,uint8_t>  ByteOffset32;
typedef MOffset<uint32_t,uint16_t> WordOffset32;
typedef MOffset<uint32_t,uint32_t> DwordOffset32;
typedef MOffset<uint32_t,uint64_t> QwordOffset32;

typedef MOffset<uint16_t,uint8_t>  ByteOffset16;
typedef MOffset<uint16_t,uint16_t> WordOffset16;
typedef MOffset<uint16_t,uint32_t> DwordOffset16;
typedef MOffset<uint16_t,uint64_t> QwordOffset16;

}}

namespace erasm {  namespace x86 {
using erasm::x64::PtrData;
using erasm::x64::AddressingMode;
using erasm::x64::ADDRESS_MODE_16;
using erasm::x64::ADDRESS_MODE_32;
using erasm::prelude::x64::EffectiveAddress;
using erasm::prelude::x64::effectiveAddressFromReg;


class PtrBase : public  PtrData
{
public:
   typedef PtrData base;
   explicit PtrBase(const ModrmConstHandle& m, Segment seg)
      : base(m,seg) 
      {}

   template<class Base,class Index,class Scale,class Disp>
   explicit PtrBase(const EffectiveAddress<Base,Index,Scale,Disp> & expr,
		    Segment seg)
      : base(ADDRESS_MODE_32,
	     expr.get_mod(),
	     expr.get_rm(),
	     expr.get_disp_size(),
	     expr.get_disp(),
	     seg)
      {
	 set_sib(expr);
      }
   template<class Base,class Index,class Disp>
   explicit PtrBase(const EffectiveAddress16<Base,Index,Disp> & expr,
		    Segment seg)
      : base(ADDRESS_MODE_16,
	     expr.get_mod(),
	     expr.get_rm(),
	     expr.get_disp_size(),
	     expr.get_disp(),
	     seg)
      {
      }
};

template<class X>
class Ptr : public PtrBase
{
public:
   typedef PtrBase base;
   typedef X value_type;

   explicit Ptr(const ModrmConstHandle& m, Segment seg)
      : base(m,seg)
      {}

   template<class Base,class Index,class Scale,class Disp>
   explicit Ptr (const EffectiveAddress<Base,Index,Scale,Disp>& expr,
		 Segment seg = DefaultSegment)
      : base(expr, seg)
      {
	 using namespace erasm::prelude;
	 using namespace erasm::prelude::x64;
	 typedef EffectiveAddress<Base,Index,Scale,Disp> E;
	 typedef EVAL_TEMPLATE((if_<isRexAddress<E>,
				    error<Error_Extended_registers_not_available_in_x86_mode,Base,Index> , Zero>)) z;
      }

   template<class Base,class Index,class Disp>
   explicit Ptr (const EffectiveAddress16<Base,Index,Disp>& expr,
		 Segment seg = DefaultSegment)
      : base (expr, seg)
      {
      }

   const base & get_base() const
      { return *this ; }
};



template<class X>
class Ptr_EBX : public Ptr<X>
{
   typedef Ptr<X> base;
   typedef X value_type;
   typedef EVAL((effectiveAddressFromReg< RegEBX >)) expr;
public:
   explicit Ptr_EBX (Segment seg)
      : base(expr(),seg)
      {}
};

template<class X>
class Ptr_BX : public Ptr<X>
{
   typedef Ptr<X> base;
   typedef X value_type;
   typedef EVAL((effectiveAddress16FromReg< RegBX >)) expr;
public:
   explicit Ptr_BX (Segment seg)
      : base(expr(),seg)
      {}
};


template<class X>
class Ptr_ESI : public Ptr<X>
{
   typedef Ptr<X> base;
   typedef X value_type;
   typedef EVAL((effectiveAddressFromReg< RegESI >)) expr;
public:
   explicit Ptr_ESI (Segment seg)
      : base(expr(),seg)
      {}
};

template<class X>
class Ptr_SI : public Ptr<X>
{
   typedef Ptr<X> base;
   typedef X value_type;
   typedef EVAL_TEMPLATE((x86::effectiveAddress16FromReg< RegSI >)) expr;
public:
   explicit Ptr_SI (Segment seg)
      : base(expr(),seg)
      {}
};


template<class X,Segment seg>
class Ptr_EDI : public Ptr<X>
{
   typedef Ptr<X> base;
   typedef X value_type;
   typedef EVAL((effectiveAddressFromReg< RegEDI >)) expr;
public:
   explicit Ptr_EDI ()
      : base(expr(),seg)
      {}
};


template<class X,Segment seg>
class Ptr_DI : public Ptr<X>
{
   typedef Ptr<X> base;
   typedef X value_type;
   typedef EVAL((x86::effectiveAddress16FromReg< RegDI >)) expr;
public:
   explicit   Ptr_DI ()
      : base(expr(),seg)
      {}
};


template<class X,Segment seg = DefaultSegment >
class PtrProxy {
public:
   typedef Ptr<X>       pointer_type;
   typedef Ptr_EBX<X>   ebx_pointer_type;
   typedef Ptr_BX<X>    bx_pointer_type;
   typedef Ptr_ESI<X>   esi_pointer_type;
   typedef Ptr_SI<X>    si_pointer_type;
   typedef Ptr_EDI<X,seg> edi_pointer_type;
   typedef Ptr_DI<X,seg>  di_pointer_type;
   typedef MOffset<offset32_t,X> offset32_type;
   typedef MOffset<offset16_t,X> offset16_type;


   template<class P,class D>
   pointer_type operator [] (const x64::Expr32<P,D>& expr) const
      {
	 using namespace erasm::prelude;
	 using namespace erasm::prelude::x64;
	 typedef EVAL_TEMPLATE((effectiveAddressFromExpr<Expr<P,D> >)) E;
	 EVAL_TEMPLATE((if_<isRexAddress<E> , 
			error < Error_Extended_registers_not_available_in_x86_mode> ,
			Zero > )) zero;
	 return pointer_type(E(expr.get_disp()),seg);
      }

   template<int i>
   pointer_type operator[] (const Register32<i>&) const
      {
	 using namespace erasm::prelude;
	 using namespace erasm::prelude::x64;
	 typedef EVAL_TEMPLATE((effectiveAddressFromReg<Register32 <i> >)) E;
	 EVAL_TEMPLATE((if_<isRexAddress<E> , 
			error < Error_Extended_registers_not_available_in_x86_mode> ,
			Zero > )) zero;
	 return pointer_type(E(),seg);
      }

   template<class P,class D>
   pointer_type operator [] (const Expr16<P,D>& expr) const
      {
	 typedef EVAL_TEMPLATE((effectiveAddress16FromExpr16<Expr16<P,D> >)) E;
	 return pointer_type(E(expr.get_disp()),seg);
      }

   template<int i>
   pointer_type operator[] (const Register16<i>&) const
      {
	 typedef EVAL_TEMPLATE((effectiveAddress16FromReg<Register16 <i> >  ))
	    E;
	 return pointer_type(E(),seg);
      }

   pointer_type operator[] (offset32_t disp) const
      {  
	 assert(! x64::EffectiveAddress32Disp(disp).is16bit());
	 return pointer_type(x64::EffectiveAddress32Disp(disp) , seg) ; 
      }

   pointer_type operator[] (offset16_t disp) const
      {
	 assert(x86::EffectiveAddress16Disp(disp).is16bit());
	 return pointer_type(x86::EffectiveAddress16Disp(disp) , seg) ; 
      }


   esi_pointer_type operator[] (const RegESI& ) const
      { return esi_pointer_type (seg) ; }
   si_pointer_type operator[] (const RegSI& ) const
      { return si_pointer_type (seg) ; }

   edi_pointer_type operator[] (const RegEDI& ) const
      { return edi_pointer_type () ; }
   di_pointer_type operator[] (const RegDI& ) const
      { return di_pointer_type () ; }

   ebx_pointer_type operator [] (const RegEBX& ) const
      { return ebx_pointer_type (seg) ; }
   bx_pointer_type operator [] (const RegBX& ) const
      { return bx_pointer_type (seg) ; }

};


template<class X>
class PtrProxyContainer : public PtrProxy<X,DefaultSegment>
{
public:
   typedef PtrProxy<X,DefaultSegment> base_type;

   base_type& base()
      { return *this ; }
   const base_type& base() const
      { return *this ; }

   PtrProxy<X,Segment_ES>  es;
   PtrProxy<X,Segment_CS>  cs;
   PtrProxy<X,Segment_SS>  ss;
   PtrProxy<X,Segment_DS>  ds;
   PtrProxy<X,Segment_FS>  fs;
   PtrProxy<X,Segment_GS>  gs;
};

template<class X,Segment seg = DefaultSegment >
class OffsetProxy {
public:
   typedef MOffset<offset16_t,X> offset16_type;
   typedef MOffset<offset32_t,X> offset32_type;

   offset32_type
   operator[] (offset32_t disp) const
      { return offset32_type(disp,seg);  }

   offset16_type
   operator [] (offset16_t disp) const
      { return offset16_type(disp,seg) ; }

};


template<class X>
class OffsetProxyContainer : public OffsetProxy<X,DefaultSegment>
{
public:
   typedef OffsetProxy<X,DefaultSegment>  base_type;

   base_type& base()
      { return *this ; }
   const base_type& base() const
      { return *this ; }

   OffsetProxy<X,Segment_ES>  es;
   OffsetProxy<X,Segment_CS>  cs;
   OffsetProxy<X,Segment_SS>  ss;
   OffsetProxy<X,Segment_DS>  ds;
   OffsetProxy<X,Segment_FS>  fs;
   OffsetProxy<X,Segment_GS>  gs;
};



   typedef Ptr<void>     VoidPtr;
typedef Ptr<uint8_t>  BytePtr;
typedef Ptr<uint8_t>  BytePtr86;
   typedef Ptr<uint16_t> WordPtr;
   typedef Ptr<uint32_t> DwordPtr;
   typedef Ptr<uint64_t> QwordPtr;
   typedef Ptr<MmWord>   MmWordPtr;
   typedef Ptr<XmmWord>  XmmWordPtr;
   typedef Ptr<XmmWord>  XmmWordPtr;
   typedef Ptr<Fword>    FwordPtr;
   typedef Ptr<Tbyte>    TbytePtr;
   typedef Ptr<Oword>    OwordPtr;
   typedef Ptr<Real4>    Real4Ptr;
   typedef Ptr<Real8>    Real8Ptr;
   typedef Ptr<Real10>   Real10Ptr;
   typedef Ptr<FarPtr16> Far16Ptr;
   typedef Ptr<FarPtr32> Far32Ptr;

typedef Ptr_BX<uint8_t>  BytePtr_BX;
typedef Ptr_EBX<uint8_t>  BytePtr_EBX;

   typedef Ptr_ESI<uint8_t>  BytePtr_ESI;
   typedef Ptr_ESI<uint16_t> WordPtr_ESI;
   typedef Ptr_ESI<uint32_t> DwordPtr_ESI;
   typedef Ptr_ESI<uint64_t> QwordPtr_ESI;

   typedef Ptr_SI<uint8_t>  BytePtr_SI;
   typedef Ptr_SI<uint16_t> WordPtr_SI;
   typedef Ptr_SI<uint32_t> DwordPtr_SI;
   typedef Ptr_SI<uint64_t> QwordPtr_SI;

typedef Ptr_EDI<uint8_t ,Segment_ES> BytePtr_ES_EDI;
typedef Ptr_EDI<uint16_t,Segment_ES> WordPtr_ES_EDI;
typedef Ptr_EDI<uint32_t,Segment_ES> DwordPtr_ES_EDI;
typedef Ptr_EDI<uint64_t,Segment_ES> QwordPtr_ES_EDI;

typedef Ptr_DI<uint8_t ,Segment_ES> BytePtr_ES_DI;
typedef Ptr_DI<uint16_t,Segment_ES> WordPtr_ES_DI;
typedef Ptr_DI<uint32_t,Segment_ES> DwordPtr_ES_DI;
typedef Ptr_DI<uint64_t,Segment_ES> QwordPtr_ES_DI;


extern PtrProxyContainer<void> void_ptr;
extern PtrProxyContainer<uint8_t> byte_ptr;
extern PtrProxyContainer<uint16_t> word_ptr;
extern PtrProxyContainer<uint32_t> dword_ptr;
extern PtrProxyContainer<uint64_t> qword_ptr;
extern PtrProxyContainer<MmWord> mmword_ptr;
extern PtrProxyContainer<XmmWord> xmmword_ptr;
extern PtrProxyContainer<Fword>   fword_ptr;
extern PtrProxyContainer<Oword>   oword_ptr;
extern PtrProxyContainer<Tbyte>   tbyte_ptr;
extern PtrProxyContainer<Real4>   real4_ptr;
extern PtrProxyContainer<Real8>   real8_ptr;
extern PtrProxyContainer<Real10>  real10_ptr;
extern PtrProxyContainer<FarPtr16> far16_ptr;
extern PtrProxyContainer<FarPtr32> far32_ptr;

extern OffsetProxyContainer<uint8_t> byte_offset;
extern OffsetProxyContainer<uint16_t> word_offset;
extern OffsetProxyContainer<uint32_t> dword_offset;
extern OffsetProxyContainer<uint64_t> qword_offset;

}}

#endif // MY_ERASM_X86_PTR_HPP
