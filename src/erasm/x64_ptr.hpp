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
#ifndef MY_X64_PTR
#define MY_X64_PTR

#include "erasm/x64_common.hpp"
#include "erasm/x64_addr64.hpp"
#include "erasm/intel_common.hpp"

namespace erasm {  namespace x64 { 
using namespace erasm::prelude::x64;


// g++ (and other portable compilers) doesn't analyse 
// implementation-dependent structures well,
// so we stick to a portable,standard-conformant represention
// and delay converting to a bit-level one until it is absolutely necessary.
// The downside is that we are forced to make many not-so-short functions
// inline to help the optimizer.


struct PtrData
{
   AddressingMode address_mode_;
   uint8_t  mod_;
   uint8_t  rm_;

   bool     has_sib_;
   uint8_t  sib_base_;
   uint8_t  sib_index_;
   uint8_t  sib_ss_;

   size_t   disp_size_;
   union {
      disp8_type  disp8_;
      disp16_type disp16_;
      disp32_type disp32_;
   };
   Segment  segment_;
   bool     rex_b_;
   bool     rex_x_;
   
   explicit PtrData(const ModrmConstHandle& m,
		    Segment segment = DefaultSegment,
		    bool rex_b = false,
		    bool rex_x = false)
      : address_mode_(m.get_address_mode())
      , mod_(m.get_mod())
      , rm_(m.get_rm())
      , disp_size_(m.get_disp_size())
      , segment_(segment) 
      , rex_b_(rex_b)
      , rex_x_(rex_x)
      {
	 set_sib(m);
	 set_disp(m);
      }

   explicit PtrData(AddressingMode address_mode,
		    uint8_t mod,uint8_t rm,
		    size_t disp_size,disp32_type disp,
		    Segment segment = DefaultSegment,
		    bool rex_b = false,
		    bool rex_x = false)
      : address_mode_(address_mode)
      , mod_(mod)
      , rm_(rm)
      , has_sib_(false)
      , disp_size_(disp_size)
      , disp32_(disp)
      , segment_(segment) 
      , rex_b_(rex_b)
      , rex_x_(rex_x)
      {}

   void set_disp(const ModrmConstHandle& expr) 
      {
	 size_t n = expr.get_disp_size();
	 assert(impl::oneof(n,0u,1u,2u,4u));
	 switch(n) {
	 case 0:
	    disp_size_ = 0;
	    break;
	 case 1:
	    disp_size_ = 1;
	    disp8_ = expr.get_disp8();
	    break;
	 case 2:
	    disp_size_ = 2;
	    disp16_ = expr.get_disp16();
	    break;
	 default:
	    disp_size_ = 4;
	    disp32_ = expr.get_disp32();
	    break;
	 }
      }

   template<class Expr>
   void set_sib(const Expr& expr) 
      {
	 if (expr.has_sib()) {
	    has_sib_  = true;
	    sib_base_  = expr.get_sib_base();
	    sib_index_ = expr.get_sib_index();
	    sib_ss_    = expr.get_sib_ss();
	 } else {
	    has_sib_ = false;
	 }
      }

   uint8_t get_mod() const
      { return mod_ ; }
   uint8_t get_rm() const
      { return rm_ ; }

   uint8_t get_sib_base() const
      { return sib_base_ ; }
   uint8_t get_sib_index() const
      { return sib_index_ ; }
   uint8_t get_sib_ss() const
      { return sib_ss_ ; }

   disp8_type get_disp8() const
      { return disp8_ ; }
   disp16_type get_disp16() const
      { return disp16_ ; }
   disp32_type get_disp32() const
      { return disp32_ ; }

   bool has_sib() const
      { return has_sib_ ; }

   size_t get_disp_size() const
      { return disp_size_ ; }

   Segment get_segment() const
      { return segment_ ; }

   AddressingMode get_address_mode() const
      { return address_mode_ ; }

   bool is_addr16() const
      { return address_mode_ == ADDRESS_MODE_16 ; }
   bool is_addr32() const
      { return address_mode_ == ADDRESS_MODE_32 ; }
   bool is_addr64() const
      { return address_mode_ == ADDRESS_MODE_64 ; }

   bool get_rex_b() const
      { return rex_b_ ; }
   bool get_rex_x() const
      { return rex_x_ ; }

   byte_t get_rex_byte() const
      { return (rex_b_ ? RexB : 0) | (rex_x_ ? RexX : 0) ; }



   int set_modrm(ModrmBytes& bytes) const
      {
	 bytes.mod = mod_;
	 bytes.rm = rm_;
	 if (has_sib_) {
	    bytes.sib.base  = sib_base_;
	    bytes.sib.index = sib_index_;
	    bytes.sib.ss = sib_ss_;
	    assert(impl::oneof(disp_size_,0u,1u,4u));
	    switch(disp_size_) {
	    case 0: 
	       break;
	    case 1: 
	       bytes.sib.disp8  = disp8_;
	       break;
	    default:
	       bytes.sib.disp32 = disp32_;
	    }

	 } else {
	    assert(impl::oneof(disp_size_,0u,1u,2u,4u));
	    switch(disp_size_) {
	    case 0: 
	       break;
	    case 1: 
	       bytes.disp8  = disp8_;
	       break;
	    case 2: 
	       bytes.disp16  = disp16_;
	       break;
	    default:
	       bytes.disp32 = disp32_;
	    }
	 }
	 size_t n = get_rep_size();
	 return n;
      }
   size_t get_rep_size() const
      { return 1 + (has_sib_ ? 1 : 0) + disp_size_ ; }

};

class PtrBase : public PtrData 
{
public:
   typedef PtrData base;

   explicit PtrBase(const ModrmConstHandle& m,
		    Segment seg,
		    const RexByte& rex)
      : base(m,seg,rex.b,rex.x)
      {}

   template<class B,class I,class S,class D>
   explicit PtrBase(const EffectiveAddress<B,I,S,D>& expr,
		    bool is_addr32,
		    Segment seg)
      : base (is_addr32 ? ADDRESS_MODE_32 : ADDRESS_MODE_64,
	      expr.get_mod(),
	      expr.get_rm(),
	      expr.get_disp_size(),
	      expr.get_disp(),
	      seg,
	      expr.get_rex_b(),
	      expr.get_rex_x() )
      {
	 assert(! expr.is16bit() );
	 set_sib(expr);
      }

};



inline Segment get_segment_code(const RegES&) { return Segment_ES ; }
inline Segment get_segment_code(const RegCS&) { return Segment_CS ; }
inline Segment get_segment_code(const RegSS&) { return Segment_SS ; }
inline Segment get_segment_code(const RegDS&) { return Segment_DS ; }
inline Segment get_segment_code(const RegFS&) { return Segment_FS ; }
inline Segment get_segment_code(const RegGS&) { return Segment_GS ; }



template<class X>
class Ptr : public PtrBase
{
public:
   typedef PtrBase base;
   typedef X value_type;

   explicit Ptr(const ModrmConstHandle& m,Segment seg,
		const RexByte& rex)
      : base(m,seg,rex)
      {}

   template<class Base,class Index,class Scale,class Disp>
   explicit Ptr (const EffectiveAddress<Base,Index,Scale,Disp>& expr,
		 bool is_addr32 = false,
		 Segment seg = DefaultSegment)
      : base(expr,
	     is_addr32,
	     seg)
      {
	 assert(expr.has_sib() == has_sib());
	 assert(expr.get_disp_size() == get_disp_size());
      }

   const base & get_base() const
      { return *this ; }
};


struct Error_PtrRex_should_not_be_instantiated_with_expressions_without_rex;
template<class X>
class PtrRex : public Ptr<X>
{
   typedef Ptr<X> base;
   typedef X value_type;

public:
   template<class Base,class Index,class Scale,class Disp>
   explicit PtrRex (const EffectiveAddress<Base,Index,Scale,Disp>& exp,
		    bool is_addr32=false,
		    Segment seg = DefaultSegment)
      : base(exp,is_addr32,seg)
      {
	 using namespace erasm::prelude;
	 typedef EVAL_TEMPLATE((ifn <isRexAddress<EffectiveAddress<Base,Index,Scale,Disp> > , 
				     Zero ,
				     error<Error_PtrRex_should_not_be_instantiated_with_expressions_without_rex> > )) x;
      }

   const base & get_base() const
      { return *this ; }
};


struct Error_PtrNoRex_should_not_be_instantiated_with_expressions_with_rex;
template<class X>
class PtrNoRex : public Ptr<X>
{
   typedef Ptr<X> base;
   typedef X value_type;
public:
   template<class Base,class Index,class Scale,class Disp>
   explicit PtrNoRex (const EffectiveAddress<Base,Index,Scale,Disp>& exp,
		      bool is_addr32=false,
		      Segment seg = DefaultSegment)
      : base(exp,is_addr32,seg)
      {
	 using namespace erasm::prelude;
	 typedef EVAL_TEMPLATE((ifn <not_<isRexAddress<EffectiveAddress<Base,Index,Scale,Disp> > > , Zero ,
				     error<Error_PtrNoRex_should_not_be_instantiated_with_expressions_with_rex> > )) x;
      }
};


template<class X>
class Ptr_RBX : public PtrNoRex<X>
{
   typedef PtrNoRex<X> base;
   typedef X value_type;
   typedef EVAL((effectiveAddressFromReg< RegRBX >)) expr;
public:
   explicit Ptr_RBX (Segment seg)
      : base(expr(),false,seg) 
      {}
};

template<class X>
class Ptr_EBX : public PtrNoRex<X>
{
   typedef PtrNoRex<X> base;
   typedef X value_type;
   typedef EVAL((effectiveAddressFromReg< RegEBX >)) expr;
public:
   explicit Ptr_EBX (Segment seg)
      : base(expr(),true,seg) 
      {}
};


template<class X>
class Ptr_RSI : public PtrNoRex<X>
{
   typedef PtrNoRex<X> base;
   typedef X value_type;
   typedef EVAL((effectiveAddressFromReg< RegRSI >)) expr;
public:
   explicit Ptr_RSI (Segment seg)
      : base(expr(),false,seg) 
      {}
};

template<class X>
class Ptr_ESI : public PtrNoRex<X>
{
   typedef PtrNoRex<X> base;
   typedef X value_type;
   typedef EVAL((effectiveAddressFromReg< RegESI >)) expr;
public:
   explicit Ptr_ESI (Segment seg)
      : base(expr(),true,seg) 
      {}
};


template<class X,Segment seg>
class Ptr_RDI : public PtrNoRex<X>
{
   typedef PtrNoRex<X> base;
   typedef X value_type;
   typedef EVAL((effectiveAddressFromReg< RegRDI >)) expr;
public:
   explicit Ptr_RDI ()
      : base(expr(),false,seg) 
      {}
};


template<class X,Segment seg>
class Ptr_EDI : public PtrNoRex<X>
{
   typedef PtrNoRex<X> base;
   typedef X value_type;
   typedef EVAL((effectiveAddressFromReg< RegEDI >)) expr;
public:
   explicit   Ptr_EDI ()
      : base(expr(),true,seg) 
      {}
};



template<class OffsetType,class ValType,bool no_segment=false>
class MOffset
{
public:
   typedef uint64_t    default_offset_type;
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
   bool is_addr32() const
      { return sizeof(offset_type) != sizeof(default_offset_type) ; }

   offset_type get_offset() const
      { return offset ; }
   int get_offset_size() const
      { return sizeof(offset) ; }


   int encode(code_ptr p) const
      { 
	 *(offset_type*) p = offset;
	 return sizeof(offset);
      }

private:
   Segment segment;
   offset_type offset;
};

typedef MOffset<uint64_t,uint8_t>  ByteOffset;
typedef MOffset<uint64_t,uint16_t> WordOffset;
typedef MOffset<uint64_t,uint32_t> DwordOffset;
typedef MOffset<uint64_t,uint64_t> QwordOffset;

typedef MOffset<uint64_t,uint8_t>  ByteOffset64;
typedef MOffset<uint64_t,uint16_t> WordOffset64;
typedef MOffset<uint64_t,uint32_t> DwordOffset64;
typedef MOffset<uint64_t,uint64_t> QwordOffset64;

typedef MOffset<uint32_t,uint8_t>  ByteOffset32;
typedef MOffset<uint32_t,uint16_t> WordOffset32;
typedef MOffset<uint32_t,uint32_t> DwordOffset32;
typedef MOffset<uint32_t,uint64_t> QwordOffset32;

// TODO
struct ByteOffset32Rex : public ByteOffset32 {};
struct ByteOffset64Rex : public ByteOffset64 {};


namespace impl {
using namespace erasm::prelude ;

template<class X,Segment seg = DefaultSegment >
class PtrProxy {
public:

   typedef PtrNoRex<X>         pointer_norex_type;
   typedef PtrRex<X>           pointer_rex_type;

   typedef Ptr_RBX<X>          rbx_pointer_type;
   typedef Ptr_EBX<X>          ebx_pointer_type;

   typedef Ptr_RSI<X>          rsi_pointer_type;
   typedef Ptr_ESI<X>          esi_pointer_type;
   typedef Ptr_RDI<X,seg>      rdi_pointer_type;
   typedef Ptr_EDI<X,seg>      edi_pointer_type;
   typedef MOffset<offset64_t,X> offset_type;
   typedef MOffset<offset32_t,X> offset32_type;

   template<class P,class D>
   EVAL_TEMPLATE((ifn<isRexExpr<Expr<P,D> > , 
		      pointer_rex_type , 
		      pointer_norex_type >))
   operator[] (const Expr<P,D>& expr) const
      {
	 typedef EVAL_TEMPLATE((ifn<isRexExpr<Expr<P,D> > , 
				    pointer_rex_type , 
				    pointer_norex_type >)) result;

	 typedef EVAL_TEMPLATE((effectiveAddressFromExpr<Expr<P,D> >))
	    expr_type;
	 return result(expr_type(expr.get_disp()),false,seg);
      }

   template<int i>
   EVAL_TEMPLATE((ifn<isRexReg<Register64<i> >, 
		      pointer_rex_type , 
		      pointer_norex_type >))
   operator[] (const Register64<i>&) const
      {
	 typedef EVAL_TEMPLATE((ifn<isRexReg<Register64<i> > , 
				    pointer_rex_type , 
				    pointer_norex_type >)) result_type;
	 typedef EVAL_TEMPLATE((effectiveAddressFromReg<Register64 <i> >  )) 
	    expr_type;
	 return result_type(expr_type(),false,seg);
      }

   template<class P,class D>
   EVAL_TEMPLATE((ifn<isRexExpr<Expr<P,D> > , 
		      pointer_rex_type , 
		      pointer_norex_type >))
   operator [] (const Expr32<P,D>& expr) const
      {
	 typedef EVAL_TEMPLATE((ifn<isRexExpr<Expr<P,D> > , 
				    pointer_rex_type , 
				    pointer_norex_type >)) result_type;
	 typedef EVAL_TEMPLATE((effectiveAddressFromExpr<Expr<P,D> >  )) 
	    expr_type;
	 return result_type(expr_type(expr.get_disp()),true,seg);
      }

   template<int i>
   EVAL_TEMPLATE((ifn<isRexReg<Register64<i> > , 
		      pointer_rex_type , 
		      pointer_norex_type >)) 
   operator[] (const Register32<i>&) const
      {
	 typedef EVAL_TEMPLATE((ifn<isRexReg<Register64<i> > , 
				    pointer_rex_type , 
				    pointer_norex_type >)) result_type;
	 typedef EVAL_TEMPLATE((effectiveAddressFromReg<Register64 <i> >  )) 
	    expr_type;
	 return result_type( expr_type() ,true,seg);
      }


   /* when disp32 is used alone,then it is used as an unsigned value? */
      struct ERROR_When_disp32_used_alone_it_must_be_an_unsigned_offset;
   
   pointer_norex_type
   operator[]  (disp32_type) const
      {
	 ::erasm::prelude::error<ERROR_When_disp32_used_alone_it_must_be_an_unsigned_offset> r;
	 return r();
      }

   pointer_norex_type
   operator[] (offset32_t disp) const
      { return pointer_norex_type(EffectiveAddress64Disp(disp),false,seg);  }

   // It seems to be too confusing to
   // have different interfaces for disp32_type,offset64_t,etc.

   // offset_type operator [] (offset64_t disp) const
   //    { return offset_type(disp,seg) ; }

   rsi_pointer_type operator [] (const RegRSI& ) const
      { return rsi_pointer_type (seg) ; }
  
   esi_pointer_type operator [] (const RegESI& ) const
      { return esi_pointer_type (seg) ; }

   rdi_pointer_type operator [] (const RegRDI& ) const
      { return rdi_pointer_type () ; }
  
   edi_pointer_type operator [] (const RegEDI& ) const
      { return edi_pointer_type () ; }

   rbx_pointer_type operator [] (const RegRBX& ) const
      { return rbx_pointer_type (seg) ; }
  
   ebx_pointer_type operator [] (const RegEBX& ) const
      { return ebx_pointer_type (seg) ; }
  
};
}
using impl::PtrProxy;

template<class X>
class PtrProxyContainer : public PtrProxy<X,DefaultSegment>
{
public:
   typedef PtrProxy<X,DefaultSegment>  base_type;

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
      typedef MOffset<offset64_t,X> offset_type;
      typedef MOffset<offset32_t,X> offset32_type;

   offset32_type
   operator[] (offset32_t disp) const
      { return offset32_type(disp,seg);  }

   offset_type
   operator [] (offset64_t disp) const
      { return offset_type(disp,seg) ; }

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
// We need a special treatment for byte pointers
typedef Ptr     <uint8_t>  BytePtr;
typedef PtrNoRex<uint8_t>  BytePtr86;
typedef PtrRex  <uint8_t>  BytePtrRex;

typedef Ptr<uint16_t> WordPtr;
typedef Ptr<uint32_t> DwordPtr;
typedef Ptr<uint64_t> QwordPtr;
typedef Ptr<MmWord>   MmWordPtr;
typedef Ptr<XmmWord>  XmmWordPtr;
typedef Ptr<Fword>    FwordPtr;
typedef Ptr<Tbyte>    TbytePtr;
typedef Ptr<Oword>    OwordPtr;
typedef Ptr<Real4>    Real4Ptr;
typedef Ptr<Real8>    Real8Ptr;
typedef Ptr<Real10>   Real10Ptr;
typedef Ptr<FarPtr16> Far16Ptr;
typedef Ptr<FarPtr32> Far32Ptr;
typedef Ptr<FarPtr64> Far64Ptr;

typedef Ptr_EBX<uint8_t>  BytePtr_EBX;
typedef Ptr_RBX<uint8_t>  BytePtr_RBX;

typedef Ptr_RSI<uint8_t>  BytePtr_RSI;
typedef Ptr_RSI<uint16_t> WordPtr_RSI;
typedef Ptr_RSI<uint32_t> DwordPtr_RSI;
typedef Ptr_RSI<uint64_t> QwordPtr_RSI;

typedef Ptr_ESI<uint8_t>  BytePtr_ESI;
typedef Ptr_ESI<uint16_t> WordPtr_ESI;
typedef Ptr_ESI<uint32_t> DwordPtr_ESI;
typedef Ptr_ESI<uint64_t> QwordPtr_ESI;

typedef Ptr_RDI<uint8_t ,Segment_ES>  BytePtr_ES_RDI;
typedef Ptr_RDI<uint16_t,Segment_ES>  WordPtr_ES_RDI;
typedef Ptr_RDI<uint32_t,Segment_ES>  DwordPtr_ES_RDI;
typedef Ptr_RDI<uint64_t,Segment_ES>  QwordPtr_ES_RDI;

typedef Ptr_EDI<uint8_t ,Segment_ES> BytePtr_ES_EDI;
typedef Ptr_EDI<uint16_t,Segment_ES> WordPtr_ES_EDI;
typedef Ptr_EDI<uint32_t,Segment_ES> DwordPtr_ES_EDI;
typedef Ptr_EDI<uint64_t,Segment_ES> QwordPtr_ES_EDI;
	
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
extern PtrProxyContainer<FarPtr64> far64_ptr;

extern OffsetProxyContainer<uint8_t> byte_offset;		
extern OffsetProxyContainer<uint16_t> word_offset;		
extern OffsetProxyContainer<uint32_t> dword_offset;	
extern OffsetProxyContainer<uint64_t> qword_offset;


}}

namespace erasm { namespace prelude {

DEFINE_FOREIGN_DATA1(x64::PtrRex,class);
DEFINE_FOREIGN_DATA1(x64::PtrNoRex,class);

}}

#endif // MY_X64_PTR
