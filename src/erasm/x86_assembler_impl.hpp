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
#ifndef MY_ERASM_X86_ASSEMBLER_IMPL_HPP
#define MY_ERASM_X86_ASSEMBLER_IMPL_HPP

#include "erasm/x86.hpp"
#include "erasm/x64_assembler_impl.hpp"

namespace erasm {  namespace x86 { 
namespace impl {


using x64::impl::PrefixBase;
using x64::impl::RegAdd;
using x64::impl::Code;
using x64::impl::make_modrm;
using x64::impl::make_imm;
using x64::impl::rexByte;
using x64::impl::get_segment;
using x64::impl::UntypedRegister;
using x64::impl::PrefixBase;
using x64::impl::OperandMode;
using x64::impl::AddressMode;
using x64::impl::OpNothing;
using x64::impl::Op16;
using x64::impl::Op32;
using x64::impl::AddrNothing;
using x64::impl::Addr16;
using x64::impl::Addr32;

template<class X>
inline int rex_modrm_rm(const X& )
{ return 0; }
template<class X>
inline int rex_modrm_r(const X& )
{ return 0; }


struct RexByte { 
   RexByte(int) {}
   RexByte(int,int) {}
   int encode(code_ptr p) const
      { return 0 ; }
};



inline Segment get_segment(const PtrBase& p)
{ return p.get_segment(); }

template<class X,class Y>
inline Segment get_segment(const MOffset<X,Y>& p)
{ return p.get_segment(); }

}}}



namespace erasm { namespace x86 { namespace addr32  { namespace data32 {
namespace impl {
using namespace erasm::x86::impl;

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
	 if (addr == Addr16) {
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
	 init_seg(x86::impl::get_segment(x)) ; 
	 insert_addr(need_address_prefix(x));
      } 

private:

   bool need_address_prefix(const PtrBase& p)
      { return p.is_addr16(); }

   template<class X,class Y>
   bool need_address_prefix(const MOffset<X,Y>& p)
      { return p.is_addr16(); }

   bool need_address_prefix(Segment s)
      { return false; }

   bool need_address_prefix(const UntypedRegister& r)
      { return false; }

};

}
}}}}



#include "erasm/x64_assembler_impl.hpp"


#endif // MY_ERASM_X86_ASSEMBLER_IMPL_HPP
