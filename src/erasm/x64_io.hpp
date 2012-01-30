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
#ifndef MY_ERASM_X64_IO_HPP
#define MY_ERASM_X64_IO_HPP

#include "erasm/x64.hpp"
#include <iostream>
#include <iomanip>
#include <stdio.h>


namespace erasm { namespace prelude { 

template<int x>
inline std::ostream& operator<< (std::ostream& os,prelude::Int<x>)
{ return os << x ; }

}}


namespace erasm {  namespace x64 {

///////////////////////////////////////////////////////////////////////////
//           Forward declarations                                        //
///////////////////////////////////////////////////////////////////////////

std::ostream& operator<< (std::ostream& os,Segment seg);

// Adding template<> to make it a list of explicit instantiations
// would reduce work a bit, at the cost of uglifying the code.
// I think I will not do that for now.

inline std::ostream& operator<< (std::ostream& os,const RegRIP& r);
inline std::ostream& operator<< (std::ostream& os,const ByteReg86& r);
inline std::ostream& operator<< (std::ostream& os,const ByteRegRex& r);
inline std::ostream& operator<< (std::ostream& os,const WordReg& r);
inline std::ostream& operator<< (std::ostream& os,const DwordReg& r);
inline std::ostream& operator<< (std::ostream& os,const QwordReg& r);
inline std::ostream& operator<< (std::ostream& os,const MmReg& r);
inline std::ostream& operator<< (std::ostream& os,const XmmReg& r);
inline std::ostream& operator<< (std::ostream& os,const StReg& r);
inline std::ostream& operator<< (std::ostream& os,const SegReg& r);
inline std::ostream& operator<< (std::ostream& os,const CrReg& r);
inline std::ostream& operator<< (std::ostream& os,const DrReg& r);

inline std::ostream& operator<< (std::ostream& os,const VoidPtr& ptr);
inline std::ostream& operator<< (std::ostream& os,const BytePtr& ptr);
inline std::ostream& operator<< (std::ostream& os,const BytePtr86& ptr);
inline std::ostream& operator<< (std::ostream& os,const BytePtrRex& ptr);
inline std::ostream& operator<< (std::ostream& os,const WordPtr& ptr);
inline std::ostream& operator<< (std::ostream& os,const DwordPtr& ptr);
inline std::ostream& operator<< (std::ostream& os,const QwordPtr& ptr);
inline std::ostream& operator<< (std::ostream& os,const MmWordPtr& ptr);
inline std::ostream& operator<< (std::ostream& os,const XmmWordPtr& ptr);
inline std::ostream& operator<< (std::ostream& os,const FwordPtr& ptr);
inline std::ostream& operator<< (std::ostream& os,const TbytePtr& ptr);
inline std::ostream& operator<< (std::ostream& os,const OwordPtr& ptr);
inline std::ostream& operator<< (std::ostream& os,const Real4Ptr& ptr);
inline std::ostream& operator<< (std::ostream& os,const Real8Ptr& ptr);
inline std::ostream& operator<< (std::ostream& os,const Far16Ptr& ptr);
inline std::ostream& operator<< (std::ostream& os,const Far32Ptr& ptr);
inline std::ostream& operator<< (std::ostream& os,const Far64Ptr& ptr);

inline std::ostream& operator<< (std::ostream& os,const ByteOffset& offs);
inline std::ostream& operator<< (std::ostream& os,const WordOffset& offs);
inline std::ostream& operator<< (std::ostream& os,const DwordOffset& offs);
inline std::ostream& operator<< (std::ostream& os,const QwordOffset& offs);
inline std::ostream& operator<< (std::ostream& os,const ByteOffset64& offs);
inline std::ostream& operator<< (std::ostream& os,const WordOffset64& offs);
inline std::ostream& operator<< (std::ostream& os,const DwordOffset64& offs);
inline std::ostream& operator<< (std::ostream& os,const QwordOffset64& offs);
inline std::ostream& operator<< (std::ostream& os,const ByteOffset32& offs);
inline std::ostream& operator<< (std::ostream& os,const WordOffset32& offs);
inline std::ostream& operator<< (std::ostream& os,const DwordOffset32& offs);
inline std::ostream& operator<< (std::ostream& os,const QwordOffset32& offs);

std::ostream& operator<< (std::ostream& os,const FarPtr16& ptr);
std::ostream& operator<< (std::ostream& os,const FarPtr32& ptr);


std::ostream&
print_code(std::ostream& os,
	   const_code_pointer_type start,
	   const_code_pointer_type end);

// Just in case..
FILE*
print_code(FILE* os,
	   const_code_pointer_type start,
	   const_code_pointer_type end);


///////////////////////////////////////////////////////////////////////////
//           Implementations                                             //
///////////////////////////////////////////////////////////////////////////


inline
std::ostream& operator<< (std::ostream& os,const RegRIP& r)
{ return os << "rip" ; }


namespace impl {
template<class X,int max_bound,const char* const name[]>
inline
std::ostream&
print (std::ostream& os, const TypedRegisterBase<X,max_bound,name>& r)
{  return os << r.get_name(); }

}

inline std::ostream& operator<< (std::ostream& os,const ByteReg86& r)
{ return impl::print(os,r) ; }
inline std::ostream& operator<< (std::ostream& os,const ByteRegRex& r)
{ return impl::print(os,r) ; }
inline std::ostream& operator<< (std::ostream& os,const WordReg& r)
{ return impl::print(os,r) ; }
inline std::ostream& operator<< (std::ostream& os,const DwordReg& r)
{ return impl::print(os,r) ; }
inline std::ostream& operator<< (std::ostream& os,const QwordReg& r)
{ return impl::print(os,r) ; }
inline std::ostream& operator<< (std::ostream& os,const MmReg& r)
{ return impl::print(os,r) ; }
inline std::ostream& operator<< (std::ostream& os,const XmmReg& r)
{ return impl::print(os,r) ; }
inline std::ostream& operator<< (std::ostream& os,const StReg& r)
{ return impl::print(os,r) ; }
inline std::ostream& operator<< (std::ostream& os,const SegReg& r)
{ return impl::print(os,r) ; }
inline std::ostream& operator<< (std::ostream& os,const CrReg& r)
{ return impl::print(os,r) ; }
inline std::ostream& operator<< (std::ostream& os,const DrReg& r)
{ return impl::print(os,r) ; }


template<typename X> const char * type_name();

template<> inline const char * type_name<byte_t>()
{ return "BYTE" ; }
template<> inline const char * type_name<word_t>()
{ return "WORD" ; }
template<> inline const char * type_name<dword_t>()
{ return "DWORD" ; }
template<> inline const char * type_name<qword_t>()
{ return "QWORD" ; }
template<> inline const char * type_name<mmword_t>()
{ return "MMWORD" ; }
template<> inline const char * type_name<xmmword_t>()
{ return "XMMWORD" ; }
template<> inline const char * type_name<fword_t>()
{ return "FWORD" ; }
template<> inline const char * type_name<oword_t>()
{ return "OWORD" ; }
template<> inline const char * type_name<tbyte_t>()
{ return "TBYTE" ; }
template<> inline const char * type_name<real4_t>()
{ return "REAL4" ; }
template<> inline const char * type_name<real8_t>()
{ return "REAL8" ; }
template<> inline const char * type_name<real10_t>()
{ return "REAL10" ; }
template<> inline const char * type_name<farptr16_t>()
{ return "FAR16" ; }
template<> inline const char * type_name<farptr32_t>()
{ return "FAR32" ; }
template<> inline const char * type_name<farptr64_t>()
{ return "FAR64" ; }


namespace impl {

std::ostream&  print (std::ostream& os,const PtrBase& r);

template<class X>
inline std::ostream& print(std::ostream& os,const Ptr<X>& r)
{  
   os << type_name<X>() << " PTR" ;
   impl::print(os,r.get_base());
   return os;
}

}

inline std::ostream& operator<< (std::ostream& os,const VoidPtr& r)
{  return impl::print(os, r.get_base()) ; }

inline std::ostream& operator<< (std::ostream& os,const BytePtr& ptr)
{ return impl::print(os,ptr) ; }
inline std::ostream& operator<< (std::ostream& os,const BytePtr86& ptr)
{ return impl::print(os,ptr) ; }
inline std::ostream& operator<< (std::ostream& os,const BytePtrRex& ptr)
{ return impl::print(os,ptr) ; }
inline std::ostream& operator<< (std::ostream& os,const WordPtr& ptr)
{ return impl::print(os,ptr) ; }
inline std::ostream& operator<< (std::ostream& os,const DwordPtr& ptr)
{ return impl::print(os,ptr) ; }
inline std::ostream& operator<< (std::ostream& os,const QwordPtr& ptr)
{ return impl::print(os,ptr) ; }
inline std::ostream& operator<< (std::ostream& os,const MmWordPtr& ptr)
{ return impl::print(os,ptr) ; }
inline std::ostream& operator<< (std::ostream& os,const XmmWordPtr& ptr)
{ return impl::print(os,ptr) ; }
inline std::ostream& operator<< (std::ostream& os,const FwordPtr& ptr)
{ return impl::print(os,ptr) ; }
inline std::ostream& operator<< (std::ostream& os,const TbytePtr& ptr)
{ return impl::print(os,ptr) ; }
inline std::ostream& operator<< (std::ostream& os,const OwordPtr& ptr)
{ return impl::print(os,ptr) ; }
inline std::ostream& operator<< (std::ostream& os,const Real4Ptr& ptr)
{ return impl::print(os,ptr) ; }
inline std::ostream& operator<< (std::ostream& os,const Real8Ptr& ptr)
{ return impl::print(os,ptr) ; }
inline std::ostream& operator<< (std::ostream& os,const Far16Ptr& ptr)
{ return impl::print(os,ptr) ; }
inline std::ostream& operator<< (std::ostream& os,const Far32Ptr& ptr)
{ return impl::print(os,ptr) ; }
inline std::ostream& operator<< (std::ostream& os,const Far64Ptr& ptr)
{ return impl::print(os,ptr) ; }



namespace impl {
template<class OffsetType,class ValType,bool no_segment>
inline
std::ostream & print(std::ostream& os,
	       const MOffset<OffsetType,ValType,no_segment>& m)
{
   using namespace std;
   if (! no_segment) {
      Segment seg = m.get_segment();
      if (seg == DefaultSegment) 
	 seg = Segment_DS;
      os << seg << ':';
   }
   os << showbase << hex << m.get_offset();
   return os;
}

}

inline std::ostream& operator<< (std::ostream& os,const ByteOffset64& offs)
{ return impl::print(os,offs) ; }
inline std::ostream& operator<< (std::ostream& os,const WordOffset64& offs)
{ return impl::print(os,offs) ; }
inline std::ostream& operator<< (std::ostream& os,const DwordOffset64& offs)
{ return impl::print(os,offs) ; }
inline std::ostream& operator<< (std::ostream& os,const QwordOffset64& offs)
{ return impl::print(os,offs) ; }
inline std::ostream& operator<< (std::ostream& os,const ByteOffset32& offs)
{ return impl::print(os,offs) ; }
inline std::ostream& operator<< (std::ostream& os,const WordOffset32& offs)
{ return impl::print(os,offs) ; }
inline std::ostream& operator<< (std::ostream& os,const DwordOffset32& offs)
{ return impl::print(os,offs) ; }
inline std::ostream& operator<< (std::ostream& os,const QwordOffset32& offs)
{ return impl::print(os,offs) ; }

inline std::ostream& operator<<(std::ostream& os,Segment seg)
{
   switch (seg) {
   case Segment_CS: return os << "cs";
   case Segment_SS: return os << "ss";
   case Segment_DS: return os << "ds";
   case Segment_ES: return os << "es";
   case Segment_FS: return os << "fs";
   case Segment_GS: return os << "gs";
   default:
      return os;
   }
}


template<class X>
struct FormatInt
{
   FormatInt(const X& x) : x(x) { }
   const X& x;
};

template<class X>
inline std::ostream & operator<< (std::ostream& os,const FormatInt<X>& x)
{ return os << x.x; }

template<>
inline std::ostream & operator<< (std::ostream& os,const FormatInt<uint8_t>& x)
{  return os << std::showbase << std::hex << (int) x.x;}

template<>
inline std::ostream & operator<< (std::ostream& os,const FormatInt<uint16_t>& x)
{  return os << std::showbase << std::hex << (int) x.x; }

template<>
inline std::ostream & operator<< (std::ostream& os,const FormatInt<uint32_t>& x)
{  return os << std::showbase << std::hex << (int) x.x; }

template<>
inline std::ostream & operator<< (std::ostream& os,const FormatInt<int8_t>& x)
{  return os << std::showbase << std::hex << (int) x.x;}

template<>
inline std::ostream & operator<< (std::ostream& os,const FormatInt<int16_t>& x)
{  return os << std::showbase << std::hex << (int) x.x; }

template<>
inline std::ostream & operator<< (std::ostream& os,const FormatInt<int32_t>& x)
{  return os << std::showbase << std::hex << (int) x.x; }

template<class X> inline FormatInt<X> format(const X& x)
{ return FormatInt<X>(x) ; }



template<class Insn>
inline
std::ostream&
print_instruction(std::ostream& os,
		  const Insn& insn)
{
   os << Insn::mnemonic;
   return os;
}

template<class Insn,class Op>
inline
std::ostream& 
print_instruction(std::ostream& os,
		  const Insn& insn,
		  const Op  & op1)
{
   os << Insn::mnemonic;
   os << " " << format(op1);
   return os;
}

template<class Insn,class Op1,class Op2>
inline
std::ostream& 
print_instruction(std::ostream& os,
		  const Insn& insn,
		  const Op1  & op1,
		  const Op2  & op2)
{
   os << Insn::mnemonic;
   os << " " << format(op1);
   os << "," << format(op2);
   return os;
}

template<class Insn,class Op1,class Op2,class Op3>
inline
std::ostream& 
print_instruction(std::ostream& os,
		  const Insn& insn,
		  const Op1  & op1,
		  const Op2  & op2,
		  const Op3  & op3)
{
   os << Insn::mnemonic ;
   os << " " << format(op1) ;
   os << "," << format(op2) ;
   os << "," << format(op3) ;
   return os;
}


}}


#endif // MY_ERASM_X64_IO_HPP
