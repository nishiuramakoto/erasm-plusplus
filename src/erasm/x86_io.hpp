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
#ifndef MY_ERASM_X86_IO_HPP
#define MY_ERASM_X86_IO_HPP

#include "erasm/x86.hpp"
#include "erasm/x64_io.hpp"
#include <iostream>
#include <iomanip>
#include <stdio.h>

namespace erasm {  namespace x86 {

///////////////////////////////////////////////////////////////////////////
//           Forward declarations                                        //
///////////////////////////////////////////////////////////////////////////

inline std::ostream& operator<< (std::ostream& os,const VoidPtr& ptr);
inline std::ostream& operator<< (std::ostream& os,const BytePtr& ptr);
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

inline std::ostream& operator<< (std::ostream& os,const ByteOffset16& offs);
inline std::ostream& operator<< (std::ostream& os,const WordOffset16& offs);
inline std::ostream& operator<< (std::ostream& os,const DwordOffset16& offs);
inline std::ostream& operator<< (std::ostream& os,const QwordOffset16& offs);
inline std::ostream& operator<< (std::ostream& os,const ByteOffset32& offs);
inline std::ostream& operator<< (std::ostream& os,const WordOffset32& offs);
inline std::ostream& operator<< (std::ostream& os,const DwordOffset32& offs);
inline std::ostream& operator<< (std::ostream& os,const QwordOffset32& offs);

using x64::print_code;
using x64::type_name;


///////////////////////////////////////////////////////////////////////////
//           Implementations                                             //
///////////////////////////////////////////////////////////////////////////


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

inline std::ostream& operator<< (std::ostream& os,const ByteOffset16& offs)
{ return impl::print(os,offs) ; }
inline std::ostream& operator<< (std::ostream& os,const WordOffset16& offs)
{ return impl::print(os,offs) ; }
inline std::ostream& operator<< (std::ostream& os,const DwordOffset16& offs)
{ return impl::print(os,offs) ; }
inline std::ostream& operator<< (std::ostream& os,const QwordOffset16& offs)
{ return impl::print(os,offs) ; }
inline std::ostream& operator<< (std::ostream& os,const ByteOffset32& offs)
{ return impl::print(os,offs) ; }
inline std::ostream& operator<< (std::ostream& os,const WordOffset32& offs)
{ return impl::print(os,offs) ; }
inline std::ostream& operator<< (std::ostream& os,const DwordOffset32& offs)
{ return impl::print(os,offs) ; }
inline std::ostream& operator<< (std::ostream& os,const QwordOffset32& offs)
{ return impl::print(os,offs) ; }



}}

#endif // MY_ERASM_X86_IO_HPP
