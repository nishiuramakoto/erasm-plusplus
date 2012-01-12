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
#include "erasm/x86_io.hpp"

#include <cstring>
#include <stdlib.h>

namespace erasm {  namespace x86 {
using namespace std;

template<class X>
inline const X& format(const X& x)
{ return x; }

inline int format(imm8_t x)
{ return x; }


struct PrintPtr
{
   ostream& os_;
   PrintPtr(ostream& os)
      : os_(os) 
      {}

   template<class X>
   void action(const X& x) 
      {
	 os_ << showbase << hex << format(x);
      }

   template<class X,class Y>
   void action(const X& x,const Y& y) 
      {
	 os_ << x << '+' << showbase << hex << format(y);
      }

   template<class X,class Y,class Z>
   void action(const X& x,const Y& y,const Z& z) 
      {
	 os_ << x << '+' << y << '+' << showbase << hex << format(z);
      }

};

template<class X>
struct ScaledIndex
{
   ScaledIndex(int ss,const X& index)
      : ss(ss) , index(index)
      {}
   int ss;
   const X & index;
};

template<class X> inline ScaledIndex<X> scaledIndex(int ss,const X&index)
{ return ScaledIndex<X>(ss,index);}

template<class X>
std::ostream& operator<< (std::ostream& os,const ScaledIndex<X>& r);


template<class X>
inline
ostream& operator<< (ostream& os,const ScaledIndex<X>& r)
{
   assert(0 <= r.ss && r.ss <4);
   switch(r.ss) {
   case 0: return os << r.index;
   case 1: return os << r.index << "*2";
   case 2: return os << r.index << "*4";
   default: return os << r.index << "*8";
   }
};


template<class Return,class Action>
Return
decode_ptr16(const PtrData& p, Action& a )
{
   const uint8_t & mod = p.mod_;
   const uint8_t & rm  = p.rm_;
   const disp8_type  & disp8  =  p.disp8_;
   const disp16_type & disp16 =  p.disp16_;


   switch(mod){
   case 0:
      switch(rm) {
      case 0:  return a.action(bx,si);
      case 1:  return a.action(bx,di);
      case 2:  return a.action(bp,si);
      case 3:  return a.action(bp,di);
      case 4:  return a.action(si);
      case 5:  return a.action(di);
      case 6:  return a.action(disp16);
      case 7:  return a.action(bx);
      }
   case 1:
      switch(rm) {
      case 0:  return a.action(bx,si,disp8);
      case 1:  return a.action(bx,di,disp8);
      case 2:  return a.action(bp,si,disp8);
      case 3:  return a.action(bp,di,disp8);
      case 4:  return a.action(si,disp8);
      case 5:  return a.action(di,disp8);
      case 6:  return a.action(bp,disp8);
      case 7:  return a.action(bx,disp8);
      }
   case 2:
      switch(rm) {
      case 0:  return a.action(bx,si,disp16);
      case 1:  return a.action(bx,di,disp16);
      case 2:  return a.action(bp,si,disp16);
      case 3:  return a.action(bp,di,disp16);
      case 4:  return a.action(si,disp16);
      case 5:  return a.action(di,disp16);
      case 6:  return a.action(bp,disp16);
      case 7:  return a.action(bx,disp16);
      }
   }
}



template<class Return,class Action>
Return
decode_ptr32(const PtrData& p, Action& a )
{
   const uint8_t & mod = p.mod_;
   const uint8_t & rm  = p.rm_;
   const uint8_t & sib_base  = p.sib_base_;
   const uint8_t & sib_index = p.sib_index_;
   const uint8_t & sib_ss    = p.sib_ss_;
   const disp8_type  & disp8  =  p.disp8_;
   const disp32_type & disp32 =  p.disp32_;

   switch(mod){
   case 0:
      switch(rm) {
      case 4:
	 switch(sib_base) {
	 case 5:
	    switch(sib_index) {
	    case 4:
	       return a.action(disp32);
	    default:
	       return a.action(scaledIndex(sib_ss,DwordReg(sib_index)),
			       disp32);
	    }
	 default:
	    switch(sib_index) {
	    case 4:
	       return a.action(DwordReg(sib_base));
	    default:
	       return a.action(DwordReg(sib_base),
			       scaledIndex(sib_ss,DwordReg(sib_index)));
	    }
	 }
      case 5:
	 return a.action(disp32);
      default:
	 return a.action(DwordReg(rm));
      }
   case 1:
      switch(rm) {
      case 4:
	 switch (sib_index) {
	 case 4:
	    return a.action(DwordReg(sib_base),
			    disp8);
	 default:
	    return a.action(DwordReg(sib_base),
			    scaledIndex(sib_ss,DwordReg(sib_index)),
			    disp8);
	 }
      default:
	 return a.action(DwordReg(rm),
			 disp8);
      }

   case 2:
      switch(rm) {
      case 4:
	 switch (sib_index) {
	 case 4:
	    return a.action(DwordReg(sib_base),
			    disp32);
	 default:
	    return a.action(DwordReg(sib_base),
			    scaledIndex(sib_ss,DwordReg(sib_index)),
			    disp32);
	 }
      default:
	 return a.action(DwordReg(rm),
			 disp32);
      }
   }
}


template<class Return,class Action>
inline Return decode_ptr(const PtrBase& p, Action& action )
{
   if (p.is_addr16()) {
      return x86::decode_ptr16<Return>(p,action);
   } else {
      return x86::decode_ptr32<Return>(p,action);
   }
}


namespace impl {
ostream& print (ostream& os,const PtrBase& r)
{
   Segment segment = r.get_segment();
   if (segment != DefaultSegment) {
      os << " " << segment << ':';
   }   
   os << "[";
   PrintPtr printer(os);
   x86::decode_ptr<void,x86::PrintPtr&>(r,printer);

   os << "]" ;
   return os;
}
}


}}
