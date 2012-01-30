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
#include <erasm/x64_io.hpp>
#include <erasm/faststream.hpp>

#include <cstring>
#include <stdlib.h>
#include <sstream>
#include <string>

namespace erasm {  namespace x64 {

using namespace std;

ostream& operator<< (ostream& os,const FarPtr16& ptr)
{
   os << showbase << hex << ptr.selector <<':'  
      << showbase << hex << ptr.offset  ;
   return os;
}


ostream& operator<< (ostream& os,const FarPtr32& ptr)
{
   os << showbase << hex << ptr.selector <<':'  
      << showbase << hex << ptr.offset  ;
   return os;
}


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
inline
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
   bool rex_b = p.rex_b_;
   bool rex_x = p.rex_x_;

   switch(mod) {
   case 0:
      switch(rm) {
      case 4:
	 switch(sib_base) {
	 case 5:
 	    if (! rex_x && sib_index == 4) {
	       return a.action(disp32);
	    } else {
	       return a.action(scaledIndex(sib_ss,
					   DwordReg(sib_index,rex_x)),
			       disp32);
	    }
	 default:
	    if (! rex_x && sib_index == 4) {
	       return a.action(DwordReg(sib_base,rex_b));
	    } else {
	       return a.action(DwordReg(sib_base,rex_b),
			       scaledIndex(sib_ss,
					   DwordReg(sib_index,rex_x)));
	    }
	 }
      case 5:
	 return a.action(disp32);
      default:
	 return a.action(DwordReg(rm,rex_b));
      }
   case 1:
      switch(rm) {
      case 4:
	 if (! rex_x && sib_index == 4) {
	    return a.action(DwordReg(sib_base,rex_b),
			    disp8);
	 } else {
	    return a.action(DwordReg(sib_base,rex_b),
			    scaledIndex(sib_ss,DwordReg(sib_index,rex_x)),
			    disp8);
	 }
      default:
	 return a.action(DwordReg(rm,rex_b),disp8);
      }

   case 2:
      switch(rm) {
      case 4:
	 if (! rex_x && sib_index == 4) {
	    return a.action(DwordReg(sib_base,rex_b),disp32);
	 } else {
	    return a.action(DwordReg(sib_base,rex_b),
			    scaledIndex(sib_ss,DwordReg(sib_index,rex_x)),
			    disp32);
	 }
      default:
	 return a.action(DwordReg(rm,rex_b),disp32);
      }
   }
}


template<class Return,class Action>
Return
decode_ptr64(const PtrBase& p, Action& a )
{
   const uint8_t & mod = p.mod_;
   const uint8_t & rm  = p.rm_;
   const uint8_t & sib_base  = p.sib_base_;
   const uint8_t & sib_index = p.sib_index_;
   const uint8_t & sib_ss    = p.sib_ss_;
   const disp8_type  & disp8  =  p.disp8_;
   const disp32_type & disp32 =  p.disp32_;
   bool rex_b = p.rex_b_;
   bool rex_x = p.rex_x_;


   switch(mod) {
   case 0:
      switch(rm) {
      case 4:
	 switch(sib_base) {
	 case 5:
 	    if (! rex_x && sib_index == 4) {
	       return a.action(disp32);
	    } else {
	       return a.action(scaledIndex(sib_ss,
					   QwordReg(sib_index,rex_x)),
			       disp32);
	    }
	 default:
	    if (! rex_x && sib_index == 4) {
	       return a.action(QwordReg(sib_base,rex_b));
	    } else {
	       return a.action(QwordReg(sib_base,rex_b),
			       scaledIndex(sib_ss,
					   QwordReg(sib_index,rex_x)));
	    }
	 }
      case 5:
	 return a.action(rip,disp32);
      default:
	 return a.action(QwordReg(rm,rex_b));
      }
   case 1:
      switch(rm) {
      case 4:
	 if (! rex_x && sib_index == 4) {
	    return a.action(QwordReg(sib_base,rex_b),
			    disp8);
	 } else {
	    return a.action(QwordReg(sib_base,rex_b),
			    scaledIndex(sib_ss,QwordReg(sib_index,rex_x)),
			    disp8);
	 }
      default:
	 return a.action(QwordReg(rm,rex_b),disp8);
      }

   case 2:
      switch(rm) {
      case 4:
	 if (! rex_x && sib_index == 4) {
	    return a.action(QwordReg(sib_base,rex_b),disp32);
	 } else {
	    return a.action(QwordReg(sib_base,rex_b),
			    scaledIndex(sib_ss,QwordReg(sib_index,rex_x)),
			    disp32);
	 }
      default:
	 return a.action(QwordReg(rm,rex_b),disp32);
      }
   }
}


template<class Return,class Action>
inline
Return
decode_ptr(const PtrBase& p, Action& action )
{
   if (p.is_addr32()) {
      return decode_ptr32<Return>(p,action);
   } else {
      return decode_ptr64<Return>(p,action);
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
   decode_ptr<void,PrintPtr&>(r,printer);

   os << "]" ;
   return os;
};
}

static inline
char hexchar(int x)
{
   if (x < 0xa) {
      return '0' + x ;
   } else {
      return 'a' + (x - 0xa);
   }
}

static inline
void
print_opcode(ostream& os,
	     const_code_pointer_type start,
	     const_code_pointer_type end)
{ 
   static char buf[200];

   int len = end - start;
   int counter=0;
   const int width = 20;

   for (int i=0;i<len;i++) {
      unsigned char x = start[i];
      buf[counter]   = hexchar(x/16);
      buf[counter+1] = hexchar(x%16);
      counter += 2;
   }

   if (counter < width) {
      memset(&buf[counter],' ',width - counter);
      counter = 20;
   }

   buf[counter]=0;

   os << buf;
}

// ostream&
// print_code(ostream& os,
// 	   const_code_pointer_type start,
// 	   const_code_pointer_type end)
// {
//    const int width=20;
//    int len = end-start;
//    for (int i=0;i<len;i++) {
//       os << setfill('0') << setw(2) << hex << noshowbase 
// 	 << (unsigned int) start[i];
//    }
//    for (int i=len*2;i<width;i++) {
//       os << " ";
//    }
//    os << setfill('0') << setw(8) << hex  << (unsigned int) start << " " 
//       << setfill(' ') << setw(2) << dec  << len << " ";
   
//    return os;
// }


static inline
void print_hex(ostream& os,unsigned int x)
{
   char buf[20];

   int len = 8;
   int counter=0;
   unsigned char* start = (unsigned char*) &x;

   for (int i=0;i<len;i++) {
      unsigned char x = start[i];
      buf[counter]   = hexchar(x/16);
      buf[counter+1] = hexchar(x%16);
      counter += 2;
   }
   buf[counter] = 0;

   os << buf;
}

ostream&
print_code(ostream& os,
	   const_code_pointer_type start,
	   const_code_pointer_type end)
{
   int len = end - start;
   print_opcode(os,start,end);
   os << noshowbase << setfill('0') << setw(8) << hex  << (unsigned int) start ;
   os << " "  << setfill(' ') << setw(2) << dec  << len << " ";
   return os;
}


FILE*
print_code(FILE* os,
	   const_code_pointer_type start,
	   const_code_pointer_type end)
{
   using namespace std;
   ::erasm::ofaststream<500> s(os);
   print_code(s,start,end);
   return os;
}


}}

