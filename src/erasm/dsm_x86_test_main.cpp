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
#include "meta_prelude.hpp"
#include "erasm/dsm_x86.hpp"
#include "erasm/x86_io.hpp"
#include "erasm/x86_assembler_test_code_32_32.hpp"
#include "erasm/x86_addr32_data32.hpp"
#include "gnu_disassembler.hpp"
#include <boost/timer.hpp>

#include <sstream>
#include <string>
#include <cstring>
#include <stdlib.h>
#include <iostream>
#include <iomanip>
#include <stdio.h>

#include "mystream.hpp"

using std::ostream;
using std::hex;
using std::showbase;
using std::cout;
using std::cerr;
using std::make_pair;
using std::string;
using std::ostringstream;


using namespace erasm::x86;
const char endl = '\n';

inline ostream& operator<< (ostream& os,const FarPtr16& ptr)
{
   os << showbase << hex << ptr.selector <<':'  
      << showbase << hex << ptr.offset  ;
   return os;
}

inline ostream& operator<< (ostream& os,const FarPtr32& ptr)
{
   os << showbase << hex << ptr.selector <<':'  
      << showbase << hex << ptr.offset  ;
   return os;
}



template<class X>
struct Formatted
{
   Formatted(const X& x) : x(x) { }
   const X& x;
};

template<class X>
inline ostream & operator<< (ostream& os,const Formatted<X>& x)
{ return os << x.x; }

template<>
inline ostream & operator<< (ostream& os,const Formatted<uint8_t>& x)
{  return os << showbase << hex << (int) x.x;}

template<>
inline ostream & operator<< (ostream& os,const Formatted<uint16_t>& x)
{  return os << showbase << hex << (int) x.x; }

template<>
inline ostream & operator<< (ostream& os,const Formatted<uint32_t>& x)
{  return os << showbase << hex << (int) x.x; }

template<>
inline ostream & operator<< (ostream& os,const Formatted<int8_t>& x)
{  return os << showbase << hex << (int) x.x;}

template<>
inline ostream & operator<< (ostream& os,const Formatted<int16_t>& x)
{  return os << showbase << hex << (int) x.x; }

template<>
inline ostream & operator<< (ostream& os,const Formatted<int32_t>& x)
{  return os << showbase << hex << (int) x.x; }

template<class X> inline Formatted<X> format(const X& x)
{ return Formatted<X>(x) ; }


template<class Insn>
ostream&
print_instruction(ostream& os,
		  const Insn& insn)
{
   print_code(os,insn.start,insn.end);
   os << Insn::mnemonic << endl;
   return os;
}

template<class Insn,class Op>
ostream& 
print_instruction(ostream& os,
		  const Insn& insn,
		  const Op  & op1)
{
   print_code(os,insn.start,insn.end);
   os << Insn::mnemonic;
   os << " " << format(op1) << endl;
   return os;
}

template<class Insn,class Op1,class Op2>
ostream& 
print_instruction(ostream& os,
		  const Insn& insn,
		  const Op1  & op1,
		  const Op2  & op2)
{
   print_code(os,insn.start,insn.end);
   os << Insn::mnemonic;
   os << " " << format(op1);
   os << "," << format(op2) << endl;
   return os;
}

template<class Insn,class Op1,class Op2,class Op3>
ostream& 
print_instruction(ostream& os,
		  const Insn& insn,
		  const Op1  & op1,
		  const Op2  & op2,
		  const Op3  & op3)
{
   print_code(os,insn.start,insn.end);
   os << Insn::mnemonic ;
   os << " " << format(op1) ;
   os << "," << format(op2) ;
   os << "," << format(op3) << endl;
   return os;
}


struct Disasm
{
   const_code_ptr start;
   const_code_ptr end;
   ostream& os;
   int count;

   Disasm(const_code_ptr start,const_code_ptr end,ostream& os) 
      : start(start),end(end),os(os) ,count(0)
      {
      }
   
   action_result_type finish(const InstructionData& params)
      {
	 return make_pair(params.end, ACTION_FINISH);
      }

   action_result_type cont(const InstructionData& params)
      {
	 count++;
	 return make_pair(params.end, ACTION_CONTINUE);
      }

   action_result_type check(const InstructionData& params)
      {
	 if (params.end >= end) {
	    return finish(params);
	 }
	 return cont(params);
      }

   action_result_type
   action(const Ret& insn)
      {
	 if (insn.end >= end) {
	    return finish(insn);
	 }
	 print_instruction(os,insn);
	 return cont(insn);
      }

   template<class Insn>
   action_result_type
   action(const Insn& insn)
      {
	 print_instruction(os,insn);
	 return cont(insn.data());
      }

   template<class Insn,class Op1>
   action_result_type
   action(const Insn& insn,
	  const Op1& op1)
      {
	 print_instruction(os,insn,op1);
	 return cont(insn.data());
      }

   template<class Insn,class Op1,class Op2>
   action_result_type
   action(const Insn& insn,
	  const Op1& op1,
	  const Op2& op2)
      {
	 print_instruction(os,insn,op1,op2);
	 return cont(insn.data());
      }

   template<class Insn,class Op1,class Op2,class Op3>
   action_result_type
   action(const Insn& insn,
	  const Op1& op1,
	  const Op2& op2,
	  const Op3& op3)
      {
	 print_instruction(os,insn,op1,op2,op3);
	 return cont(insn.data());
      }
   
   const_code_ptr
   error(const InstructionData& data)
      {
	 print_code(os,data.start,data.start+5);
	 os << "decode error:"   << data.action_code << endl;
	 os << "decoded length:" << data.start - start << endl;
	 return data.start;
      }

};


byte_t buff[1024 * 1024 * 10 ];

int gen_code(int n)
{
   using namespace erasm::x86::addr32::data32;
   code_ptr p = buff;
   for (int i=0; i<n;i++) {
      p += gen_manual_test(p);
      p += gen_auto_test(p);      
   }
   int len = p - buff;

   p += ret(p);
   return len;
}

#ifdef TEST_GDSM
int dsm(code_ptr beg,code_ptr end )
{
   cerr << "testing gdsm" << endl;
   using namespace gnu_dsm;
   Disassembler dsm(false,false,"intel,i386,addr32,data32");
   int count = dsm.print(beg,end);
   return count;
}
#else
my::omystream<526 -1>  myout(stdout);
//my::nullstream  myout;
int dsm(code_ptr beg,code_ptr end )
{
   cerr << "testing mydsm" << endl;
   typedef Disasm dsm_type;
   dsm_type   mydsm(beg,end  , myout);
   decode_instruction<dsm_type,true,true>(buff,mydsm);
   return mydsm.count;
}
#endif


int main(int argc,char**argv)
{
#ifdef PROFILING
   int n = 1000;
#else
   int n = 1;
#endif

   {
      int len = gen_code(n);


      boost::timer t0;

      int count = dsm(buff,buff+len);

      float elapsed = t0.elapsed();
      cerr << "elapsed time = " << elapsed << " sec" << endl;
      cerr << "number of bytes:" << len << endl;
      cerr << "number of instructions:" << count << endl;
   }


   return 0;
}

