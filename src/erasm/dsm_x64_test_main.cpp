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
#include "erasm/dsm_x64.hpp"
#include "erasm/x64_io.hpp"
#include "erasm/x64_assembler_test_code_64_32.hpp"


#include <cstring>
#include <stdlib.h>
#include <iostream>
#include <iomanip>
#include <stdio.h>


using namespace std;
using namespace erasm::x64;


template<class X>
struct Id
{
   Id(const X& x) : x(x) {}
   const X& x;
};

template<class X>
struct FormattedInt
{
   FormattedInt(const X& x) : x(x) {}
   const X& x;
};


namespace erasm { namespace prelude {

DEFINE_FOREIGN_DATA1(Id,class);
DEFINE_FOREIGN_DATA1(FormattedInt,class);

template<class X>
struct isBuiltinIntegerType_;

template<class X>
struct eval<isBuiltinIntegerType_<X> >
{
   typedef False result;
};

template<>
struct eval<isBuiltinIntegerType_<int8_t> >
{
   typedef True result;
};

template<>
struct eval<isBuiltinIntegerType_<uint8_t> >
{
   typedef True result;
};

template<>
struct eval<isBuiltinIntegerType_<int16_t> >
{
   typedef True result;
};

template<>
struct eval<isBuiltinIntegerType_<uint16_t> >
{
   typedef True result;
};

template<>
struct eval<isBuiltinIntegerType_<int32_t> >
{
   typedef True result;
};

template<>
struct eval<isBuiltinIntegerType_<uint32_t> >
{
   typedef True result;
};

template<>
struct eval<isBuiltinIntegerType_<int64_t> >
{
   typedef True result;
};

template<>
struct eval<isBuiltinIntegerType_<uint64_t> >
{
   typedef True result;
};

template<class X>
struct isBuiltinIntegerType;
template<class X>
struct eval<isBuiltinIntegerType<X> >
{
   //   typedef typename eval<X>::result X_;
   typedef typename eval<isBuiltinIntegerType_<X> > ::result result;
};

ERASM_META_ASSERT_EQUAL((isBuiltinIntegerType<int>),(True));
ERASM_META_ASSERT_EQUAL((isBuiltinIntegerType<Zero>),(False));

}}


template<class X> inline ostream& operator<< (ostream& os,Id<X> x)
{ return os << x.x; }

template<class X> inline ostream& operator<< (ostream& os,FormattedInt<X> x)
{ return os << showbase << hex << (int)x.x; }


using namespace erasm::prelude;
template<class X>
struct FormattedOp
   : eval<if_<isBuiltinIntegerType<X> , 
	      FormattedInt<X> , 
	      Id<X> > > :: result
{
   typedef typename  eval<if_<isBuiltinIntegerType<X> , 
			      FormattedInt<X> , 
			      Id<X> > > :: result   base;
   FormattedOp(const X& x) 
      : base(x)
      {}
   const base & get_base() const
      { return *this ; }
};

template<class X> inline ostream& operator<< (ostream& os,FormattedOp<X> x)
{  return os << x.get_base(); }


template<class X> inline FormattedOp<X> format(const X& x)
{ return FormattedOp<X>(x); }



template<class Insn>
inline 
ostream& 
print_instruction(ostream& os,
		  const Insn& insn)
{
   print_code(os,insn.start,insn.end);
   os << Insn::mnemonic << endl;   
   return os;
}

template<class Insn,class Op>
inline
ostream& 
print_instruction(ostream& os,
		  const Insn& insn,
		  const Op  & op1)
{
   print_code(os,insn.start,insn.end);
   os << Insn::mnemonic
      << " " << format(op1) << endl;
   return os;
}

template<class Insn,class Op1,class Op2>
inline
ostream& 
print_instruction(ostream& os,
		  const Insn& insn,
		  const Op1  & op1,
		  const Op2  & op2)
{
   print_code(os,insn.start,insn.end);
   os << Insn::mnemonic
      << " " << format(op1)
      << "," << format(op2) << endl;
   return os;
}

template<class Insn,class Op1,class Op2,class Op3>
inline
ostream& 
print_instruction(ostream& os,
		  const Insn& insn,
		  const Op1  & op1,
		  const Op2  & op2,
		  const Op3  & op3)
{
   print_code(os,insn.start,insn.end);
   os << Insn::mnemonic 
      << " " << format(op1) 
      << "," << format(op2) 
      << "," << format(op3) << endl;
   return os;
}




template<class Stream>
struct Disasm
{
   const_code_ptr start;
   const_code_ptr end;
   Stream& os;

   Disasm(const_code_ptr start,const_code_ptr end,Stream& os) 
      : start(start),end(end),os(os) 
      {}
   
   action_result_type cont(const InstructionData& params)
      {
	 return make_pair(params.end,
			  params.end < end ? 
			  ACTION_CONTINUE : ACTION_FINISH );
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

byte_t buff[1024 * 1024 ];

int main()
{
   code_ptr p = buff;
   p += erasm::x64::addr64::data32::gen_manual_test(p);
   p += erasm::x64::addr64::data32::gen_auto_test(p);

   int len = p-buff;

   typedef Disasm<ostream> dsm_type;

   dsm_type   dsm(buff,buff+len  ,cout);
   decode_instruction<dsm_type,true,true>(buff,dsm);

   return 0;
}

