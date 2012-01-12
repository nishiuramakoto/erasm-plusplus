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


using std::ostream;
using std::hex;
using std::showbase;
using std::cout;
using std::endl;
using std::cerr;
using std::make_pair;
using std::string;
using std::ostringstream;

using namespace erasm::x86;

struct Counter
{
   const_code_ptr start;
   const_code_ptr end;
   int counter;

   Counter(const_code_ptr start,const_code_ptr end) 
      : start(start),end(end),counter(0)
      {}
   
   action_result_type check(const InstructionData& params) const
      {
	 return make_pair(params.end,
			  params.end < end ? 
			  ACTION_CONTINUE : ACTION_FINISH );
      }

   action_result_type cont(const InstructionData& params) const
      {
	 return make_pair(params.end,ACTION_CONTINUE);
      }

   action_result_type
   action(const Ret& insn)
      {
	 counter ++;
	 return check(insn);
      }

   action_result_type
   action(const Jmp& insn)
      {
	 counter ++;
	 return check(insn);
      }


   template<class Insn>
   action_result_type
   action(const Insn& insn)
      {
	 counter ++;
	 return cont(insn);
      }

   template<class Insn,class Op1>
   action_result_type
   action(const Insn& insn,
	  const Op1& op1)
      {
	 counter ++;
	 return cont(insn);
      }

   template<class Insn,class Op1,class Op2>
   action_result_type
   action(const Insn& insn,
	  const Op1& op1,
	  const Op2& op2)
      {
	 counter ++;
	 return cont(insn);
      }

   template<class Insn,class Op1,class Op2,class Op3>
   action_result_type
   action(const Insn& insn,
	  const Op1& op1,
	  const Op2& op2,
	  const Op3& op3)
      {
	 counter ++;
	 return cont(insn);
      }
   
   const_code_ptr
   error(const InstructionData& params)
      {
	 printf("decode error:%d\ndecoded length:%d\n",
		params.action_code,
		params.start - start);
	 return params.start;
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
      p += ret(p);
   }
   return p-buff;
}


#ifdef TEST_MYDSM
int counter(code_ptr start,code_ptr end)
{
   cerr << "testing mydsm" << endl;
   Counter mydsm(start,end);
   decode_instruction<Counter,true,true>(buff,mydsm);
   int counter = mydsm.counter;
   return counter;
}

#elif defined TEST_GDSM
int counter(code_ptr start,code_ptr end)
{
   cerr << "testing gdsm" << endl;
   using namespace gnu_dsm;
   Disassembler dsm(false,false,"intel,i386,addr32,data32");
   int counter = 0;
   code_ptr p = start;
   while (p < end) {
      p += dsm.insn_len(p);
      counter ++;
   }
   return counter;
}
#endif


int main()
{
   int len = gen_code(1000);
   boost::timer t0;

   int count = counter(buff,buff+len);


   float elapsed = t0.elapsed();
   cerr << "elapsed time = " << elapsed << " sec" << endl;
   cerr << "number of bytes:" << len << endl;
   cerr << "number of instructions:" << count << endl;

   return 0;
}


