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
#include "erasm/x64_assembler_test_code_64_32.hpp"
#include "gnu_disassembler.hpp"
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <boost/timer.hpp>

using namespace std;
using namespace erasm::x64;


struct Counter
{
   const_code_ptr start;
   const_code_ptr end;
   int counter;

   Counter(const_code_ptr start,const_code_ptr end) 
      : start(start),end(end),counter(0)
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
	 counter ++;
	 return cont(insn.data());
      }

   template<class Insn,class Op1>
   action_result_type
   action(const Insn& insn,
	  const Op1& op1)
      {
	 counter ++;
	 return cont(insn.data());
      }

   template<class Insn,class Op1,class Op2>
   action_result_type
   action(const Insn& insn,
	  const Op1& op1,
	  const Op2& op2)
      {
	 counter ++;
	 return cont(insn.data());
      }

   template<class Insn,class Op1,class Op2,class Op3>
   action_result_type
   action(const Insn& insn,
	  const Op1& op1,
	  const Op2& op2,
	  const Op3& op3)
      {
	 counter ++;
	 return cont(insn.data());
      }
   
   const_code_ptr
   error(const InstructionData& params)
      {
	 printf("decode error:%d\ndecoded length:%d\n",
		params.action_code,
		params.start - start);
      }

};



int gcounter(code_ptr start,code_ptr end)
{
   using namespace gnu_dsm;
   Disassembler dsm(false,true);
   int counter = 0;
   code_ptr p = start;
   while (p < end) {
      p += dsm.disassemble(p);
      counter ++;
   }
   return counter;
}





byte_t buff[1024 * 1024 * 20];

int main()
{
   code_ptr p = buff;
   for (int i=0;i<1000;i++) {
      p += erasm::x64::addr64::data32::gen_manual_test(p);
      p += erasm::x64::addr64::data32::gen_auto_test(p);
   }

   int len = p-buff;

   //cout << "gnu dsm:" << endl;
   //dsm_gnu(buff,p);

   {
      boost::timer t0;
#ifdef TEST_MYDSM
      printf("testing mydsm\n");

      Counter mydsm(buff,buff+len);
      decode_instruction<Counter,true,true>(buff,mydsm);
      int counter = mydsm.counter;
#else
      printf("testing gdsm\n");
      int counter = gcounter(buff,buff+len);
#endif
      printf("elapsed time =%f sec\n",t0.elapsed());
      printf("number of instruction:%d\n",counter);
      printf("number of bytes:%d\n",len);
   }



   return 0;
}

