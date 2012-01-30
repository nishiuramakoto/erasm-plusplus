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

#ifndef FORMAT
#define FORMAT C
#endif

#define CONCAT(X,Y) X ## Y
#define PRINT_CODE_FUNC(X)  CONCAT(print_code_,X)
#define PRINT_CODE PRINT_CODE_FUNC(FORMAT)

#define ERASM_NO_META_ASSERT 1

#include <erasm/meta_prelude.hpp>
#include <erasm/dsm_x64.hpp>
#include <erasm/dsm_x64_util.hpp>
#include <erasm/faststream.hpp>
#include <erasm/x64_addr64_data32.hpp>
#include <erasm/x64_assembler_test_code_64_32.hpp>

#include <boost/timer.hpp>

#include <iostream>
#include <iomanip>

using namespace std;
using namespace erasm::x64;

inline ostream& print_code_A(ostream& os,const_code_ptr start,const_code_ptr end)
{ return os ; }

inline ostream& print_code_B(ostream& os,const_code_ptr start,const_code_ptr end)
{ return os << std::hex << (uint32_t) start << " " ; }

inline ostream& print_code_C(ostream& os,const_code_ptr start,const_code_ptr end)
{ return print_code(os,start,end) ; }


struct MyDsm : SimpleDsm<PRINT_CODE>
{
   typedef SimpleDsm<PRINT_CODE>  base;
   const_code_ptr end;
   MyDsm(const_code_ptr start,const_code_ptr end,ostream& os = erasm::cout) 
      : base(start,os) ,end(end)
      {}

   using base::action;

   action_result_type
   action(const Ret& insn)
      {
	 if (insn.end >= end) {
	    return finish(insn);
	 }
	 print_code(os(),insn);
	 print_instruction(os(),insn) << endl;
	 return next(insn);
      }
};

byte_t buff[1024 * 1024 * 10 ];

int gen_code(int n)
{
   using namespace erasm::x64::addr64::data32;
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
   Disassembler dsm(true,false,"intel,i386,addr64,data32");
   int count = dsm.print(beg,end);
   return count;
}
#elif defined TEST_UDIS
int dsm(code_ptr beg,code_ptr end )
{
   cerr << "testing udis86" << endl;
   ud_t ud_obj;
   unsigned char* p = (unsigned char*)  beg;
   size_t size = end - beg;
   int count = 0;

   ud_init(&ud_obj);
   ud_set_input_buffer(&ud_obj, p , size);

   ud_set_mode(&ud_obj, 64);

   ud_set_syntax(&ud_obj, UD_SYN_INTEL);
   ud_set_vendor(&ud_obj, UD_VENDOR_INTEL);

   int len;
   while (len = ud_disassemble(&ud_obj)) {
      unsigned char* pc = beg + ud_obj.pc;
      //printf("%08x",ud_obj.pc);
      PRINT_CODE(::erasm::cout,pc, pc + len);	 
      printf("\t%s\n", ud_insn_asm(&ud_obj));
      count ++;
   }
   return count;
}
#elif defined TEST_DR
int dsm(code_ptr beg,code_ptr end )
{
   cerr << "testing dr" << endl;
   int count = 0;
   disassemble_set_syntax(DR_DISASM_INTEL);

   byte* p = (byte*) beg;

   while (p < end) {
      byte* next = disassemble(GLOBAL_DCONTEXT,p,1);
      if (next) {
	 p = next ; 
      } else {
	 p ++;
      }
      count ++;
   }
   return count;
}
#else
int dsm(code_ptr beg,code_ptr end )
{
   cerr << "testing mydsm" << endl;
   typedef MyDsm dsm_type;
   dsm_type   mydsm(beg,end);
   erasm::x64::addr64::data32::decode<dsm_type>(buff,mydsm);
   return mydsm.get_counter();
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

