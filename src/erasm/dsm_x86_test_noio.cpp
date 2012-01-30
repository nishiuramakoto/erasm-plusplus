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

#define PRINT_CODE  print_code_A
#define ERASM_NO_META_ASSERT 1

#include "gnu_disassembler.hpp"
#include <erasm/dsm_x86.hpp>
#include <erasm/dsm_x86_util.hpp>
#include <erasm/x86_assembler_test_code_32_32.hpp>
#include <erasm/x86_addr32_data32.hpp>
#include <erasm/faststream.hpp>
#include <boost/timer.hpp>

#ifdef TEST_UDIS
#include <udis86.h>
#endif

#ifdef TEST_DR
#include <dr_api.h>
#endif

using erasm::x86::const_code_ptr;

inline std::ostream& print_code_A(std::ostream& os,const_code_ptr start,const_code_ptr end)
{ return os ; }

inline std::ostream& print_code_B(std::ostream& os,const_code_ptr start,const_code_ptr end)
{ return os << std::hex << (uint32_t) start << " " ; }

inline std::ostream& print_code_C(std::ostream& os,const_code_ptr start,const_code_ptr end)
{ return erasm::x86::print_code(os,start,end) ; }


using namespace std;
using namespace erasm::x86;

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

   static char buf[200];
   int count = 0;
   code_ptr p = beg;

   while (p<end) {
      p += dsm.disassemble(p);
      count += dsm.get_buf_size();
   }
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
   //ud_set_input_file(&ud_obj, stdin);
   ud_set_input_buffer(&ud_obj, p , size);

   //ud_set_mode(&ud_obj, 64);
   ud_set_mode(&ud_obj, 32);

   ud_set_syntax(&ud_obj, UD_SYN_INTEL);
   ud_set_vendor(&ud_obj, UD_VENDOR_INTEL);

   int len;
   int printed = 0;
   static char buf[200];

   while (len = ud_disassemble(&ud_obj)) {
      unsigned char* pc = beg + ud_obj.pc;
      //printf("%08x",ud_obj.pc);
      PRINT_CODE(::erasm::cout_counter,pc, pc + len);	 
      printed += sprintf(buf,"\t%s\n", ud_insn_asm(&ud_obj));
      count ++;
   }
   return printed + erasm::cout_counter;
}
#elif defined TEST_DR
int dsm(code_ptr beg,code_ptr end )
{
   void * context = dr_standalone_init();
   cerr << "testing dr" << endl;
   int count = 0;
   disassemble_set_syntax(DR_DISASM_INTEL);

   byte* p = (byte*) beg;

   while (p < end) {
      byte* next = disassemble(context,p,1);
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
   dsm_type   mydsm(beg,end,erasm::cout_counter);
   erasm::x86::addr32::data32::decode<dsm_type>(buff,mydsm);
   return erasm::cout_counter.get_count();
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

