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
#ifndef GNU_DISASSEMBLER
#define GNU_DISASSEMBLER
#include <dis-asm.h>
#include <string>

namespace gnu_dsm {
	class Disassembler;

//	bool is_continuous_instruction(const char* assembly_string);
	bool is_continuous_code(Disassembler& d,const char*addr,int len);


	class Disassembler {
	public:
	   // identical to objdump -M switch , so see objdump -h
	      Disassembler(bool att_syntax=true,bool x64 = false, const char* options = 0) ;

	   int disassemble(const void* address);

	   dis_insn_type get_last_instruction_type() const
	      { return  _disinfo.insn_type; }
	
	   const char* get_last_instruction() const
	      { return _buff ; }

	   char get_branch_delay_insns() const 	/* How many sequential insn's will run before
	      a branch takes effect.  (0 = normal) */
	      { return _disinfo.branch_delay_insns ; }

	   int get_prolog_len(const char* address,unsigned int minimum_len);
	   
	   int fprint(FILE* f,const void* start, int num_instr);
	   int fprint(FILE* f,const void* start,const void* end);
	   
	   int print(const void* start,int num_instr)
	      { return fprint(stdout, start,num_instr) ; }
	   int print(const void* start,const void* end)
	      { return fprint(stdout, start, end) ; }

	   void set_att_syntax() 
	      { _att_syntax =true; }
	   void set_intel_syntax() 
	      { _att_syntax =false; }

	   int insn_len(unsigned char* p) 
	      {
		 _buff_pos = 0;
		 return print_insn_i386((bfd_vma)p,&disinfo_counter_);
	      }
	
	private:
	   char* current_position() 
	      { return &_buff[_buff_pos]; }


	   disassemble_info _disinfo;
	   disassemble_info disinfo_counter_;
	   char _buff[100];
	   int  _buff_pos;
	   int  _creation_flag;
	   bool _att_syntax;
	   std::string _options;
	   friend void disassembler_printf(Disassembler* _this,const char*fmt,...);
	   friend void generic_print_address (bfd_vma addr, struct disassemble_info *info);
	};


} //gnu_dsm
#endif // GNU_DISASSEMBLER
