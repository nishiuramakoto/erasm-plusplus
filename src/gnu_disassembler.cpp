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
#include "gnu_disassembler.hpp"
#include <cstring>
#include <cstdarg>
#include <cstdio>
#include <cstdlib>
#include <cstdarg>
#include <assert.h>

#include "dprintf.h"

#ifdef TRACE_DSM
#else
#undef TRACE
#define TRACE
#endif


static inline
gnu_dsm::Disassembler* disassembler(struct disassemble_info * info)
{ return reinterpret_cast<gnu_dsm::Disassembler*> (info->stream) ; }


namespace gnu_dsm {
int dummy_fprintf(void* dummy,const char *fmt,...) { return 0;}

/* Function used to get bytes to disassemble.  MEMADDR is the
   address of the stuff to be disassembled, MYADDR is the address to
   put the bytes in, and LENGTH is the number of bytes to read.
   INFO is a pointer to this struct.
   Returns an errno value or 0 for success.  */
int buffer_read_memory(bfd_vma memaddr, bfd_byte *myaddr, unsigned int length,
		       struct disassemble_info *info)
{
    
   memcpy(myaddr,reinterpret_cast<const void*>(memaddr),length);
   return 0;
}

/* Function which should be called if we get an error that we can't
   recover from.  STATUS is the errno value from read_memory_func and
   MEMADDR is the address that we were trying to read.  INFO is a
   pointer to this struct.  */
void perror_memory
(int status, bfd_vma memaddr, struct disassemble_info *info)
{
   DUMP(%d,status);
   DUMP(%08x,memaddr);
}

void perror_memory_dummy
(int status, bfd_vma memaddr, struct disassemble_info *info)
{
   return;
}

/* Function called to print ADDR.  */
void generic_print_address
(bfd_vma addr, struct disassemble_info *info)
{
   TRACE;
   sprintf(disassembler(info)->current_position(),"  <%08x>  ",(unsigned int)addr);
   TRACE;
}

/* Function called to print ADDR.  */
void generic_print_address_dummy
(bfd_vma addr, struct disassemble_info *info)
{
   return;
}

/* Function called to determine if there is a symbol at the given ADDR.
   If there is, the function returns 1, otherwise it returns 0.
   This is used by ports which support an overlay manager where
   the overlay number is held in the top part of an address.  In
   some circumstances we want to include the overlay number in the
   address, (normally because there is a symbol associated with
   that address), but sometimes we want to mask out the overlay bits.  */
int generic_symbol_at_address
(bfd_vma addr, struct disassemble_info * info)
{
   TRACE;
   return 0;
}

int generic_symbol_at_address_dummy
(bfd_vma addr, struct disassemble_info * info)
{
   return 0;
}

}

namespace gnu_dsm {



void disassembler_printf(Disassembler* d,const char*fmt,...)
{
   TRACE;
   assert(fmt);
   assert(strlen(fmt)<100);
   assert(d);
   va_list ap;
   va_start(ap,fmt);
   d->_buff_pos+=vsprintf(d->_buff+d->_buff_pos,fmt,ap);
   va_end(ap);
   TRACE;
}





Disassembler::Disassembler(bool att_syntax,bool x64,const char* options) 
   : _att_syntax(att_syntax)
   , _options(options ? options : "" )
{
   _buff_pos=0;
   INIT_DISASSEMBLE_INFO(_disinfo,this,disassembler_printf);
   //	_disinfo.application_data = this;
   _disinfo.flavour = bfd_target_coff_flavour;
   //_disinfo.flavour = bfd_target_elf_flavour;
   _disinfo.arch = bfd_arch_i386;

   if (x64 && att_syntax ) {
      _disinfo.mach = bfd_mach_x86_64;
   } else if (x64 && ! att_syntax) {
      _disinfo.mach = bfd_mach_x86_64_intel_syntax;
   } else if (!x64 && att_syntax) {
      _disinfo.mach = bfd_mach_i386_i386;
   } else if (!x64 && ! att_syntax) {
      _disinfo.mach = bfd_mach_i386_i386_intel_syntax;
   }

   // Why this requires a non-const pointer ?
   // We don't know, and we will never be sure who is responsibile for the allocated memory.
   // That's how it is when you program in C.
											   
   _disinfo.disassembler_options = _options.empty() ? NULL : (char*) _options.c_str();

   //_disinfo.endian = 0;
   _disinfo.insn_sets=0;
   _disinfo.section = NULL;
   _disinfo.symbols=NULL;
   _disinfo.read_memory_func=gnu_dsm::buffer_read_memory;
   _disinfo.memory_error_func=gnu_dsm::perror_memory;
   _disinfo.print_address_func = gnu_dsm::generic_print_address;
   _disinfo.symbol_at_address_func = gnu_dsm::generic_symbol_at_address;


   INIT_DISASSEMBLE_INFO(disinfo_counter_,this,dummy_fprintf);
   //	disinfo_counter_.application_data = this;
   disinfo_counter_.flavour = bfd_target_coff_flavour;
   //disinfo_counter_.flavour = bfd_target_elf_flavour;
   disinfo_counter_.arch = bfd_arch_i386;

   if (x64 && att_syntax ) {
      disinfo_counter_.mach = bfd_mach_x86_64;
   } else if (x64 && ! att_syntax) {
      disinfo_counter_.mach = bfd_mach_x86_64_intel_syntax;
   } else if (!x64 && att_syntax) {
      disinfo_counter_.mach = bfd_mach_i386_i386;
   } else if (!x64 && ! att_syntax) {
      disinfo_counter_.mach = bfd_mach_i386_i386_intel_syntax;
   }

   // Why this requires a non-const pointer ?
   // We don't know, and we will never be sure who is responsibile for the allocated memory.
   // That's how it is when you program in C.
											   
   disinfo_counter_.disassembler_options = _options.empty() ? NULL : (char*) _options.c_str();

   //disinfo_counter_.endian = 0;
   disinfo_counter_.insn_sets=0;
   disinfo_counter_.section = NULL;
   disinfo_counter_.symbols=NULL;
   disinfo_counter_.read_memory_func=gnu_dsm::buffer_read_memory;
   disinfo_counter_.memory_error_func=gnu_dsm::perror_memory_dummy;
   disinfo_counter_.print_address_func = gnu_dsm::generic_print_address_dummy;
   disinfo_counter_.symbol_at_address_func = gnu_dsm::generic_symbol_at_address_dummy;

}

int Disassembler::disassemble(const void* address)
{
   TRACE;
   _buff_pos=0;
   int ret;
   //		DUMP(%08x,address);

   ret= print_insn_i386( reinterpret_cast<bfd_vma>(address) , &_disinfo );
   TRACE;
   return ret;
}

void print_raw(const char*p,int l)
{
   TRACE;
   int i;
   int width=20;
   for (i=0;i<l;i++,width-=2) {
      printf("%02x",(unsigned char)*p++);
   }
   for (i=0;i<width;i++) {
      putchar(' ');
   }
   TRACE;
}

int Disassembler::fprint(FILE* file,const void* start,int num_instr)
{
   TRACE;
   const char* addr = (const char*)start;
   int count,l;
   
   for (count=0;count<num_instr;count++,addr+=l) {
      l=disassemble(addr);
      print_raw(addr,l);
      //		dprintf ("%08x %d %s\n",addr,l,_buff);
      fprintf (file,"%08x %d %s\n",(unsigned int)addr,l,_buff);
		
   }
   TRACE;
   return count;
}

int Disassembler::fprint(FILE* file,const void* start,const void* end)
{
   TRACE;
   const char* addr = (const char*) start;
   int l;
   int count=0;
   for (;addr<end;addr+=l) {
      l=disassemble(addr);
      print_raw(addr,l);
      fprintf (file,"%08x %d %s\n",(unsigned int)addr,l,_buff);
      count ++ ;
   }
   TRACE;
   return count;
}



int Disassembler::get_prolog_len(const char* address,unsigned int minimum_len)
{
   const char* p=address;
   while (p<address+minimum_len) 
      p+=disassemble(p);
   return p-address;
}

#define MATCH_PREFIX(prefix,str) (memcmp(prefix,str,strlen(prefix)) ==0)
bool is_continuous_instruction(const char* assembly_string)
{
   assert(assembly_string);
   if (MATCH_PREFIX("j",assembly_string) // Is this really correct?
       || MATCH_PREFIX("ljmp",assembly_string)
       || MATCH_PREFIX("call",assembly_string)
       || MATCH_PREFIX("lcall",assembly_string)
       || MATCH_PREFIX("ret",assembly_string) 
       || MATCH_PREFIX("nret",assembly_string) 
       || MATCH_PREFIX("lret",assembly_string))
      return false;
   else
      return true;
}


bool is_continuous_code(Disassembler& d,const char*addr,int len)
{
	
   int oplen;
   while (len>0) {
		
      oplen=d.disassemble(addr);
		
      len-=oplen;
      addr+=oplen;

      // don't know but this doesn't work
      // 		switch(d.get_get_last_instruction_type()) {
      // 		case dis_noninsn:
      // 		case dis_nonbranch:
      // 		case dis_dref:
      // 		case dis_dref2:
      // 			continue;
      // 		default:
      // 			return false;
      // 		}
      if (not is_continuous_instruction(d.get_last_instruction()))
	 return false;
		
   }
	
   return true;
}

	
}
