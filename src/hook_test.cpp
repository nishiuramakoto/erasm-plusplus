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

#include "assembler.hpp"
#include "gnu_disassembler.hpp"
#include <cstdio>
#include <cstdlib>
#include "memory_protection.hpp"
#include <errno.h>
#include <memory>

const int lenJMP = 5;
inline int asm_jmp(char* addr, const char* target)
{  
	int l=assemble_JMP(addr, target-addr-lenJMP);
	assert(l==lenJMP);
	return l;
}

// this does not care abount memory protection.
template<class _Alloc = std::allocator<char> >
struct hook_buffer 
{
	hook_buffer(char* target_func,int target_prolog_len)
		: _target_func(target_func) , _target_prolog_len(target_prolog_len) , _status(false)
		{
			_buffer = _Alloc().allocate(target_prolog_len+lenJMP);
			if (!_buffer) { _status = false ; return ;}

			memcpy (_buffer                  ,target_func, target_prolog_len);
			asm_jmp(_buffer+target_prolog_len,target_func+ target_prolog_len);
			_status=true;
		}
	int hook(const char* installed_func) 
		{
			assert(_status);
			return asm_jmp(_target_func,installed_func);
		}
	int unhook()
		{
			assert(_status);
			memcpy(_target_func,_buffer,_target_prolog_len);
		}
	~hook_buffer() 
		{
			if (_buffer) _Alloc().deallocate(_buffer,size());
		}
	operator bool () const { return _status ; }
	const char* begin() const { return _buffer ; }
	const char* end  () const { return _buffer + size() ; }
	size_t size() const { return _target_prolog_len + lenJMP ; }
	char* target() { return _target_func ; }
	int   prolog_len() { return _target_prolog_len ; }
private:	
	char* _target_func;
	int   _target_prolog_len;
	char* _buffer;
	bool  _status;

	hook_buffer(const hook_buffer&);
};



gnu_dsm::Disassembler g_dsm;



hook_buffer<>*  install_hook(char* target_func,const char* install_func)
{

//	printf("hooked func=%08x\ninstalled func=%08x\n",target_func,install_func);

	int prolog_len=g_dsm.get_prolog_len(target_func,lenJMP);
//	g_dsm.print(target_func,target_func+prolog_len);

//	puts("preparing hook buffer");

	hook_buffer<>* hook = new hook_buffer<> (target_func,prolog_len);
	assert(hook && *hook);

	{
		// assume current protection mode is PROT_READ|PROT_EXEC
		LinuxCodeProtection cp((const void*)target_func,prolog_len);
		if (!cp) {
			puts(strerror(errno));
			abort();
		}
		hook->hook(install_func);
//		g_dsm.print(hook->begin(),hook->end());
	}
	
	return hook;
}

void
destruct_hook ( hook_buffer<>* hook)
{
	assert(hook && *hook);
	{
		// assume current protection mode is PROT_READ|PROT_EXEC
		LinuxCodeProtection cp((const void*)(hook->target()),hook->prolog_len());
		if (!cp) {
			puts(strerror(errno));
			abort();
		}
		hook->unhook();
//		g_dsm.print(hook->target(),hook->target()+hook->prolog_len());
	}
	delete hook;
}


typedef int (*hooked_entry_type)(const char* mess);
hooked_entry_type hooked_entry;

int hooked (const char* mess)
{
	printf ("hooked func:%s\n",mess);
	return 10;
}


int installed(const char* mess)
{
	printf ("installed func:%s\n",mess);
	hooked_entry("this should not be hooked");
	return 10;
}


int main() 
{
	hooked("ok?");
	hook_buffer<> * hook = install_hook((char*)hooked,(const char*)installed);
	hooked_entry = (hooked_entry_type) (hook->begin());
	hooked("ok?");

	destruct_hook(hook);
	hooked("ok?");
}
