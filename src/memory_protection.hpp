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
#ifndef __MY_MEMORY_PROTECTION
#define __MY_MEMORY_PROTECTION

#if  defined HAVE_SYS_MMAN_H && defined HAVE_UNISTD_H
#include <sys/mman.h>
#include <unistd.h>
// TODO: Obtain existing protection mode.How is it done in linux ?

inline const void* align_on_page (const void*addr)
{
	size_t page_size = (size_t) sysconf (_SC_PAGESIZE);
	return (const void*)(page_size *((size_t)addr/page_size));
}
class LinuxMemoryProtection 
{
	int _old;
	const void*_addr;
	size_t _len;
	bool _success;
public:
	LinuxMemoryProtection(int old_protection,int new_protection,const void*addr,size_t len) 
		: _old(old_protection) 
		{
			_addr=align_on_page(addr);
			_len = (size_t)addr-(size_t)_addr+len;
			_success = mprotect((void*)_addr,_len,new_protection) == 0;
		}
	~LinuxMemoryProtection() 
		{
			if (_success) mprotect((void*)_addr,_len,_old);
		}
	operator bool () const { return _success ; }
	bool operator ! () const { return! _success ; }
};

// sentry for code protection . 
class LinuxCodeProtection : public LinuxMemoryProtection
{
public:
	LinuxCodeProtection(const void*addr,size_t len) 
		: LinuxMemoryProtection(PROT_READ|PROT_EXEC , PROT_READ | PROT_WRITE |PROT_EXEC , addr,len)
		{ }
};
typedef LinuxMemoryProtection MemoryProtection;
typedef LinuxCodeProtection CodeProtection;
#endif

#endif // __MY_MEMORY_PROTECTION
