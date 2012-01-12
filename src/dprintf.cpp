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
#include "dprintf.h"
#include <stdio.h>
#include <assert.h>
#include <stdarg.h>
#include <stdlib.h>


#ifdef WINDOWS
#include <windows.h>
#include <process.h>
#else
#include <unistd.h>
#include <sys/types.h>
#define InitializeCriticalSection(x)
#define EnterCriticalSection(x)
#define LeaveCriticalSection(x)
#define OutputDebugString(x)
#define CRITICAL_SECTION int
#define ExitProcess(x) exit(x)
#define GetCurrentThreadId(x) 0
#endif



int dprintf_use_OutputDebugString=0;

#define WINTRACE if (dprintf_use_OutputDebugString) {			\
	sprintf(dprintf_data.buf,"%s:%d",__FUNCTION__,__LINE__) ;	\
	OutputDebugString(dprintf_data.buf) ;				\
} 
#define DPRINTF_OUTPUT_DEBUGSTRING(str) do { if (dprintf_use_OutputDebugString) {OutputDebugString(str); } } while(0)

int dprintf_initialized=0;

struct DprintfData
{
	FILE* 		 stream  ;
	CRITICAL_SECTION  cs;
 	int   		  pid ;
	char buf[1024];
	~DprintfData()
		{ if (stream && stream != stdout && stdout != stderr) fclose(stream) ; }
} ;
static DprintfData dprintf_data;

void 
dprintf_init()
{
	dprintf_data.stream = stdout;
	InitializeCriticalSection(&dprintf_data.cs);
	dprintf_data.pid = getpid();
	dprintf_initialized = 1;
}


// No destructor.
// There is no standard way to guarantee an object to be constructed first/destructed last
// so leave proper cleaning-up  to the system.



int dprintf_open(const char* name ,const char* mode) 
{
	printf("dprintf_open(%s,%s) but continue using stdout\n",name,mode);
	return 0;
	if (dprintf_data.stream && dprintf_data.stream != stdout && dprintf_data.stream != stderr)
		fclose(dprintf_data.stream);
	dprintf_data.stream =fopen(name,mode);
	if (!dprintf_data.stream) {
		WINTRACE;
	}
	return dprintf_data.stream != NULL;
}

void dprintf_close()
{
	if (dprintf_data.stream && dprintf_data.stream != stdout && dprintf_data.stream != stderr) {
		fclose(dprintf_data.stream);
		WINTRACE;
	}
}

void dprintf_flush() 
{
	assert(dprintf_data.stream);
	fflush(dprintf_data.stream);
}

FILE* dprintf_stream() 
{
	return dprintf_data.stream;
}


void
dprintf_real (const char * fmt, ...) 
{
	if (!dprintf_initialized)
		dprintf_init();
	int tid=GetCurrentThreadId();
	int pid=dprintf_data.pid;

	FILE* s = dprintf_data.stream;
	assert(s);

	EnterCriticalSection(&dprintf_data.cs);
	{
		char* p = dprintf_data.buf;
		va_list ap;
		va_start(ap, fmt);
		p+=sprintf(p,"%x:%x:" ,pid,tid);
		p+=vsprintf(p,fmt, ap);
		fputs(dprintf_data.buf,dprintf_data.stream);
		DPRINTF_OUTPUT_DEBUGSTRING(dprintf_data.buf);
		fputc('\n',s);
		va_end(ap);
	}
	LeaveCriticalSection(&dprintf_data.cs);
}

void
dprintf_raw (const char * fmt, ...) 
{
	if (!dprintf_initialized)
		dprintf_init();

	FILE* s = dprintf_data.stream;
	assert(s);

	EnterCriticalSection(&dprintf_data.cs);
	{
		char* p = dprintf_data.buf;
		va_list ap;
		va_start(ap, fmt);
		p+=vsprintf(p,fmt, ap);
		fputs(dprintf_data.buf,dprintf_data.stream);
		DPRINTF_OUTPUT_DEBUGSTRING(dprintf_data.buf);
		va_end(ap);
	}
	LeaveCriticalSection(&dprintf_data.cs);
}



void dprintf_assert(const char* expr,const char* file,int line , const char* function )
{
	dprintf("%s:%d:%s:ASSERTION FAILED:%s" ,file,line,function,expr);
// TODO: need portable process exit function (mingw's abort() only aborts current thread.)
	ExitProcess(127);
}
