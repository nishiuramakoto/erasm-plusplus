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
#ifndef __DPRINTF_H
#define __DPRINTF_H

#include <stdio.h>

#ifdef  __cplusplus
extern "C" {
#endif // __cplusplus

#ifdef NDEBUG

#ifndef NO_DPRINTF
#define NO_DPRINTF
#endif //NO_DPRINTF

#ifndef NO_TRACE
#define NO_TRACE
#endif //NO_TRACE

#ifndef NO_DUMP
#define NO_DUMP
#endif  //NO_DUMP


#endif //NDEBUG

#ifdef NO_TRACE
#define TRACE
#else
#define TRACE do {dprintf("%s:%d:%s" ,__FILE__ , __LINE__,__FUNCTION__); dprintf_flush() ; } while(0);
#endif //NO_TRACE

#ifdef NO_DUMP
#define  DUMP(format,x)
#else
#define  DUMP(format , x)  do { dprintf("%s:%d:%s:%s  = "  # format , __FILE__ , __LINE__ , __FUNCTION__ ,  # x , (x)) ;   dprintf_flush() ; } while(0)
#endif //NO_DUMP

// prepends pid,appends newline
void dprintf_real    (const char * fmt, ...) ; 
// no pretty printing
void dprintf_raw    (const char * fmt, ...) ; 

// misc util
int  dprintf_open(const char* name,const char* mode) ;
void dprintf_close() ;
void dprintf_flush () ; 
FILE* dprintf_stream(); 
void dprintf_assert(const char* expr,const char* file,int line , const char* function ) ;
// flags to use OutputDebugString
extern int dprintf_use_OutputDebugString;

#ifdef _MSC_VER

// cl.exe cannot handle __VA_ARGS__ ???
#ifdef NO_DPRINTF
inline	void dprintf(...) { }
#else
#define dprintf dprintf_real
#endif //NO_DPRINTF

#else
// Good compilers handle __VA_ARGS__ correctly as per The Standard.

#ifdef NO_DPRINTF
#define dprintf(...) 
#else
#define dprintf(...) dprintf_real(__VA_ARGS__)
#endif  // NO_DPRINTF


#endif // _MSC_VER

	
#ifdef  __cplusplus
} //extern "C" {
#endif

#endif // __DPRINTF_H
