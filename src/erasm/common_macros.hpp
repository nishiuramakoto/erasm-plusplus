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

#ifndef MY_COMMON_MACROS
#define MY_COMMON_MACROS

#include <cstdlib>
#include <iostream>

namespace erasm { namespace impl {
// Just to suppress an annoying warning from g++
inline bool false_()
{ return false ;}
}}

#ifdef NDEBUG
#define TRACE
#define DUMP(x)
#define DUMP2(x,y)
#define DUMP3(x,y,z)
#define DUMP4(x,y,z,w)

#else // ! defined NDEBUG

#ifndef DEBUG_STREAM
#define DEBUG_STREAM std::cerr
#endif

#define DEBUG_FLUSH  (DEBUG_STREAM.flush())


#define CRASH    (*(int*)0 = 0)
// might vary across compilers

#ifdef __GNUC__
#define TRACE_INFO __FILE__ << ':' << __LINE__ <<':' << __PRETTY_FUNCTION__
#else
#define TRACE_INFO __FILE__ << ':' << __LINE__
#endif

#define TRACE					\
(  DEBUG_STREAM << TRACE_INFO <<  std::endl ,	\
   DEBUG_FLUSH,					\
   ::erasm::impl::false_() )

#define FATAL(mesg)						\
( DEBUG_STREAM << TRACE_INFO << ':' << mesg << std::endl ,	\
  DEBUG_FLUSH,							\
  std::exit(-1),						\
  ::erasm::impl::false_())

#define UNDEFINED FATAL("undefined")


#define DUMP_LINE(x)   # x << '=' << (x) << ':'

#define DUMP(x)				\
   ( DEBUG_STREAM << TRACE_INFO << ':'		\
     << DUMP_LINE(x)				\
     << std::endl , DEBUG_STREAM.flush(), ::erasm::impl::false_())

#define DUMP2(x,y)				\
   ( DEBUG_STREAM << TRACE_INFO << ':'		\
     << DUMP_LINE(x)				\
     << DUMP_LINE(y)				\
     << std::endl , DEBUG_STREAM.flush(), ::erasm::impl::false_())

#define DUMP3(x,y,z)				\
   ( DEBUG_STREAM << TRACE_INFO << ':'		\
     << DUMP_LINE(x)				\
     << DUMP_LINE(y)				\
     << DUMP_LINE(z)				\
     << std::endl , DEBUG_STREAM.flush(), ::erasm::impl::false_())


#define DUMP4(x,y,z,w)				\
( DEBUG_STREAM << TRACE_INFO << ':'		\
  << DUMP_LINE(x)				\
  << DUMP_LINE(y)				\
  << DUMP_LINE(z)				\
  << DUMP_LINE(w)				\
  << std::endl , DEBUG_STREAM.flush(), ::erasm::impl::false_())

#define DUMP5(x,y,z,w,u)				\
   ( DEBUG_STREAM << TRACE_INFO << ':'			\
     << DUMP_LINE(x)					\
     << DUMP_LINE(y)					\
     << DUMP_LINE(z)					\
     << DUMP_LINE(w)					\
     << DUMP_LINE(u)					\
     << std::endl , DEBUG_STREAM.flush(), ::erasm::impl::false_())

#endif // #ifdef NDEBUG


#ifndef ALWAYS_INLINE 
#ifdef __GNUC__
#define ALWAYS_INLINE __attribute__((always_inline))
#else
#define ALWAYS_INLINE 
#endif
#endif

#endif // MY_COMMON_MACROS
