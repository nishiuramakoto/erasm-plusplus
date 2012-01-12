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
#ifndef MY_ERASM_X64_IMPLEMENTATION_DEFINED_HPP
#define MY_ERASM_X64_IMPLEMENTATION_DEFINED_HPP

// Compiler-dependent language constructs

#include <boost/static_assert.hpp>

#include <boost/cstdint.hpp>
#ifdef BOOST_NO_INT64_T
#error Sorry,we need int64_t even for legacy x86 mode
#endif

namespace erasm { namespace x64 {

using boost::int8_t;
using boost::int16_t;
using boost::int32_t;
using boost::int64_t;

using boost::uint8_t;
using boost::uint16_t;
using boost::uint32_t;
using boost::uint64_t;

typedef boost::int8_t  disp8_type;
typedef boost::int16_t disp16_type;
typedef boost::int32_t disp32_type;

typedef boost::uint8_t  offset8_t;
typedef boost::uint16_t offset16_t;
typedef boost::uint32_t offset32_t;
typedef boost::uint64_t offset64_t;

typedef uint8_t byte_t;
typedef byte_t* code_ptr;
typedef byte_t* code_pointer;
typedef byte_t* code_pointer_type;
typedef const byte_t * const_code_ptr;
typedef const byte_t * const_code_pointer;
typedef const byte_t * const_code_pointer_type;

// TODO:these types should be redefined to be unsigned types
typedef int8_t  imm8_t;
typedef int16_t imm16_t;
typedef int32_t imm32_t;
typedef int64_t imm64_t;

typedef int8_t  rel8_t;
typedef int16_t rel16_t;
typedef int32_t rel32_t;
typedef int64_t rel64_t;


// precise struct layout is implementation defined
#if defined(__GNUC__) || defined(_MSC_VER)
 
#pragma pack(push,1)

struct ModrmModrmByte
{
   unsigned int rm:3;
   unsigned int op_reg:3;
   unsigned int mod:2;
};

BOOST_STATIC_ASSERT(sizeof(ModrmModrmByte) == 1);

struct ModrmSibByte
{
   unsigned int base:3;
   unsigned int index:3;
   unsigned int ss:2;
};

BOOST_STATIC_ASSERT(sizeof(ModrmSibByte) == 1);

struct SibBytes {
   unsigned int base:3;
   unsigned int index:3;
   unsigned int ss:2;
   union 
   {
      int8_t  disp8;
      int32_t disp32;
      uint8_t  disp_bytes[4];
   };
};

BOOST_STATIC_ASSERT(sizeof(SibBytes) == 5);


union ModrmBytes
{
   uint8_t raw_bytes[6];
   struct {
      unsigned int rm:3;
      unsigned int op_reg:3;
      unsigned int mod:2;
      
      union {
	 SibBytes sib ;
	 int8_t  disp8;
	 int16_t disp16;
	 int32_t disp32;
	 uint8_t  disp_bytes[4];
      };
   };
};

BOOST_STATIC_ASSERT(sizeof(ModrmBytes) == 6);

union RexByte {
   unsigned int byte :8;
   struct {
      unsigned int b :1;
      unsigned int x :1;
      unsigned int r :1;
      unsigned int w :1;
      unsigned int :2;
      unsigned int rex :1;
      unsigned int :1;
   };
};

BOOST_STATIC_ASSERT(sizeof(RexByte) == 1);


union FarPtr16
{
   struct {
      uint16_t offset;
      uint16_t selector;
   };
   uint8_t raw_bytes[4];
};

BOOST_STATIC_ASSERT(sizeof(FarPtr16) == 4);

union FarPtr32
{
   struct {
      uint32_t offset;
      uint16_t selector;
   };
   uint8_t raw_bytes[6];
};

BOOST_STATIC_ASSERT(sizeof(FarPtr32) == 6);

union FarPtr64
{
   struct {
      uint64_t  offset;
      uint16_t  selector;
   };
   uint8_t raw_bytes[10];

};

BOOST_STATIC_ASSERT(sizeof(FarPtr64) == 10);

#pragma pack(pop)


#else
#error This compiler is not yet supported
#endif


}}

#endif // MY_ERASM_X64_IMPLEMENTATION_DEFINED_HPP
