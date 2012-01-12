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
#ifndef MY_ERASM_DSM_X64_PREFIX_HPP
#define MY_ERASM_DSM_X64_PREFIX_HPP

#include <boost/static_assert.hpp>

namespace erasm {
const unsigned int PREFIX_LOCK		= 0xF0U;
const unsigned int PREFIX_REPNE		= 0xF2U;
const unsigned int PREFIX_REPNZ		= 0xF2U;
const unsigned int PREFIX_F2		= 0xF2U;
const unsigned int PREFIX_REP		= 0xF3U;
const unsigned int PREFIX_REPE		= 0xF3U;
const unsigned int PREFIX_REPZ		= 0xF3U;
const unsigned int PREFIX_F3		= 0xF3U;
const unsigned int PREFIX_CS		= 0x2EU;
const unsigned int PREFIX_SS		= 0x36U;
const unsigned int PREFIX_DS		= 0x3EU;
const unsigned int PREFIX_ES		= 0x26U;
const unsigned int PREFIX_FS		= 0x64U;
const unsigned int PREFIX_GS		= 0x65U;
const unsigned int PREFIX_NOT_BRANCH	= 0x2EU;
const unsigned int PREFIX_BRANCH	= 0x3EU;
const unsigned int PREFIX_OPERAND_SIZE	= 0x66U;
const unsigned int PREFIX_66		= 0x66U;
const unsigned int PREFIX_ADDRESS_SIZE	= 0x67U;

const unsigned int PREFIX_FLAG_LOCK	= 0x1U;
const unsigned int PREFIX_FLAG_REPNE	= 0x2U;
const unsigned int PREFIX_FLAG_REP	= 0x4U;
const unsigned int PREFIX_FLAG_CS	= 0x8U;
const unsigned int PREFIX_FLAG_SS	= 0x10U;
const unsigned int PREFIX_FLAG_DS	= 0x20U;
const unsigned int PREFIX_FLAG_ES	= 0x40U;
const unsigned int PREFIX_FLAG_FS	= 0x80U;
const unsigned int PREFIX_FLAG_GS	= 0x100U;
const unsigned int PREFIX_FLAG_OPERAND_SIZE	= 0x200U;
const unsigned int PREFIX_FLAG_ADDRESS_SIZE	= 0x400U;
const unsigned int PREFIX_FLAG_REX	= 0x400000U;
const unsigned int PREFIX_FLAG_REX_W	= 0x480000U;
const unsigned int PREFIX_FLAG_REX_R	= 0x440000U;
const unsigned int PREFIX_FLAG_REX_X	= 0x420000U;
const unsigned int PREFIX_FLAG_REX_B	= 0x410000U;
}


namespace erasm {  namespace x64 {

// Don't use STL bitsets (or any other STL containers)
// because they might throw exceptions.

#pragma pack(push,1)

union prefix_bitset {
   unsigned int flags;
   
   struct {
      unsigned int :1;
      unsigned int :1;
      unsigned int :1;
      unsigned int not_branch:1; // == cs
      unsigned int :1;
      unsigned int branch:1;   // == ds
   };
   
   struct {
      unsigned int lock:1;
      unsigned int repne:1;
      unsigned int rep:1;
      unsigned int cs:1;
      unsigned int ss:1;
      unsigned int ds:1;
      unsigned int es:1;
      unsigned int fs:1;
      unsigned int gs:1;
      unsigned int operand_size:1;
      unsigned int address_size:1;
      unsigned int :5;
      
   };
};

BOOST_STATIC_ASSERT(sizeof(prefix_bitset) <= 4);

#pragma pack(pop)


}}


#endif // MY_ERASM_DSM_X64_PREFIX_HPP
