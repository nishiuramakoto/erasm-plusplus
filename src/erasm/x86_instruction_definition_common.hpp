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
#ifndef MY_ERASM_X86_INSTRUCTION_DEFINITION_COMMON_HPP
#define MY_ERASM_X86_INSTRUCTION_DEFINITION_COMMON_HPP

#include "erasm/x64_instruction_definition_common.hpp"
#include "erasm/x86.hpp"

namespace erasm { namespace x86 {

using x64::prefix_bitset;
using x64::ACTION_FINISH;
using x64::ACTION_CONTINUE;
using x64::ACTION_ERROR;
using x64::action_code_type;
using x64::action_result_type;

struct InstructionData
{
   const_code_ptr start;
   const_code_ptr opcode_start;
   const_code_ptr end;

   action_code_type action_code;

   prefix_bitset  prefix;
   Segment segment;
   bool is_data16;
   bool is_addr32;

   InstructionData& data()
      { return *this; }

   const InstructionData& data() const
      { return *this; }
};


}}

#endif // MY_ERASM_X86_INSTRUCTION_DEFINITION_COMMON_HPP
