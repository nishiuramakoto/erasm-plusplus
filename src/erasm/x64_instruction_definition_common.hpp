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
#ifndef MY_ERASM_X64_INSTRUCTION_DEFINITION_COMMON_HPP
#define MY_ERASM_X64_INSTRUCTION_DEFINITION_COMMON_HPP

// for std::pair
#include <utility> 

#include "meta_prelude.hpp"
#include "erasm/x64.hpp"
#include "erasm/intel_prefix_bitset.hpp"

namespace erasm { namespace x64 {
enum ACTION_CODE {
   ACTION_FINISH,
   ACTION_CONTINUE,
   ACTION_ERROR,
};
typedef ACTION_CODE action_code_type ;
typedef std::pair<const_code_ptr,action_code_type> action_result_type;


struct InstructionData
{
   const_code_ptr start;
   const_code_ptr opcode_start;
   const_code_ptr end;

   action_code_type action_code;

   Segment segment;
   RexByte        rex;
   prefix_bitset  prefix;

   bool is_data16;
   bool is_addr32;

   InstructionData& data()
      { return *this; }

   const InstructionData& data() const
      { return *this; }
};


}}




#endif // MY_ERASM_X64_INSTRUCTION_DEFINITION_COMMON_HPP
