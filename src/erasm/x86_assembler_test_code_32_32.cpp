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
#define ERASM_NO_META_ASSERT 1

#include "erasm/x86.hpp"

#include "erasm/x86_addr32_data32.hpp"
#include "erasm/x86_assembler_test_code_32_32.hpp"

static int erasm_gen_manual_test_x86_32_32(unsigned char* p)
{
   using namespace erasm::x86;
   using namespace erasm::x86::addr32;
   using namespace erasm::x86::addr32::data32;

   code_ptr p0 = p;

#include "erasm/x86_assembler_manual_test.cpp"

   return p-p0;
}

static int erasm_gen_auto_test_x86_32_32(unsigned char* p)
{
   using namespace erasm::x86;
   using namespace erasm::x86::addr32;
   using namespace erasm::x86::addr32::data32;
   code_ptr p0 = p;

   // auto-generated tests
#include "erasm/x86_assembler_auto_test.cpp"

   return p-p0;
}


namespace erasm { namespace x86 { namespace addr32 { namespace data32 {

int gen_manual_test(unsigned char* p)
{
   return erasm_gen_manual_test_x86_32_32(p);
}

int gen_auto_test(unsigned char* p)
{
   return erasm_gen_auto_test_x86_32_32(p);
}

}}}}
