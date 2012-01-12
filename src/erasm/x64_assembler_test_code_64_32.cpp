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

#include "erasm/x64.hpp"
#include <cstring>


#ifdef USE_SAMPLE_HEADER
#include "x64_assembler_sample.hpp"
#else
#include "erasm/x64_addr64_data32.hpp"
#endif

#include "erasm/x64_assembler_test_code_64_32.hpp"

namespace erasm { namespace x64 { namespace addr64 { namespace data32 {

int gen_manual_test(unsigned char* p)
{
   code_ptr p0 = p;
#include "erasm/x64_assembler_manual_test.cpp"
   return p-p0;
}

int gen_auto_test(unsigned char* p)
{
   code_ptr p0 = p;

#include "erasm/x64_assembler_auto_test.cpp"

   return p-p0;
}

}}}}
