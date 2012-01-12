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
#include <cstring>
#include "erasm/x64_assembler_test_code_64_32.hpp"
#include "erasm/x64.hpp"
#include "gnu_disassembler.hpp"

using namespace std;
using namespace erasm::prelude::x64;


byte_t buff[1024 * 1024];

int main()
{
   using namespace std;
   using namespace gnu_dsm;
   Disassembler dsm(false,true);
   int len = 0;
   code_ptr p = & buff[0];
   p+= erasm::x64::addr64::data32::gen_manual_test(p);
   p+= erasm::x64::addr64::data32::gen_auto_test(p);
   len = p - buff;
   cerr << "code len=" << len << endl;
   dsm.print(buff,buff+len);
   return 0;
}
