#!/bin/sh
#    Copyright (C) 2011,2012 Makoto Nishiura.

#    This file is part of ERASM++.

#    ERASM++ is free software; you can redistribute it and/or modify it under
#    the terms of the GNU General Public License as published by the Free
#    Software Foundation; either version 3, or (at your option) any later
#    version.

#    ERASM++ is distributed in the hope that it will be useful, but WITHOUT ANY
#    WARRANTY; without even the implied warranty of MERCHANTABILITY or
#    FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
#    for more details.

#    You should have received a copy of the GNU General Public License
#    along with ERASM++; see the file COPYING.  If not see
#    <http://www.gnu.org/licenses/>.  

#CXX=g++
CPPFLAGS="-I. -DERASM_NO_META_ASSERT"
CODE_FILE="$srcdir/erasm/x64_assembler_illegal_code.cpp"

TEMPLATE='
#include "erasm/x64.hpp" 
#include "erasm/x64_addr64_data32.hpp"
using namespace erasm::x64;
using namespace erasm::x64::addr64;
using namespace erasm::x64::addr64::data32;
void test64(code_ptr p)
{
   %s;
}
' 

.  $srcdir/check_illegal_code.sh
check_file "$CODE_FILE"
