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

CXX=g++
CPPFLAGS="-I. -DERASM_NO_META_ASSERT"
CODE_FILE="$srcdir/erasm/x86_assembler_illegal_code_32_32.cpp"
TEMPLATE='
#include "erasm/x86.hpp" 
#include "erasm/x86_addr32_data32.hpp"
using namespace erasm;
using namespace erasm::x86;
using namespace erasm::x86::addr32;
using namespace erasm::x86::addr32::data32;
namespace {
void test_illegal_code(code_ptr p)
{
   %s;
}
}
'

.  $srcdir/check_illegal_code.sh
check_file "$CODE_FILE"

