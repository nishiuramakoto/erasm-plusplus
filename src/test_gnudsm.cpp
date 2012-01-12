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
   along with ERASM++; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */
#include "test_gnudsm.hpp"
#include "gnu_disassembler.hpp"
#include "assembler.hpp"

using namespace my;
using namespace std;

CPPUNIT_TEST_SUITE_REGISTRATION( GnuDsmTest );


int f() { return 0 ; }

void
GnuDsmTest::testAssemble()
{
	char buf[100];
	char* p=buf;
	
	int instruction_len = assemble_PUSH(p,regmemw(EBP));
	CPPUNIT_ASSERT(	buf[0] == 0x66 );
	CPPUNIT_ASSERT( instruction_len == 3 );
}


void
GnuDsmTest::testDisassemble()
{
	char buf[100];
	char* p=buf;
	gnu_dsm::Disassembler dsm;
	
	p+=assemble_PUSH(p,regmemw(EBP));

	int len=dsm.disassemble(buf);
	CPPUNIT_ASSERT(	string("push   %bp") == dsm.get_last_instruction() );
}
