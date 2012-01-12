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
#    along with ERASM++; see the file COPYING3.  If not see
#    <http://www.gnu.org/licenses/>.  
#!/bin/sh
TEST_BIN=./x64_assembler_test_64_32
TEST_CODE="${srcdir}/erasm/x64_assembler_manual_test.cpp ${srcdir}/erasm/x64_assembler_auto_test.cpp"
CHECKER=./erasm_checker

echo $CXX
echo $CPP

if [ $OSTYPE = msys ]
then
    echo Support for 64-bit code by Windows version of libopcodes is buggy,
    echo so we silently fail at the moment.
    exit 1
fi


. ${srcdir}/test_assembler_common.sh
echo testing $0 ...
do_test
