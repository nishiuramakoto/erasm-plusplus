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
# -*- coding:utf-8-unix ; -*-


do_test()
{
    $TEST_BIN | sed -e 's/$/;|/'  > out1.txt && \
    for x in $TEST_CODE 
    do
	$CPP $x | egrep -v '^ *$|^#.*|^ *//|^ */\\*.*\\*/ *$'| cut -d ' ' -f 3-
    done  > out2.txt && \
    paste out1.txt out2.txt  | column  -t -s '|' | $CHECKER > out3.txt
    EXIT=$?
    grep -v "^PASS:" out3.txt

    if [ $EXIT = 0 ];
    then
	echo "[PASS]"
    else
	echo "[FAIL]"
    fi
    exit $EXIT
}
