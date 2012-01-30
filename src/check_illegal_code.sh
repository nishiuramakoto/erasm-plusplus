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

TMPDIR=`mktemp -d`
trap 'rm -rf "$TMPDIR"' EXIT
TMPCPP="$TMPDIR/tmp.cpp"


MAX_ERROR=10
function compile_test_fail()
{
    
    STATEMENT="$1"
    printf "$TEMPLATE" "$STATEMENT" > $TMPCPP
    #if $CXX -fmax-errors=$MAX_ERROR -I$srcdir $CPPFLAGS $CXXFLAGS  -c  $TMPCPP;
    if $CXX -I$srcdir $CPPFLAGS $CXXFLAGS  -c  $TMPCPP;
    then
	echo "Statement was not rejected:$STATEMENT"
	return 1
    else
	return 0
    fi
}

function check_file()
{
    CODE_FILE=$1
    echo Checking illegal code in $CODE_FILE

    FIRST_TEST="add(p,eax,0u);"
    if compile_test_fail "$FIRST_TEST" ;then
	echo "FAIL (compilation failed)"
	exit 1
    else
	echo OK
    fi


    egrep -v '^ *$|^ *//'  "$CODE_FILE" |
    (EXIT_STATUS=0;
	while read x
	do
	    echo CHECKING ILLEGAL CODE:"$x"
	    if compile_test_fail "$x";then
		echo OK
	    else
		echo "FAIL (compilation unexpectedly succeeded)"
		EXIT_STATUS=1
	    fi
	done;
	exit $EXIT_STATUS)
}

