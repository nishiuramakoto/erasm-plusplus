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

AC_INIT([ERASM],[0.1.2],[nishiuramakoto@gmail.com])
AC_CONFIG_HEADERS([src/config.h])
AC_CONFIG_AUX_DIR([config])
AC_CONFIG_MACRO_DIR([m4])
AC_LANG([C++])
AM_INIT_AUTOMAKE([subdir-objects])

AC_PROG_CC
AC_PROG_CXX
AC_PROG_INSTALL

LT_INIT


AC_CHECK_HEADER([sys/mman.h],
		[AC_DEFINE([HAVE_SYS_MMAN_H],1,[Define to 1 if you have <sys/mman.h>])],
		[AC_MSG_WARN([no sys/mman.h])])

AC_CHECK_LIB([intl],[libintl_dngettext],[my_have_libintl=yes],[my_have_libintl=no])
AM_CONDITIONAL([HAVE_LIBINTL],[test "$my_have_libintl" = yes])

AM_CONDITIONAL([HOST_IS_64BIT],[case "$host_os" in *64*) true ;; *) false ;; esac ])

AM_CONDITIONAL([LINUX],[  case "$host_os" in  *linux*)  true  ;; *)  false ;;  esac])

AM_CONDITIONAL([WINDOWS],[ case "$host_os" in *mingw*) true ;; *cygwin*)  true ;;  *)  false  ;;  esac])

##########################################################################
# debug compilation support
##########################################################################

AC_MSG_CHECKING([whether to build with debug information])
AC_ARG_ENABLE([debug],
    [AS_HELP_STRING([--enable-debug],
        [enable debug data generation (def=no)])],
    [debugit="$enableval"],
    [debugit=no])
AC_MSG_RESULT([$debugit])

if test x"$debugit" = x"yes"; then
    AC_DEFINE([DEBUG],[],[Debug Mode])
#    AM_CXXFLAGS="$AM_CXXFLAGS -g -Wall -Werror -Wno-uninitialized -O0"
    AM_CXXFLAGS="$AM_CXXFLAGS -g -Wall -Wno-uninitialized -O0"
else
    AC_DEFINE([NDEBUG],[],[No-debug Mode])
    AM_CPPFLAGS="$AM_CPPFLAGS -DNDEBUG"
    AM_CXXFLAGS="$AM_CXXFLAGS -DNDEBUG -O3 -Wall"
fi

##########################################################################

AC_SUBST([AM_CXXFLAGS])


AC_OUTPUT([Makefile src/Makefile ])


                
